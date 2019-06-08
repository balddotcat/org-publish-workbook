;;; org-publish-workbook.el --- org export publish pipeline -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Elo Laszlo

;; Author: Elo Laszlo <hello at bald dot cat>
;; Created: December 2018
;; Package-Version: 0.2.0
;; Keywords: files
;; Homepage: [https://github.com/balddotcat/org-publish-workbook
;; Package-Requires: ((emacs "24"))

;; This file is not part of GNU Emacs

;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; org-export can easily generate multiple versions of the
;; same org data; an org-publish-workbook binds an export
;; backend with an org-publish project, to help with the
;; process of iteratively customizing output.
;;
;; A workbook consists of an entry in org-publish-workbook-
;; alist, a set of entries in org-publish-project-alist and
;; an org-export-derived-backend.

(require 'ox-publish)
(require 'seq)

(defvar org-publish-workbook-root-directory
  (with-current-buffer "*Messages*" default-directory)
  "Workbook ROOT directory.

Defaults to the Emacs server's initial working directory.")

(defvar org-publish-workbook-template-directory
  (expand-file-name ".etc/export-template" org-publish-workbook-root-directory)
  "Workbook template directory.

Defaults to '.etc/export-template', relative to root.")

(defvar org-publish-workbook-output-directory
  (expand-file-name ".var" org-publish-workbook-root-directory)
  "Workbook output path.

Defaults to '.var', relative to root.")

(defvar org-publish-workbook-insert-template-fn
  'org-publish-workbook-insert-setupfile
  "Called when either the workbook or it's project have
  :template property set.

Defaults to `org-publish-workbook-insert-setupfile'.
Function is called with template file path, as an
`org-export-before-processing-hook'.")

(defvar org-publish-workbook-alist '()
  "An alist that defines workbook behaviour.")

(defun org-publish-workbook-add (label backend &rest projects)
  "Initialize workbook.

Adds an entry to `org-publish-workbook-alist', based on the
provided first argument, LABEL.  The BACKEND can be either a
symbol or a list starting with the same symbol, the cdr acting as
an alist of options.  Optional PROJECTS are applied onto
`org-publish-workbook-add-project'.

:base-directory, :publishing-directory and :template-directory
are expanded relative to workbook root, unless absolute (starts
with '/')."
  (org-publish-workbook-remove label)
  (when (eq (type-of backend) 'symbol) (setq backend (cons backend nil)))
  (let* ((opts (cdr backend))
         (workbook (list :base-extension
                         (when opts (plist-get opts :base-extension))
                         :base-directory
                         (pcase (when opts (format "%s" (plist-get opts :base-directory)))
                           ((pred (equal 'nil)) org-publish-workbook-root-directory)
                           ((pred (string-prefix-p "/")) (plist-get opts :base-directory))
                           (path (expand-file-name path org-publish-workbook-root-directory)))
                         :exclude
                         (when opts (plist-get opts :exclude))
                         :publishing-directory
                         (pcase (when opts (format "%s" (plist-get opts :publishing-directory)))
                           ((pred (equal 'nil)) org-publish-workbook-output-directory)
                           ((pred (string-prefix-p "/")) (plist-get opts :publishing-directory))
                           (path (expand-file-name path org-publish-workbook-output-directory)))
                         :template-directory
                         (pcase (when opts (format "%s" (plist-get opts :template-directory)))
                           ((pred (equal 'nil)) org-publish-workbook-template-directory)
                           ((pred (string-prefix-p "/")) (plist-get opts :template-directory))
                           (path (expand-file-name path org-publish-workbook-template-directory)))
                         :template
                         (when opts (plist-get opts :template))))
         (workbook (and (apply 'org-publish-workbook-backend label backend)
                        (add-to-list 'org-publish-workbook-alist
                                     `(,label ,@workbook))
                        (assoc label org-publish-workbook-alist))))
    (when workbook
      (when projects
        (mapc (lambda (project)
                (org-publish-workbook-add-project (car workbook) project))
              projects))
      (car workbook))))

(defun org-publish-workbook-remove (&rest labels)
  "Remove workbook."
  (mapc (lambda (label)
          (when (assoc label org-publish-workbook-alist)
            (and (org-publish-workbook-remove-backend label)
                 (apply 'org-publish-workbook-remove-project (org-publish-workbook-projects label))
                 (setq org-publish-workbook-alist
                       (assq-delete-all label org-publish-workbook-alist)))))
        labels))

(defun org-publish-workbook-backend (label &rest options)
  "Return org-export backend by LABEL.

LABEL is a string.  With OPTIONS, create a derived backend; the
car is the `org-export' backend to derive from, as a symbol, the
cdr is properties to override.

Please see `org-export-define-derived-backend' for options."
  (when options
    (apply 'org-export-define-derived-backend label
           (car options)
           (list :filters-alist (plist-get (cdr options) :filters-alist)
                 :menu-entry (plist-get (cdr options) :menu-entry)
                 :options-alist (plist-get (cdr options) :options-alist)
                 :translate-alist (plist-get (cdr options) :translate-alist))))
  (org-export-get-backend label))

(defun org-publish-workbook-remove-backend (&rest labels)
  "Remove workbook backend by LABELS."
  (mapc (lambda (label)
          (let ((backend (org-export-get-backend label)))
            (when backend
              (setq org-export-registered-backends
                    (delete backend org-export-registered-backends)))))
        labels))

(defun org-publish-workbook-project (workbook &optional project)
  "Return WORKBOOK org-export publish PROJECT.

PROJECT properties default to those set in WORKBOOK, or the
equivalent environment variable, when available.

Please see `org-publish-project-alist'. The custom properties
:template and :decorate create export hooks through
`org-publish-workbook-decorate-project'.

:base-directory, :publishing-directory and :template-directory
are expanded relative to workbook root, unless absolute (starts
with '/')."
  (when project
    (let* ((label
            (format "%s-%s" workbook (car project)))
           (defaults
             (cdr (assoc workbook org-publish-workbook-alist)))
           (options
            (cdr project)))
      (pcase (when options (plist-get options :base-directory))
        ((pred (equal 'nil))
         (setq options (plist-put options :base-directory
                                  (or (plist-get defaults :base-directory)
                                      org-publish-workbook-root-directory))))
        (path
         (when (not (string-prefix-p "/" path))
           (setq options (plist-put options :base-directory
                                    (expand-file-name
                                     path (or (plist-get defaults :base-directory)
                                              org-publish-workbook-root-directory)))))))
      (when (not (plist-get options :exclude))
        (let ((exclude (plist-get defaults :exclude)))
          (when exclude
            (plist-put options :exclude exclude))))
      (pcase (when options (plist-get options :publishing-directory))
        ((pred (equal 'nil))
         (setq options (plist-put options :publishing-directory
                                  (or (plist-get defaults :publishing-directory)
                                      org-publish-workbook-output-directory))))
        (path
         (when (not (string-prefix-p "/" path))
           (setq options (plist-put options :publishing-directory
                                    (expand-file-name
                                     path (or (plist-get defaults :publishing-directory)
                                              org-publish-workbook-output-directory)))))))
      (when (not (plist-get options :publishing-function))
        (plist-put options :publishing-function
                   `((lambda (plist filename pub-dir)
                       (org-publish-org-to ',workbook filename
                                           ,(or (plist-get options :base-extension)
                                                (plist-get defaults :base-extension))
                                           plist pub-dir)))))
      (let* ((decorators (reverse (plist-get options :decorators))))
        (when (plist-get options :decorate)
          (setq decorators (push (plist-get options :decorate) decorators)))
        (when (or (plist-get options :template)
                  (plist-get defaults :template))
          (push `(:before-processing ,(identity org-publish-workbook-insert-template-fn)
                                     ,(org-publish-workbook-resolve-template-path workbook project))
                decorators))
        (seq-reduce (lambda (workbook-project decorator)
                      (when (not (string-prefix-p ":" (symbol-name (car decorator))))
                        (push :final-output decorator))
                      (apply 'org-publish-workbook-decorate-project workbook-project decorator))
                    decorators (cons label options))))))

(defun org-publish-workbook-add-project (workbook &optional project)
  (when project
    (let ((label (format "%s-%s" workbook (car project)))
          (workbook-project (org-publish-workbook-project workbook project)))
      (when workbook-project
        (org-publish-workbook-remove-project label)
        (add-to-list 'org-publish-project-alist workbook-project)))))

(defun org-publish-workbook-remove-project (&rest labels)
  (when labels
    (mapc (lambda (label)
            (when (and label (assoc label org-publish-project-alist))
              (setq org-publish-project-alist (assq-delete-all label org-publish-project-alist))))
          labels)))

(defun org-publish-workbook-projects (workbook)
  (let ((workbook (format "%s" workbook)))
    (mapcar 'car (seq-filter (lambda (project) (string-prefix-p workbook (car project) t))
                             org-publish-project-alist))))

(defun org-publish-workbook-decorate-project (project hook-type fn &rest args)
  "Add hook to edit content before or after export.

HOOK-TYPE is by default :final-output, can also be :before-processing.
PROJECT is an `org-publish-workbook-project'.

When editing :final-output, FN is called with the export action's
final output, and any ARGS.  :before-processing actions are
expanded within each current-buffer.

Hooks are activated and deactivated during the project's
:preparation-function and :completion-function, respectively."
  (let* ((project-properties (cdr project))
         (hook
          (pcase (identity hook-type)
            (:before-processing `(lambda (backend)
                                   (funcall ',fn ,@args)))
            (_ `(lambda (content backend info)
                  (apply ',fn content ',args)))))
         (add-hook
          (pcase (identity hook-type)
            (:before-processing `(lambda (project-properties)
                                   (add-hook 'org-export-before-processing-hook ,hook)))
            (_ `(lambda (project-properties)
                  (add-to-list 'org-export-filter-final-output-functions ,hook)))))
         (remove-hook
          (let ((hooks (pcase (identity hook-type)
                         (:before-processing 'org-export-before-processing-hook)
                         (_ 'org-export-filter-final-output-functions))))
            `(lambda (project-properties)
               (setq ,hooks (delete ,hook ,hooks))))))
    (plist-put project-properties :preparation-function
               (cons add-hook (plist-get project-properties :preparation-function)))
    (plist-put project-properties :completion-function
               (cons remove-hook (plist-get project-properties :completion-function)))
    (cons (car project) project-properties)))

(defun org-publish-workbook-insert-setupfile (template)
  "Update or insert org buffer property, with the value of TEMPLATE."
  (when (and template (file-exists-p template))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\\+SETUPFILE: " nil t)
          (delete-region (point) (progn (forward-line 1) (point)))
        (insert "#+SETUPFILE: "))
      (insert (format "%s\n" template)))))

(defun org-publish-workbook-resolve-template-path (workbook &optional project)
  "Return WORKBOOK PROJECT template path.

When not directly defined, a PROJECT's :template and
:template-directory properties default to it's WORKBOOK's,
or to the value of `org-publish-workbook-template-directory'.

Absolute paths are retained, otherwise paths are relative to
workbook :template-directory."
  (let* ((defaults (or (cdr (assoc workbook org-publish-workbook-alist))
                       (list)))
         (template (or (when project (plist-get (cdr project) :template))
                       (plist-get defaults :template)))
         (template (format "%s" (or template (car project)))))
    (if (and template (string-prefix-p "/" template)) template
      (let ((template-directory (or (plist-get defaults :template-directory)
                                    org-publish-workbook-template-directory)))
        (seq-some (lambda (path)
                    (let ((path (expand-file-name path template-directory)))
                      (and (file-exists-p path) path)))
                  (list (format "%s" template)
                        (format "%s.org" template)
                        (format "%s-%s.org" workbook template)
                        (format "%s.org" workbook)))))))

(defun org-publish-workbook-render (workbook &optional force async &rest projects)
  "Publish WORKBOOK projects.

FORCE and ASYNC are passed to `org-publish-project'.  When
PROJECTS are supplied, publish only these."
  (let ((projects (if projects
                      (mapcar (lambda (project)
                                (format "%s-%s" workbook project))
                              projects)
                    (org-publish-workbook-projects workbook))))
    (mapc (lambda (project)
            (org-publish-project project force async))
          projects)))

(provide 'org-publish-workbook)
;;; org-publish-workbook.el ends here
