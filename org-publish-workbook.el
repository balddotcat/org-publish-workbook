;;; org-publish-workbook.el --- an org-publish pipeline utilizing ox-slimhtml -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Elo Laszlo

;; Author: Elo Laszlo <hello at bald dot cat>
;; Created: December 2018
;; Package-Version: 0.1.0
;; Keywords: files
;; Homepage: https://github.com/balddotcat/org-publish-workbook
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
;; WIP

(and (require 'package) (package-initialize))
(require 'ox-publish)
(require 'ox-slimhtml)

(defconst org-publish-workbook--path-root
  (expand-file-name "~/workbook")
  "Workbook path root.")

(defconst org-publish-workbook--path-template
  (expand-file-name ".etc/export-template" org-publish-workbook--path-root)
  "Workbook template path root.")

(defconst org-publish-workbook--path-output
  (expand-file-name ".var" org-publish-workbook--path-root)
  "Workbook output path root.")

(defun org-publish-workbook-init (workbook plist &rest projects)
  "Initialize WORKBOOK.

WORKBOOK is a string.  PLIST defines the `org-export' backend.
PROJECTS are added to `org-publish-project-alist'."
  (let ((backend (org-publish-workbook-backend workbook plist)))
    (when projects
      (mapc (lambda (project)
              (org-publish-workbook-add-project backend project))
            projects))))

(defun org-publish-workbook-backend (label &rest backend-properties)
  (when backend-properties
    (apply 'org-export-define-derived-backend label 'slimhtml backend-properties))
  (org-export-backend-name (org-export-get-backend label)))

(defun org-publish-workbook-publishing-function (workbook)
  "Return WORKBOOK publishing function."
  (list (lambda (plist filename pub-dir)
          (org-publish-org-to workbook filename ".html" plist pub-dir))))

(defun org-publish-workbook-project (workbook &optional project)
  (let* ((param (when project (cdr project)))
         (project-properties (list :publishing-function (org-publish-workbook-publishing-function workbook)))
         (template (org-publish-workbook-template-path workbook project)))
    (plist-put project-properties :base-directory
               (expand-file-name (or (when param (plist-get param :base-directory))
                                     (format "%s" workbook))
                                 org-publish-workbook--path-root))
    (plist-put project-properties :publishing-directory
               (expand-file-name (or (when param (plist-get param :publishing-directory))
                                     (format "%s" workbook))
                                 org-publish-workbook--path-output))
    (plist-put project-properties :recursive
               (when param (plist-get param :recursive)))
    (plist-put project-properties :preparation-function
               (when param (plist-get param :preparation-function)))
    (plist-put project-properties :completion-function
               (when param (plist-get param :completion-function)))
    (seq-reduce (lambda (project hook)
                  (apply 'org-publish-workbook-decorate (car hook) project (cdr hook)))
                (seq-filter 'identity (list
                                       (when template
                                         `(:before-processing org-publish-workbook--insert-template ,template))))
                (cons (if project
                          (format "%s-%s" workbook (car project))
                        (format "%s" workbook))
                      project-properties))))

(defun org-publish-workbook-add-project (workbook &optional project)
  (let* ((project-label (if project
                            (format "%s-%s" workbook (car project))
                          (format "%s" workbook)))
         (publishing-project (assoc project-label org-publish-project-alist)))
    (when publishing-project
      (setq org-publish-project-alist (assq-delete-all (car publishing-project) org-publish-project-alist)))
    (and (add-to-list 'org-publish-project-alist
                      (org-publish-workbook-project workbook project)
                      nil
                      #'(lambda (new-project old-project) (string= (car new-project) (car old-project))))
         project-label)))

(defun org-publish-workbook-decorate (hook-type project fn &rest args)
  (let* ((project-properties (cdr project))
         (add-hook (pcase (identity hook-type)
                     (:before-processing 'org-publish-workbook-decorate-before-processing)
                     (_ 'org-publish-workbook-decorate-final-output)))
         (remove-hook (pcase (identity hook-type)
                        (:before-processing 'org-publish-workbook-decorate-remove-before-processing-hook)
                        (_ 'org-publish-workbook-decorate-remove-final-output-hook)))
         (hook (apply add-hook fn args)))
    (plist-put project-properties :preparation-function
               (cons hook
                     (plist-get project-properties :preparation-function)))
    (plist-put project-properties :completion-function
               (cons (funcall remove-hook hook)
                     (plist-get project-properties :preparation-function)))
    (cons (car project) project-properties)))

(defun org-publish-workbook-decorate-final-output (fn &rest args)
  (lambda (project-properties)
    (add-to-list 'org-export-filter-final-output-functions
                 (lambda (content backend info)
                   (apply fn content info args)))))

(defun org-publish-workbook-decorate-remove-final-output-hook (fn)
  (lambda (project-properties)
    (setq org-export-filter-final-output-functions
          (seq-filter (lambda (filter)
                        (and (not (eq filter fn)) filter))
                      org-export-filter-final-output-functions))))

(defun org-publish-workbook-decorate-before-processing (fn &rest args)
  (lambda (project-properties)
    (add-hook 'org-export-before-processing-hook
              (lambda (backend)
                (apply fn args)))))

(defun org-publish-workbook-decorate-remove-before-processing-hook (fn)
  (lambda (project-properties)
    (remove-hook 'org-export-before-processing-hook fn)))

(defun org-publish-workbook-template-path (workbook &optional project)
  (let* ((template (when project (plist-get (cdr project) :template)))
         (template (when template (or (and (stringp template) template)
                                      (car project)))))
    (when template
      (seq-some (lambda (path)
                  (let ((path (expand-file-name path org-publish-workbook--path-template)))
                    (and (file-exists-p path) path)))
                (list (format "%s" template)
                      (format "%s.org" template)
                      (format "%s-%s.org" workbook template)
                      (format "%s.org" workbook))))))

(defun org-publish-workbook--insert-template (template)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#\\+SETUPFILE: " nil t)
        (delete-region (point) (progn (forward-line 1) (point)))
      (insert "#+SETUPFILE: "))
    (insert template "\n")))

(provide 'org-publish-workbook)
;;; org-publish-workbook.el ends here
