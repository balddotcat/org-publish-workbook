;;; org-publish-workbook -*- lexical-binding: t; -*-
;; Copyright (C) 2018 Elo Laszlo

(and (require 'package) (package-initialize))
(require 'ox-publish)

(require 'ox-slimhtml (expand-file-name "~/workbook/.lib/slimhtml/ox-slimhtml.el"))
(require 'posthtml (expand-file-name "~/workbook/.lib/posthtml/posthtml.el"))

(setq debug-on-error nil)

(setq org-html-htmlize-output-type nil)
(setq org-publish-list-skipped-files nil)

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
              ;; (apply 'org-publish-workbook-add-project backend project))
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
                  (apply 'org-publish-workbook-add-hook project (car hook) (cdr hook)))
                (seq-filter 'identity (list
                                       (when template
                                         `(org-publish-workbook-hook--insert-template ,template))))
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

(defun org-publish-workbook-add-hook (project fn &rest args)
  (let ((project-properties (cdr project)))
    (plist-put project-properties :preparation-function
               (cons (apply 'org-publish-workbook-hook fn args)
                     (plist-get project-properties :preparation-function)))
    (plist-put project-properties :completion-function
               (cons (apply 'org-publish-workbook-hook fn args)
                     (plist-get project-properties :completion-function)))
    (cons (car project) project-properties)))

(defun org-publish-workbook-hook (fn &rest args)
  (lambda (project-properties)
    (add-hook 'org-export-before-processing-hook
              (lambda (backend) (apply fn args)))))

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

(defun org-publish-workbook-hook--insert-template (template)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#\\+SETUPFILE: " nil t)
        (delete-region (point) (progn (forward-line 1) (point)))
      (insert "#+SETUPFILE: "))
    (insert template "\n")))

(provide 'org-publish-workbook)
;;; org-publish-workbook.el ends here
