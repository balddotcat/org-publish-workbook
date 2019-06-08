(and (require 'package) (package-initialize))
(load-file "org-publish-workbook.el")

(defmacro workbook-test (name &rest body)
  `(ert-deftest ,name ()
     (setq org-export-registered-backends
           (delete [cl-struct-org-export-backend test html nil nil nil nil nil]
                   org-export-registered-backends))
     (setq org-publish-project-alist nil)
     (setq org-publish-workbook-alist nil)
     ,@body))


(workbook-test workbook-init
               (org-publish-workbook-add 'test 'html '("project"))
               (should (assoc 'test org-publish-workbook-alist))
               (should (seq-filter (lambda (backend)
                                     (equal backend [cl-struct-org-export-backend test html nil nil nil nil nil]))
                                   org-export-registered-backends))
               (should (assoc "test-project" org-publish-project-alist)))


(workbook-test add-project
               (org-publish-workbook-add 'test 'html)
               (org-publish-workbook-add-project
                (org-export-backend-name (org-publish-workbook-backend 'test))
                '("project"))
               (should (assoc "test-project" org-publish-project-alist)))


(workbook-test project-properties
               (org-publish-workbook-add 'test 'html '("project"))
               (let ((properties (cdr (assoc "test-project" org-publish-project-alist))))
                 (should (string= (plist-get properties :base-directory)
                                  (format "%s" org-publish-workbook-root-directory)))
                 (should (string= (plist-get properties :publishing-directory)
                                  (format "%s.var" org-publish-workbook-root-directory)))
                 (should (equal nil (plist-get properties :recursive)))
                 (should (equal nil (plist-get properties :preparation-function)))
                 (should (equal nil (plist-get properties :completion-function)))))


(workbook-test workbook-remove
               (org-publish-workbook-add 'test 'html '("project"))
               (org-publish-workbook-remove 'test)
               (should (eq nil (org-export-get-backend 'test)))
               (should (eq nil (assoc 'test org-publish-workbook-alist)))
               (should (eq nil (assoc "test-project" org-publish-project-alist))))

(workbook-test project-template-property
               (let* ((project (org-publish-workbook-project 'test '("project" :template PATH)))
                      (project-properties (cdr project))
                      (preparation-function (plist-get project-properties :preparation-function))
                      (completion-function (plist-get project-properties :completion-function)))
                 (should (equal (cadr (cdar (cddr (car preparation-function))))
                                '(lambda (backend) (funcall (quote org-publish-workbook-insert-setupfile) nil))))
                 (should (equal (cadr (cadr (cdar (cddr (car completion-function)))))
                                '(lambda (backend) (funcall (quote org-publish-workbook-insert-setupfile) nil))))))

(workbook-test project-decorate-property
               (let* ((project (org-publish-workbook-project 'test '("project" :decorate '(identity))))
                      (project-properties (cdr project))
                      (preparation-function (plist-get project-properties :preparation-function))
                      (completion-function (plist-get project-properties :completion-function)))
                   (should (equal (cadr (cdar (cddr (car preparation-function))))
                                  '(lambda (content backend info) (apply (quote quote) content (quote ((identity)))))))
                   (should (equal (cadr (cadr (cdar (cddr (car completion-function)))))
                                  '(lambda (content backend info) (apply (quote quote) content (quote ((identity)))))))))

(workbook-test before-processing-hooks
               (org-publish-workbook-add 'test 'html '("project"))
               (let* ((project (assoc "test-project" org-publish-project-alist))
                      (project-properties (cdr project)))
                 (should (not (plist-get project-properties :preparation-function)))
                 (should (not (plist-get project-properties :completion-function)))
                 (let* ((updated-project (org-publish-workbook-decorate-project project :before-processing 'identity))
                        (preparation-function (plist-get (cdr updated-project) :preparation-function))
                        (completion-function (plist-get (cdr updated-project) :completion-function)))
                   (should (equal (cadr (cdar (cddr (car preparation-function))))
                                  '(lambda (backend) (funcall (quote identity)))))
                   (should (equal (cadr (cadr (cdar (cddr (car completion-function)))))
                                  '(lambda (backend) (funcall (quote identity))))))))

(workbook-test final-output-hooks
               (org-publish-workbook-add 'test 'html '("project"))
               (let* ((project (assoc "test-project" org-publish-project-alist))
                      (project-properties (cdr project)))
                 (should (not (plist-get project-properties :preparation-function)))
                 (should (not (plist-get project-properties :completion-function)))
                 (let* ((updated-project (org-publish-workbook-decorate-project project :final-output 'identity))
                        (preparation-function (plist-get (cdr updated-project) :preparation-function))
                        (completion-function (plist-get (cdr updated-project) :completion-function)))
                   (should (equal (cadr (cdar (cddr (car preparation-function))))
                                  '(lambda (content backend info) (apply (quote identity) content (quote nil)))))
                   (should (equal (cadr (cadr (cdar (cddr (car completion-function)))))
                                  '(lambda (content backend info) (apply (quote identity) content (quote nil))))))))
