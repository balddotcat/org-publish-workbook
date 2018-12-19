(load-file "org-publish-workbook.el")

(defmacro workbook-test (name &rest body)
  `(ert-deftest ,name ()
     (setq org-export-registered-backends
           (delete [cl-struct-org-export-backend test slimhtml nil nil nil nil nil]
                   org-export-registered-backends))
     (setq org-publish-project-alist nil)
     (should (eq nil (org-export-get-backend 'test)))
     (should (eq nil org-publish-project-alist))
     ,@body))

(workbook-test workbook-init
               (org-publish-workbook-init 'test '() '("test"))
               (should (equal 'test (org-publish-workbook-backend 'test)))
               (should (assoc "test-test" org-publish-project-alist))
               (let ((project-properties (cdr (assoc "test-test" org-publish-project-alist))))
                 (should (string= (format "%s/test" org-publish-workbook--path-root)
                                  (plist-get project-properties :base-directory)))))

(workbook-test project-properties
               (let ((project-properties (cdr (org-publish-workbook-project
                                               (org-publish-workbook-backend 'test '())))))
                 (should (string= (format "%s/test" org-publish-workbook--path-root)
                                  (plist-get project-properties :base-directory)))
                 (should (string= (format "%s/.var/test" org-publish-workbook--path-root)
                                  (plist-get project-properties :publishing-directory)))
                 (should (equal nil (plist-get project-properties :recursive)))
                 (should (equal nil (plist-get project-properties :preparation-function)))
                 (should (equal nil (plist-get project-properties :completion-function)))))

(workbook-test add-project
               (org-publish-workbook-backend 'test '())
               (let ((project-properties (cdr (assoc (org-publish-workbook-add-project
                                                      (org-publish-workbook-backend 'test) '("test"))
                                                     org-publish-project-alist))))
                 (should (string= (format "%s/test" org-publish-workbook--path-root)
                                  (plist-get project-properties :base-directory)))
                 (should (string= (format "%s/.var/test" org-publish-workbook--path-root)
                                  (plist-get project-properties :publishing-directory)))
                 (should (equal nil (plist-get project-properties :recursive)))
                 (should (equal nil (plist-get project-properties :preparation-function)))))


(workbook-test redefine-project
               (org-publish-workbook-backend 'test '())
               (org-publish-workbook-add-project (org-publish-workbook-backend 'test) '("test"))
               (org-publish-workbook-add-project (org-publish-workbook-backend 'test)
                                                 '("test"
                                                   :publishing-directory "hello"
                                                   :base-directory "hello"))
               (should (equal 1 (length org-publish-project-alist)))
               (let ((project (cdr (assoc "test-test" org-publish-project-alist))))
                 (should (string= (format "%s/.var/hello" org-publish-workbook--path-root)
                                  (plist-get project :publishing-directory)))
                 (should (string= (format "%s/hello" org-publish-workbook--path-root)
                                  (plist-get project :base-directory)))))

(workbook-test decorate
               (org-publish-workbook-init 'test '() '("test"))
               (let* ((project (assoc "test-test" org-publish-project-alist))
                      (project-properties (cdr project)))
                 (should (not (plist-get project-properties :preparation-function)))
                 (should (not (plist-get project-properties :completion-function)))
                 (let ((updated-project (org-publish-workbook-decorate :before-processing project 'identity)))
                   (should (equal (nth 3 (car (plist-get (cdr updated-project) :preparation-function)))
                                  '(add-hook (quote org-export-before-processing-hook)
                                             (function (lambda (backend)
                                                         (apply fn args))))))
                   (should (equal (nth 3 (car (plist-get (cdr updated-project) :completion-function)))
                                  '(remove-hook (quote org-export-before-processing-hook) fn))))
                 (let ((updated-project (org-publish-workbook-decorate :final-output project 'identity)))
                   (should (equal (nth 3 (car (plist-get (cdr updated-project) :preparation-function)))
                                  '(add-to-list (quote org-export-filter-final-output-functions)
                                                (function (lambda (content backend info)
                                                            (apply fn content info args))))))
                   (should (equal (nth 3 (car (plist-get (cdr updated-project) :completion-function)))
                                  '(setq org-export-filter-final-output-functions
                                         (seq-filter (function
                                                      (lambda (filter)
                                                        (and (not (eq filter fn)) filter)))
                                                     org-export-filter-final-output-functions)))))))
