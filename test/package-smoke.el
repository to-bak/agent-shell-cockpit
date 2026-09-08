;;; package-smoke.el --- Install and exercise package autoloads -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'package)
(require 'lisp-mnt)

(let* ((root (make-temp-file "cockpit-package-" t))
       (user-emacs-directory root)
       (custom-file (expand-file-name "custom.el" root))
       (package-quickstart-file (expand-file-name "package-quickstart.el" root))
       (package-user-dir (expand-file-name "elpa" root))
       (package-check-signature nil)
       (source default-directory)
       (name "agent-shell-cockpit-0.2.0")
       (directory (expand-file-name name root))
       (tar (concat directory ".tar"))
       (package-alist nil)
       (dependency-paths (seq-filter
                          (lambda (path) (and (stringp path)
                                              (not (file-in-directory-p path source)))) load-path)))
  (unwind-protect
      (progn
        ;; Register the real dependencies already supplied by the test runner.
        ;; No package archives or user package configuration are consulted.
        (dolist (dependency '(agent-shell magit-section transient))
          (let* ((library (locate-library (symbol-name dependency)))
                 (file (concat (file-name-sans-extension library) ".el"))
                 (version (with-temp-buffer (insert-file-contents file)
					    (or (lm-header "package-version") (lm-header "version")))))
            (push (cons dependency
                        (list (package-desc-create :name dependency :version (version-to-list version)
                                                   :dir (file-name-directory library)))) package-alist)))
        (make-directory directory)
        (dolist (file (directory-files source t "\\`agent-shell-cockpit.*\\.el\\'"))
          (copy-file file (expand-file-name (file-name-nondirectory file) directory)))
        (with-temp-file (expand-file-name "agent-shell-cockpit-pkg.el" directory)
          (insert ";;; -*- lexical-binding: t; no-byte-compile: t; -*-\n(define-package \"agent-shell-cockpit\" \"0.2.0\" \"Workspace cockpit\"\n"
                  " '((emacs \"29.1\") (agent-shell \"0.75.1\") (magit-section \"4.0.0\") (transient \"0.7.0\")))\n"))
        (let ((default-directory root))
          (unless (zerop (call-process "tar" nil nil nil "-cf" tar name)) (error "Could not build package tar")))
        (package-install-file tar)
        (let* ((installed (expand-file-name name package-user-dir))
               (child (expand-file-name "test/package-smoke-child.el" source))
               (arguments (append '("-Q" "--batch")
                                  (apply #'append (mapcar (lambda (path) (list "-L" path)) dependency-paths))
                                  (list "-L" installed "-l" (expand-file-name "agent-shell-cockpit-autoloads.el" installed)
                                        "-l" child))))
          (with-temp-buffer
            (let ((status (apply #'call-process invocation-name nil t nil arguments)))
              (princ (buffer-string))
              (unless (zerop status) (error "Installed package smoke test failed"))))))
    (delete-directory root t)))

;;; package-smoke.el ends here
