;;; lint-dependencies.el --- Describe installed checker dependencies -*- lexical-binding: t; -*-
(require 'package)
(require 'lisp-mnt)
(defun agent-shell-cockpit-test-dependency-source-file (dependency)
  "Return the readable source file for installed DEPENDENCY."
  (let* ((library (or (locate-library (symbol-name dependency))
                      (error "Could not locate dependency: %s" dependency)))
         (source (concat (file-name-sans-extension library) ".el")))
    ;; Emacs 31 installs bundled transient source as transient.el.gz.
    (cond ((file-exists-p source) source)
          ((file-exists-p (concat source ".gz")) (concat source ".gz"))
          (t (error "Could not find source for dependency: %s" dependency)))))

(dolist (dependency '(agent-shell magit-section transient))
  (let* ((library (locate-library (symbol-name dependency)))
         (file (agent-shell-cockpit-test-dependency-source-file dependency))
         (version (with-temp-buffer (insert-file-contents file)
			    (or (lm-header "package-version") (lm-header "version")))))
    (push (cons dependency
                (list (package-desc-create :name dependency :version (version-to-list version)
                                           :dir (file-name-directory library)))) package-alist)))
(let (failed)
  (dolist (file command-line-args-left)
    (with-temp-buffer
      (insert-file-contents file t)
      (emacs-lisp-mode)
      (setq-local package-lint-main-file "agent-shell-cockpit.el")
      (dolist (diagnostic (package-lint-buffer))
        (setq failed t)
        (pcase-let ((`(,line ,column ,type ,message) diagnostic))
          (princ (format "%s:%s:%s: %s: %s\n" file line column type message))))))
  (kill-emacs (if failed 1 0)))
;;; lint-dependencies.el ends here
