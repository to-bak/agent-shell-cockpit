;;; agent-shell-cockpit-test-helper.el --- Test support for cockpit -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Keep most tests independent of agent-shell's full dependency graph.
(defvar agent-shell--state nil)
(defvar agent-shell-cockpit-test--buffers nil)
(defvar agent-shell-agent-configs nil)
(defun agent-shell-buffers () agent-shell-cockpit-test--buffers)
(defun agent-shell-status (&rest _) 'ready)
(defun agent-shell-new-shell () (interactive) nil)
(defun agent-shell-start (&rest _) nil)
(defun agent-shell-subscribe-to (&rest _) 1)
(defun agent-shell-unsubscribe (&rest _) nil)
(defun agent-shell--resolved-agent-configs () agent-shell-agent-configs)
(provide 'agent-shell)

(require 'agent-shell-cockpit)

(defmacro agent-shell-cockpit-test-with-root (&rest body)
  "Evaluate BODY with an isolated cockpit workspace root."
  (declare (indent 0) (debug body))
  `(let* ((test-root (make-temp-file "cockpit-test-" t))
          (agent-shell-cockpit-workspace-directory
           (file-name-as-directory (expand-file-name "workspaces" test-root)))
          (agent-shell-cockpit-archive-directory nil))
     (unwind-protect
         (progn ,@body)
       (delete-directory test-root t))))

(defun agent-shell-cockpit-test-git (directory &rest arguments)
  "Run Git ARGUMENTS in DIRECTORY and return output."
  (with-temp-buffer
    (let ((default-directory directory))
      (unless (zerop (apply #'process-file "git" nil t nil arguments))
        (error "%s" (buffer-string)))
      (string-trim (buffer-string)))))

(defun agent-shell-cockpit-test-make-repository (directory)
  "Create a committed Git repository at DIRECTORY."
  (make-directory directory t)
  (agent-shell-cockpit-test-git directory "init" "-q")
  (agent-shell-cockpit-test-git directory "config" "user.email" "test@example.com")
  (agent-shell-cockpit-test-git directory "config" "user.name" "Cockpit Test")
  (with-temp-file (expand-file-name "README.md" directory) (insert "test\n"))
  (agent-shell-cockpit-test-git directory "add" "README.md")
  (agent-shell-cockpit-test-git directory "commit" "-qm" "initial")
  directory)

(provide 'agent-shell-cockpit-test-helper)

;;; agent-shell-cockpit-test-helper.el ends here
