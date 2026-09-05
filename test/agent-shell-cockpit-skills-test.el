;;; agent-shell-cockpit-skills-test.el --- Launch skill tests -*- lexical-binding: t; -*-

(require 'agent-shell-cockpit-test-helper)

(ert-deftest agent-shell-cockpit-skills-include-built-in-cockpit-skill ()
  (let ((agent-shell-cockpit-skills nil))
    (should (eq (caar (agent-shell-cockpit-skills-all)) 'cockpit))
    (should (string-match-p
             "repositories/"
             (agent-shell-cockpit-skills-render 'cockpit nil)))))

(ert-deftest agent-shell-cockpit-skills-register-and-compose-sources ()
  (agent-shell-cockpit-test-with-root
    (let* ((agent-shell-cockpit-skills nil)
           (workspace (agent-shell-cockpit-workspace-create :name "alpha"))
           (skill-file (expand-file-name "skill.txt" test-root)))
      (with-temp-file skill-file
        (insert "From a file.\n"))
      (agent-shell-cockpit-register-skill
       'inline :key "i" :title "Inline" :content "Inline text.")
      (agent-shell-cockpit-register-skill
       'file :key "f" :title "File" :file skill-file)
      (agent-shell-cockpit-register-skill
       'dynamic :key "d" :title "Dynamic"
       :function
       (lambda (current)
         (format "Workspace: %s" (map-elt current 'name))))
      (should
       (equal
        (agent-shell-cockpit-skills-compose
         '(inline file dynamic) workspace)
        "Inline text.\n\nFrom a file.\n\nWorkspace: alpha")))))

(ert-deftest agent-shell-cockpit-skills-require-exactly-one-source ()
  (let ((agent-shell-cockpit-skills nil))
    (should-error
     (agent-shell-cockpit-register-skill 'empty :title "Empty"))
    (should-error
     (agent-shell-cockpit-register-skill
      'ambiguous :title "Ambiguous" :content "Text" :file "skill.txt"))))

(ert-deftest agent-shell-cockpit-skills-launch-defaults-to-empty-selection ()
  (let ((agent-shell-cockpit-skills nil)
        (agent-shell-cockpit-default-skills nil))
    (cl-letf (((symbol-function 'transient-setup) #'ignore))
      (agent-shell-cockpit-skills-launch 'workspace)
      (should-not agent-shell-cockpit-skills--launch-selection))))

(ert-deftest agent-shell-cockpit-skills-require-menu-key ()
  (let ((agent-shell-cockpit-skills nil))
    (should-error
     (agent-shell-cockpit-register-skill
      'keyless :title "Keyless" :content "Text"))))

(ert-deftest agent-shell-cockpit-skills-reject-conflicting-menu-keys ()
  (let ((agent-shell-cockpit-skills nil))
    (should-error
     (agent-shell-cockpit-register-skill
      'reserved :key "c" :title "Reserved" :content "Text"))
    (agent-shell-cockpit-register-skill
     'first :key "x" :title "First" :content "One")
    (should-error
     (agent-shell-cockpit-register-skill
      'second :key "x" :title "Second" :content "Two"))))

(ert-deftest agent-shell-cockpit-skills-build-dynamic-transient-items ()
  (let ((agent-shell-cockpit-skills nil))
    (agent-shell-cockpit-register-skill
     'extra :key "x" :title "Extra" :content "Text")
    (let ((items (agent-shell-cockpit-skills--menu-children nil)))
      (should (equal (mapcar (lambda (item)
                               (plist-get (cdr item) :key))
                             items)
                     '("c" "x"))))))

(ert-deftest agent-shell-cockpit-session-start-injects-selected-skills ()
  (agent-shell-cockpit-test-with-root
    (let* ((workspace (agent-shell-cockpit-workspace-create :name "alpha"))
           (agent-shell-cockpit-test--buffers nil)
           buffer observed-input
           (command
            (lambda ()
              (interactive)
              (setq observed-input
                    (seq-some (lambda (source) (funcall source))
                              agent-shell-context-sources)
                    buffer (generate-new-buffer " *cockpit skilled agent*"))
              (with-current-buffer buffer
                (setq default-directory (map-elt workspace 'root)))
              (push buffer agent-shell-cockpit-test--buffers)
              buffer)))
      (unwind-protect
          (cl-letf (((symbol-function 'agent-shell-new-shell) command))
            (agent-shell-cockpit-session-start-select
             workspace "Selected skill.")
            (should (equal observed-input "Selected skill.")))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(provide 'agent-shell-cockpit-skills-test)

;;; agent-shell-cockpit-skills-test.el ends here
