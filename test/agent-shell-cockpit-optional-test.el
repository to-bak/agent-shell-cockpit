;;; agent-shell-cockpit-optional-test.el --- Optional integration checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-cockpit)

(ert-deftest cockpit-optional-consult-preserves-order-and-cleans-preview ()
  (require 'consult)
  (require 'agent-shell-cockpit-consult)
  (let ((agent-shell-cockpit-default-instructions nil)
        (agent-shell-cockpit-instructions
         '((a :title "First" :source (literal "FIRST"))
           (b :title "Second" :source (literal "SECOND")))))
    (dolist (cancel '(nil t))
      (let ((choices '("b — Second" "a — First" "Remove last" "a — First" "Done"))
            (original (window-buffer))
            previews preview-buffer)
        (cl-letf (((symbol-function 'consult--read)
                   (lambda (_table &rest options)
                     (let ((choice (pop choices)))
                       (funcall (plist-get options :state) 'preview choice)
                       (setq preview-buffer (window-buffer))
                       (push (with-current-buffer preview-buffer (buffer-string)) previews)
                       (when cancel (signal 'quit nil))
                       choice))))
          (if cancel
              (should (eq (condition-case nil
                              (agent-shell-cockpit-consult-read)
                            (quit 'cancelled)) 'cancelled))
            (should (equal (agent-shell-cockpit-consult-read) '(b a)))
            (should (equal (car previews) "SECOND\n\nFIRST")))
          (should (eq (window-buffer) original))
          (should-not (buffer-live-p preview-buffer)))))))

(ert-deftest cockpit-optional-evil-loaded-after-cockpit ()
  (require 'evil)
  (let ((agent-shell-cockpit-refresh-interval nil))
    (unwind-protect
        (progn
          (evil-mode 1)
          (with-temp-buffer
            (agent-shell-cockpit-mode)
            (evil-local-mode 1)
            (evil-motion-state)
            (should (eq (key-binding (kbd "S")) #'agent-shell-cockpit-start-agent-defaults))
            (should (eq (key-binding (kbd "N")) #'agent-shell-cockpit-start-standalone))
            (should (eq (key-binding (kbd "s")) #'agent-shell-cockpit-start-agent)))
          (dolist (mode '(agent-shell-cockpit-mode agent-shell-cockpit-workspace-view-mode))
            (with-temp-buffer
              (funcall mode)
              (evil-local-mode 1)
              (evil-motion-state)
              (when (eq mode 'agent-shell-cockpit-workspace-view-mode)
                (should (eq (key-binding (kbd "S"))
                            #'agent-shell-cockpit-workspace-view-start-agent-defaults)))
              (should (eq (key-binding (kbd "I")) #'agent-shell-cockpit-visit-instruction)))))
      (evil-mode -1))))

(ert-deftest cockpit-optional-org-roam-is-explicit-and-reference-only ()
  (let ((agent-shell-cockpit-instruction-adapters
         (copy-tree agent-shell-cockpit-instruction-adapters))
        (agent-shell-cockpit-instructions
         '((manifest :title "Manifest" :source (org-roam "first-node"))
           (review :title "Review" :source (org-roam "second-node")))))
    (should-error (agent-shell-cockpit-instructions-render '(manifest)) :type 'user-error)
    (require 'agent-shell-cockpit-org-roam)
    (require 'org-roam)
    (let ((org-roam-directory temporary-file-directory))
      (cl-letf (((symbol-function 'org-roam-node-from-id)
                 (lambda (_) (ert-fail "Rendering must not fetch node contents"))))
        (let ((text (agent-shell-cockpit-instructions-render '(manifest review))))
          (should (string-match-p "id:first-node" text))
          (should (string-match-p "id:second-node" text))
          (should (= 1 (length (split-string text "Org-roam directory:" t)))))))))

;;; agent-shell-cockpit-optional-test.el ends here
