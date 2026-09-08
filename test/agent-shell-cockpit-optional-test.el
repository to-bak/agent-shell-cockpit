;;; agent-shell-cockpit-optional-test.el --- Optional integration checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-cockpit)

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
            (should (eq (key-binding (kbd "S")) #'agent-shell-cockpit-start-agent))
            (should (eq (key-binding (kbd "s")) #'agent-shell-cockpit-start-agent)))
          (dolist (mode '(agent-shell-cockpit-mode agent-shell-cockpit-workspace-view-mode))
            (with-temp-buffer
              (funcall mode)
              (evil-local-mode 1)
              (evil-motion-state)
              (when (eq mode 'agent-shell-cockpit-workspace-view-mode)
                (should (eq (key-binding (kbd "S"))
                            #'agent-shell-cockpit-workspace-view-start-agent)))
              (should (eq (key-binding (kbd "I")) #'agent-shell-cockpit-visit-instruction)))))
      (evil-mode -1))))

(ert-deftest cockpit-optional-org-roam-is-explicit-and-reference-only ()
  (let ((agent-shell-cockpit-instruction-adapters
         (copy-tree agent-shell-cockpit-instruction-adapters))
        (agent-shell-cockpit-instructions
         '((manifest :title "Manifest" :source (org-roam "first-node"))
           (review :title "Review" :source (org-roam "second-node")))))
    (should-error (agent-shell-cockpit-instructions-render '(manifest)) :type 'user-error)
    (load "agent-shell-cockpit-org-roam" nil t)
    (require 'org-roam)
    (let ((org-roam-directory temporary-file-directory))
      (cl-letf (((symbol-function 'org-roam-node-from-id)
                 (lambda (_) (ert-fail "Rendering must not fetch node contents"))))
        (let ((text (agent-shell-cockpit-instructions-render '(manifest review))))
          (should (string-match-p "id:first-node" text))
          (should (string-match-p "id:second-node" text))
          (should (= 1 (length (split-string text "Org-roam directory:" t)))))))))

;;; agent-shell-cockpit-optional-test.el ends here

(ert-deftest cockpit-optional-evil-navigation-is-in-view-menus ()
  (require 'evil)
  (let ((agent-shell-cockpit-refresh-interval nil))
    (unwind-protect
        (progn
          (evil-mode 1)
          (dolist (mode '(agent-shell-cockpit-mode agent-shell-cockpit-workspace-view-mode
                          agent-shell-cockpit-archive-view-mode))
            (with-temp-buffer
              (funcall mode)
              (evil-local-mode 1)
              (evil-motion-state)
              (cl-letf (((symbol-function 'agent-shell-cockpit-workspace-view--workspace)
                         (lambda () nil)))
                (unwind-protect
                    (progn
                      (agent-shell-cockpit-dispatch)
                      (dolist (key '("j" "k"))
                        (should (eq (lookup-key transient--transient-map (kbd key))
                                    (if (equal key "j") #'agent-shell-cockpit-next
                                      #'agent-shell-cockpit-previous)))))
                  (transient-quit-all))))))
      (evil-mode -1))))

(ert-deftest cockpit-optional-org-roam-preview-uses-node-file-and-point ()
  (require 'org-roam)
  (let* ((agent-shell-cockpit-instruction-adapters nil)
         (file (make-temp-file "cockpit-roam-preview-" nil ".org" "* First\n* Node\nBody\n"))
         (node (org-roam-node-create :id "fixture" :file file :point 9))
         (agent-shell-cockpit-instructions '((a :title "Node" :source (org-roam "fixture")))))
    (load "agent-shell-cockpit-org-roam" nil t)
    (unwind-protect
        (cl-letf (((symbol-function 'org-roam-node-from-id) (lambda (_) node)))
          (with-temp-buffer
            (agent-shell-cockpit-instructions-preview 'a nil (current-buffer))
            (should (derived-mode-p 'org-mode))
            (should (= (point) 9))
            (should (looking-at "\\* Node"))))
      (delete-file file))))
