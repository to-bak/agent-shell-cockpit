;;; agent-shell-cockpit-instructions-test.el --- Instruction tests -*- lexical-binding: t; -*-

(require 'agent-shell-cockpit-test-helper)

(ert-deftest cockpit-instructions-custom-adapter-bootstraps-once ()
  (let ((agent-shell-cockpit-instruction-adapters nil)
        (agent-shell-cockpit-instructions
         '((a :title "A" :source (example "one"))
           (brief :title "Brief" :source (literal "Be brief."))
           (b :title "B" :source (example "two")))))
    (agent-shell-cockpit-register-instruction-adapter
     'example :reference (lambda (source _) (concat "example:" (cadr source)))
     :bootstrap "Example bootstrap.")
    (let ((text (agent-shell-cockpit-instructions-render '(a brief b a))))
      (should (equal text (concat "Example bootstrap.\n\nRead and follow A:\nexample:one"
                                 "\n\nBe brief.\n\nRead and follow B:\nexample:two"))))
    (agent-shell-cockpit-unregister-instruction-adapter 'example)
    (should-error (agent-shell-cockpit-instructions-render '(a)) :type 'user-error)))

(ert-deftest cockpit-instructions-file-is-reference-never-content ()
  (let* ((file (make-temp-file "cockpit-reference-" nil ".md"))
         (agent-shell-cockpit-instructions
          `((review :title "Review" :source (file ,file)))))
    (unwind-protect
        (progn
          (with-temp-file file (insert "SOURCE CONTENT MUST NOT BE INJECTED"))
          (let ((text (agent-shell-cockpit-instructions-render '(review))))
            (should (string-match-p (regexp-quote file) text))
            (should-not (string-match-p "SOURCE CONTENT" text))
            (should-not (get-file-buffer file))))
      (delete-file file))))

(ert-deftest cockpit-instructions-reject-duplicate-and-unknown-identifiers ()
  (let ((agent-shell-cockpit-instructions
         '((a :title "A" :source (literal "A"))
           (a :title "Other" :source (literal "Other")))))
    (should-error (agent-shell-cockpit-instructions-render '(a)) :type 'user-error))
  (let ((agent-shell-cockpit-instructions nil))
    (should-error (agent-shell-cockpit-instructions-render '(missing)) :type 'user-error)))

(ert-deftest cockpit-instructions-render-before-inserting-or-launching ()
  (let ((agent-shell-cockpit-instructions
         '((a :title "A" :source (literal "Good"))
           (b :title "B" :source (unloaded "node")))) launched)
    (cl-letf (((symbol-function 'agent-shell-cockpit-instructions-read)
               (lambda (&rest _) '(a b)))
              ((symbol-function 'agent-shell-cockpit-session-start-select)
               (lambda (&rest _) (setq launched t))))
      (with-temp-buffer
        (insert "Existing prompt")
        (should-error (agent-shell-cockpit-insert-instruction) :type 'user-error)
        (should (equal (buffer-string) "Existing prompt")))
      (should-error (agent-shell-cockpit-instructions-launch) :type 'user-error)
      (should-not launched))))

(ert-deftest cockpit-instructions-picker-uses-stable-ids-and-defaults ()
  (let ((agent-shell-cockpit-instructions
         '((a :title "Same title" :source (literal "A"))
           (b :title "Same title" :source (literal "B"))))
        (agent-shell-cockpit-default-instructions '(b)))
    (cl-letf (((symbol-function 'completing-read-multiple)
               (lambda (_ table _predicate _match initial &rest _)
                 (should (equal initial "b"))
                 (should (equal (mapcar #'car table) '("a" "b")))
                 '("b" "a"))))
      (should (equal (agent-shell-cockpit-instructions-read) '(b a))))))

(ert-deftest cockpit-instructions-org-id-reference-and-visit ()
  (require 'org-id)
  (let* ((file (make-temp-file "cockpit-org-id-" nil ".org"))
         (agent-shell-cockpit-instructions '((node :title "Node" :source (org-id "abc"))))
         visited)
    (unwind-protect
        (cl-letf (((symbol-function 'org-id-find-id-file) (lambda (_) file))
                  ((symbol-function 'org-id-goto) (lambda (id) (setq visited id)))
                  ((symbol-function 'completing-read) (lambda (&rest _) "node")))
          (should (string-match-p "id:abc" (agent-shell-cockpit-instructions-render '(node))))
          (agent-shell-cockpit-visit-instruction)
          (should (equal visited "abc")))
      (delete-file file))))

(ert-deftest cockpit-instructions-no-global-org-roam-load ()
  (should-not (featurep 'agent-shell-cockpit-org-roam))
  (should-not (assq 'org-roam agent-shell-cockpit-instruction-adapters)))

(ert-deftest cockpit-worktree-row-is-flat-and-does-not-run-synchronous-git ()
  (agent-shell-cockpit-test-with-root
   (let* ((workspace (agent-shell-cockpit-workspace-create :name "alpha"))
          (path (expand-file-name "service" (agent-shell-cockpit-workspace-worktrees-path workspace))))
     (make-directory path)
     (cl-letf (((symbol-function 'agent-shell-cockpit-git--run)
                (lambda (&rest _) (ert-fail "Rendering must not run Git"))))
       (with-temp-buffer
         (agent-shell-cockpit-workspace-view-mode)
         (setq agent-shell-cockpit-workspace-view--root (map-elt workspace 'root))
         (agent-shell-cockpit-workspace-view-refresh)
         (goto-char (point-min))
         (search-forward "service")
         (let ((section (magit-current-section)))
           (should-not (oref section content))
           (should (equal (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)) "service …"))))))))

(ert-deftest cockpit-instructions-selected-text-reaches-native-launch ()
  (agent-shell-cockpit-test-with-root
   (let* ((workspace (agent-shell-cockpit-workspace-create :name "alpha"))
          (agent-shell-cockpit-instructions '((brief :title "Brief" :source (literal "Be brief."))))
          observed)
     (cl-letf (((symbol-function 'agent-shell-cockpit-instructions-read) (lambda (&rest _) '(brief)))
               ((symbol-function 'agent-shell-cockpit-session-start-select)
                (lambda (target text) (should (equal target workspace)) (setq observed text))))
       (agent-shell-cockpit-instructions-launch workspace)
       (should (equal observed "Be brief."))))))

(provide 'agent-shell-cockpit-instructions-test)
;;; agent-shell-cockpit-instructions-test.el ends here
