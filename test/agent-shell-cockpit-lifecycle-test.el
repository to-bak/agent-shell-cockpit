;;; agent-shell-cockpit-lifecycle-test.el --- Recovery regressions -*- lexical-binding: t; -*-

(require 'agent-shell-cockpit-test-helper)

(defmacro cockpit-test-with-worktree (&rest body)
  "Evaluate BODY with a workspace and a real detached worktree."
  (declare (indent 0))
  `(agent-shell-cockpit-test-with-root
    (let* ((source (agent-shell-cockpit-test-make-repository (expand-file-name "source" test-root)))
           (workspace (agent-shell-cockpit-workspace-create :name "alpha"))
           (repository (agent-shell-cockpit-git-add-worktree
                        :workspace workspace :source source :name "service"
                        :mode 'detached :ref "HEAD"))
           (worktree (agent-shell-cockpit-workspace-repository-path workspace repository)))
      ,@body)))

(ert-deftest cockpit-archive-retains-detached-commits-through-gc-and-restore ()
  (cockpit-test-with-worktree
   (with-temp-file (expand-file-name "detached.txt" worktree) (insert "Keep this"))
   (agent-shell-cockpit-test-git worktree "add" ".")
   (agent-shell-cockpit-test-git worktree "commit" "-qm" "detached work")
   (let* ((head (agent-shell-cockpit-test-git worktree "rev-parse" "HEAD"))
          (archive (agent-shell-cockpit-workspace-archive workspace))
          (record (car (map-elt archive 'worktrees))))
     (should-not (file-exists-p worktree))
     (should (equal head (map-elt record 'head)))
     (agent-shell-cockpit-test-git source "reflog" "expire" "--expire=now" "--all")
     (agent-shell-cockpit-test-git source "gc" "--prune=now")
     (should (equal head (agent-shell-cockpit-test-git source "rev-parse" (map-elt record 'retention))))
     (let ((restored (agent-shell-cockpit-workspace-restore archive)))
       (should (equal (map-elt restored 'state) "active"))
       (should (equal head (agent-shell-cockpit-test-git worktree "rev-parse" "HEAD")))
       (should (file-exists-p (expand-file-name "detached.txt" worktree)))))))

(ert-deftest cockpit-archive-refuses-ignored-files ()
  (cockpit-test-with-worktree
   (let ((exclude (expand-file-name "info/exclude" (agent-shell-cockpit-git-common-directory worktree))))
     (with-temp-file exclude (insert "local.secret\n")))
   (with-temp-file (expand-file-name "local.secret" worktree) (insert "Keep"))
   (should-error (agent-shell-cockpit-workspace-archive workspace) :type 'user-error)
   (should (file-exists-p (expand-file-name "local.secret" worktree)))
   (should-not (map-elt (agent-shell-cockpit-store-read (map-elt workspace 'root)) 'operation))))

(ert-deftest cockpit-unassigned-agent-blocks-removal ()
  (cockpit-test-with-worktree
   (let ((buffer (generate-new-buffer " *test agent*")))
     (unwind-protect
         (let ((agent-shell-cockpit-test--buffers (list buffer)))
           (with-current-buffer buffer (setq default-directory worktree))
           (should-error (agent-shell-cockpit-git-remove-worktree workspace repository) :type 'user-error)
           (should-error (agent-shell-cockpit-workspace-archive workspace) :type 'user-error))
       (kill-buffer buffer)))))

(ert-deftest cockpit-removal-refuses-unadopted-and-locked-worktrees ()
  (cockpit-test-with-worktree
   (let ((unowned (copy-tree repository)))
     (agent-shell-cockpit-store-set unowned 'owned nil)
     (should-error (agent-shell-cockpit-git-check-removal workspace unowned) :type 'user-error))
   (agent-shell-cockpit-test-git source "worktree" "lock" worktree)
   (should-error (agent-shell-cockpit-git-check-removal workspace repository) :type 'user-error)))

(ert-deftest cockpit-archive-recovers-removal-before-journal-write ()
  (cockpit-test-with-worktree
   (let ((original (symbol-function 'agent-shell-cockpit-git--run)) crashed)
     (cl-letf (((symbol-function 'agent-shell-cockpit-git--run)
                (lambda (directory &rest args)
                  (prog1 (apply original directory args)
                    (when (and (not crashed) (equal (seq-take args 2) '("worktree" "remove")))
                      (setq crashed t) (error "Simulated crash after removal"))))))
       (should-error (agent-shell-cockpit-workspace-archive workspace)))
     (should-not (file-exists-p worktree))
     (let ((archive (agent-shell-cockpit-workspace-archive workspace)))
       (should (equal (map-elt archive 'state) "archived"))
       (should-not (map-elt archive 'operation))
       (agent-shell-cockpit-workspace-restore archive)
       (should (file-directory-p worktree))))))

(ert-deftest cockpit-restore-recovers-after-one-of-two-worktrees ()
  (cockpit-test-with-worktree
   (agent-shell-cockpit-git-add-worktree
    :workspace workspace :source (agent-shell-cockpit-test-make-repository
                                  (expand-file-name "other-source" test-root))
    :name "second" :mode 'detached :ref "HEAD")
   (let ((archive (agent-shell-cockpit-workspace-archive workspace))
         (original (symbol-function 'agent-shell-cockpit-git--run)) (count 0))
     (cl-letf (((symbol-function 'agent-shell-cockpit-git--run)
                (lambda (directory &rest args)
                  (when (equal (seq-take args 2) '("worktree" "add"))
                    (cl-incf count)
                    (when (= count 2) (error "Second creation failed")))
                  (apply original directory args))))
       (should-error (agent-shell-cockpit-workspace-restore archive)))
     (let* ((partial (agent-shell-cockpit-store-read (map-elt workspace 'root)))
            (restored (agent-shell-cockpit-workspace-restore partial)))
       (should-not (map-elt restored 'operation))
       (dolist (entry (map-elt restored 'worktrees))
         (should (file-directory-p (agent-shell-cockpit-workspace-repository-path restored entry))))))))

(ert-deftest cockpit-preserved-files-restore-without-overwrite ()
  (cockpit-test-with-worktree
   (let ((storage (expand-file-name ".agent-shell-cockpit/preserved/service/local.txt" (map-elt workspace 'root))))
     (make-directory (file-name-directory storage) t)
     (with-temp-file storage (insert "Preserved"))
     (with-temp-file (expand-file-name "local.txt" worktree) (insert "Conflict"))
     (should-error (agent-shell-cockpit-lifecycle-restore-files workspace repository) :type 'user-error)
     (should (file-exists-p storage))
     (delete-file (expand-file-name "local.txt" worktree))
     (let* ((archive (agent-shell-cockpit-workspace-archive workspace))
            (restored (agent-shell-cockpit-workspace-restore archive)))
       (should-not (map-elt restored 'operation))
       (should (equal (with-temp-buffer (insert-file-contents (expand-file-name "local.txt" worktree)) (buffer-string)) "Preserved"))))))

(ert-deftest cockpit-restore-refuses-occupied-destination ()
  (cockpit-test-with-worktree
   (let ((archive (agent-shell-cockpit-workspace-archive workspace)))
     (agent-shell-cockpit-workspace-create :name "alpha")
     (should-error (agent-shell-cockpit-workspace-restore archive) :type 'user-error)
     (should (file-directory-p (map-elt archive 'root)))
     (should (equal (map-elt (agent-shell-cockpit-workspace-restore archive "beta") 'name) "beta")))))

(provide 'agent-shell-cockpit-lifecycle-test)
;;; agent-shell-cockpit-lifecycle-test.el ends here
