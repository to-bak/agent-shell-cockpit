;;; agent-shell-cockpit-git-test.el --- Git and project tests -*- lexical-binding: t; -*-

(require 'agent-shell-cockpit-test-helper)

(ert-deftest agent-shell-cockpit-git-adds-and-removes-new-branch-worktree ()
  (agent-shell-cockpit-test-with-root
    (let* ((source (agent-shell-cockpit-test-make-repository
                    (expand-file-name "source" test-root)))
           (workspace (agent-shell-cockpit-workspace-create
                       :name "alpha" :title "Alpha"))
           (repository
            (agent-shell-cockpit-git-add-worktree
             :workspace workspace :source source :name "service"
             :mode 'new :branch "feature/alpha" :ref "HEAD"))
           (worktree (agent-shell-cockpit-workspace-repository-path
                      workspace repository)))
      (should (file-directory-p worktree))
      (should (equal (agent-shell-cockpit-test-git
                      worktree "branch" "--show-current")
                     "feature/alpha"))
      (should (member (expand-file-name "README.md" worktree)
                      (project-files (list 'agent-shell-cockpit
                                           (map-elt workspace 'root)))))
      (agent-shell-cockpit-git-remove-worktree workspace repository)
      (should-not (file-exists-p worktree))
      (should (member "feature/alpha"
                      (agent-shell-cockpit-git-branches source))))))

(ert-deftest agent-shell-cockpit-git-refuses-dirty-removal ()
  (agent-shell-cockpit-test-with-root
    (let* ((source (agent-shell-cockpit-test-make-repository
                    (expand-file-name "source" test-root)))
           (workspace (agent-shell-cockpit-workspace-create
                       :name "alpha" :title "Alpha"))
           (repository (agent-shell-cockpit-git-add-worktree
                        :workspace workspace :source source :name "service"
                        :mode 'detached :ref "HEAD"))
           (worktree (agent-shell-cockpit-workspace-repository-path
                      workspace repository)))
      (with-temp-file (expand-file-name "dirty.txt" worktree) (insert "dirty"))
      (should-error
       (agent-shell-cockpit-git-remove-worktree workspace repository)
       :type 'user-error)
      (should (file-directory-p worktree)))))

(ert-deftest agent-shell-cockpit-git-offers-refs-and-default-starting-point ()
  (agent-shell-cockpit-test-with-root
    (let* ((source (agent-shell-cockpit-test-make-repository
                    (expand-file-name "source" test-root)))
           (current (agent-shell-cockpit-test-git
                     source "branch" "--show-current"))
           (points (agent-shell-cockpit-git-starting-points source)))
      (agent-shell-cockpit-test-git source "tag" "v1")
      (setq points (agent-shell-cockpit-git-starting-points source))
      (should (member current points))
      (should (member "v1" points))
      (should (member "HEAD" points))
      (should (equal (agent-shell-cockpit-git-default-starting-point source)
                     current)))))

(ert-deftest agent-shell-cockpit-git-supports-existing-branch-and-rejects-duplicate ()
  (agent-shell-cockpit-test-with-root
    (let* ((source (agent-shell-cockpit-test-make-repository
                    (expand-file-name "source" test-root)))
           (workspace (agent-shell-cockpit-workspace-create
                       :name "alpha" :title "Alpha")))
      (agent-shell-cockpit-test-git source "branch" "existing")
      (let* ((repository (agent-shell-cockpit-git-add-worktree
                          :workspace workspace :source source :name "service"
                          :mode 'existing :branch "existing"))
             (worktree (agent-shell-cockpit-workspace-repository-path
                        workspace repository)))
        (should (equal (agent-shell-cockpit-test-git
                        worktree "branch" "--show-current")
                       "existing"))
        (should-error
         (agent-shell-cockpit-git-add-worktree
          :workspace workspace :source source :name "duplicate"
          :mode 'detached :ref "HEAD")
         :type 'user-error)))))

(ert-deftest agent-shell-cockpit-project-finds-workspace-from-worktree ()
  (agent-shell-cockpit-test-with-root
    (let* ((source (agent-shell-cockpit-test-make-repository
                    (expand-file-name "source" test-root)))
           (workspace (agent-shell-cockpit-workspace-create
                       :name "alpha" :title "Alpha"))
           (repository (agent-shell-cockpit-git-add-worktree
                        :workspace workspace :source source :name "service"
                        :mode 'detached :ref "HEAD"))
           (worktree (agent-shell-cockpit-workspace-repository-path
                      workspace repository)))
      (should (equal (agent-shell-cockpit-project-find worktree)
                     (list 'agent-shell-cockpit (map-elt workspace 'root)))))))

(ert-deftest agent-shell-cockpit-archive-removes-clean-worktrees ()
  (agent-shell-cockpit-test-with-root
    (let* ((source (agent-shell-cockpit-test-make-repository
                    (expand-file-name "source" test-root)))
           (workspace (agent-shell-cockpit-workspace-create
                       :name "alpha" :title "Alpha"))
           (_repository (agent-shell-cockpit-git-add-worktree
                         :workspace workspace :source source :name "service"
                         :mode 'new :branch "feature/archive" :ref "HEAD"))
           (archived (agent-shell-cockpit-workspace-archive workspace)))
      (should (equal (map-elt archived 'state) "archived"))
      (should (file-directory-p (map-elt archived 'root)))
      (should-not (file-exists-p
                   (expand-file-name "alpha"
                                     agent-shell-cockpit-workspace-directory)))
      (should (member "feature/archive"
                      (agent-shell-cockpit-git-branches source))))))

(ert-deftest agent-shell-cockpit-archive-preflight-preserves-dirty-worktree ()
  (agent-shell-cockpit-test-with-root
    (let* ((source (agent-shell-cockpit-test-make-repository
                    (expand-file-name "source" test-root)))
           (workspace (agent-shell-cockpit-workspace-create
                       :name "alpha" :title "Alpha"))
           (repository (agent-shell-cockpit-git-add-worktree
                        :workspace workspace :source source :name "service"
                        :mode 'detached :ref "HEAD"))
           (worktree (agent-shell-cockpit-workspace-repository-path
                      workspace repository)))
      (with-temp-file (expand-file-name "dirty.txt" worktree) (insert "dirty"))
      (should-error (agent-shell-cockpit-workspace-archive workspace)
                    :type 'user-error)
      (should (file-directory-p worktree))
      (should (equal (map-elt workspace 'state) "active")))))

(provide 'agent-shell-cockpit-git-test)

;;; agent-shell-cockpit-git-test.el ends here
