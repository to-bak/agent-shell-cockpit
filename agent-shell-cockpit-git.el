;;; agent-shell-cockpit-git.el --- Git worktrees for cockpit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Safe, shell-free Git worktree operations for cockpit workspaces.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-cockpit-store)
(require 'agent-shell-cockpit-workspace)

(defun agent-shell-cockpit-git--run (directory &rest arguments)
  "Run Git with ARGUMENTS in DIRECTORY and return trimmed output."
  (unless (executable-find "git")
    (user-error "Git executable not found"))
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory
                              (expand-file-name directory))))
      (unless (zerop (apply #'process-file "git" nil t nil arguments))
        (error "Git failed: %s" (string-trim (buffer-string))))
      (string-trim-right (buffer-string)))))

(defun agent-shell-cockpit-git-repository-p (directory)
  "Return non-nil when DIRECTORY belongs to a Git repository."
  (condition-case nil
      (progn
        (agent-shell-cockpit-git--run directory "rev-parse" "--git-dir")
        t)
    (error nil)))

(defun agent-shell-cockpit-git-common-directory (directory)
  "Return the canonical Git common directory for DIRECTORY."
  (file-truename
   (expand-file-name
    (agent-shell-cockpit-git--run directory "rev-parse" "--git-common-dir")
    directory)))

(defun agent-shell-cockpit-git-branches (directory)
  "Return local branch names available from DIRECTORY."
  (split-string
   (agent-shell-cockpit-git--run
    directory "for-each-ref" "--format=%(refname:short)" "refs/heads/")
   "\n" t))

(defun agent-shell-cockpit-git-starting-points (directory)
  "Return branch, remote, and tag names usable as start points in DIRECTORY."
  (delete-dups
   (append
    (agent-shell-cockpit-git-branches directory)
    (split-string
     (agent-shell-cockpit-git--run
      directory "for-each-ref" "--format=%(refname:short)"
      "refs/remotes/" "refs/tags/")
     "\n" t)
    '("HEAD"))))

(defun agent-shell-cockpit-git-default-starting-point (directory)
  "Return a sensible default start point for a new worktree in DIRECTORY."
  (let* ((branches (agent-shell-cockpit-git-branches directory))
         (remote-head
          (condition-case nil
              (agent-shell-cockpit-git--run
               directory "symbolic-ref" "--short" "refs/remotes/origin/HEAD")
            (error nil)))
         (remote-branch
          (and remote-head
               (string-remove-prefix "origin/" remote-head)))
         (current
          (agent-shell-cockpit-git--run directory "branch" "--show-current")))
    (or (and remote-branch (member remote-branch branches) remote-branch)
        (seq-find (lambda (candidate) (member candidate branches))
                  '("main" "master"))
        (and (not (string-empty-p current)) current)
        remote-head
        "HEAD")))

(defun agent-shell-cockpit-git-clean-p (directory)
  "Return non-nil when DIRECTORY is clean, including untracked files."
  (string-empty-p
   (agent-shell-cockpit-git--run
    directory "status" "--porcelain" "--untracked-files=all")))

(defun agent-shell-cockpit-git-files (directory)
  "Return absolute tracked and unignored untracked files in DIRECTORY."
  (mapcar (lambda (path) (expand-file-name path directory))
          (split-string
           (agent-shell-cockpit-git--run
            directory "ls-files" "-co" "--exclude-standard")
           "\n" t)))

(defun agent-shell-cockpit-git-description (directory)
  "Return the current branch or abbreviated commit for DIRECTORY."
  (condition-case nil
      (let ((branch (agent-shell-cockpit-git--run
                     directory "branch" "--show-current")))
        (if (string-empty-p branch)
            (concat "detached at "
                    (agent-shell-cockpit-git--run
                     directory "rev-parse" "--short" "HEAD"))
          branch))
    (error "not a Git worktree")))

(defun agent-shell-cockpit-git--worktree-paths (directory)
  "Return worktree paths registered for DIRECTORY's repository."
  (let (paths)
    (dolist (line (split-string
                   (agent-shell-cockpit-git--run
                    directory "worktree" "list" "--porcelain")
                   "\n" t))
      (when (string-prefix-p "worktree " line)
        (push (file-name-as-directory
               (file-truename (string-remove-prefix "worktree " line)))
              paths)))
    (nreverse paths)))

(defun agent-shell-cockpit-git--source-already-attached-p (workspace source)
  "Return non-nil when SOURCE is already attached to WORKSPACE."
  (let ((common (agent-shell-cockpit-git-common-directory source)))
    (seq-some
     (lambda (repository)
       (condition-case nil
           (equal common
                  (agent-shell-cockpit-git-common-directory
                   (agent-shell-cockpit-workspace-repository-path
                    workspace repository)))
         (error nil)))
     (agent-shell-cockpit-workspace-active-repositories workspace))))

(cl-defun agent-shell-cockpit-git-add-worktree
    (&key workspace source name mode ref branch)
  "Add a worktree to WORKSPACE from SOURCE.
NAME is the worktree directory name.  MODE is one of `new', `existing', or
`detached'.  REF is the base or detached ref and BRANCH names a new or
existing branch as appropriate."
  (unless (agent-shell-cockpit-git-repository-p source)
    (user-error "Not a Git repository: %s" source))
  (when (agent-shell-cockpit-git--source-already-attached-p workspace source)
    (user-error "Repository is already attached to this workspace"))
  (unless (and (stringp name)
               (string-match-p "\\`[[:alnum:]][[:alnum:]_.-]*\\'" name))
    (user-error "Invalid worktree name: %s" name))
  (unless (memq mode '(new existing detached))
    (user-error "Unsupported checkout mode: %S" mode))
  (let* ((source (file-name-as-directory (file-truename source)))
         (destination
          (expand-file-name
           name
           (expand-file-name agent-shell-cockpit-repositories-directory-name
                             (map-elt workspace 'root))))
         (record (list (cons 'name name))))
    (when (file-exists-p destination)
      (user-error "Worktree destination already exists: %s" destination))
    (pcase mode
      ('new
       (when (string-empty-p (or branch ""))
         (user-error "A new worktree requires a branch name"))
       (agent-shell-cockpit-git--run
        source "worktree" "add" "-b" branch destination (or ref "HEAD")))
      ('existing
       (when (string-empty-p (or branch ""))
         (user-error "Select an existing branch"))
       (agent-shell-cockpit-git--run
        source "worktree" "add" destination branch))
      ('detached
       (agent-shell-cockpit-git--run
        source "worktree" "add" "--detach" destination (or ref "HEAD"))))
    record))

(defun agent-shell-cockpit-git-remove-worktree (workspace repository)
  "Remove clean REPOSITORY worktree from WORKSPACE without deleting its branch."
  (let ((path (agent-shell-cockpit-workspace-repository-path
               workspace repository)))
    (unless (file-directory-p path)
      (user-error "Worktree is missing: %s" path))
    (unless (agent-shell-cockpit-git-clean-p path)
      (user-error "Worktree has tracked or untracked changes: %s" path))
    (let* ((target (file-name-as-directory (file-truename path)))
           (source (seq-find (lambda (candidate)
                               (not (equal candidate target)))
                             (agent-shell-cockpit-git--worktree-paths path))))
      (unless source
        (user-error "No other worktree can remove %s" path))
      (agent-shell-cockpit-git--run source "worktree" "remove" path))
    repository))

(provide 'agent-shell-cockpit-git)

;;; agent-shell-cockpit-git.el ends here
