;;; agent-shell-cockpit-git.el --- Git worktrees for cockpit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak
;; Author: to-bak
;; Assisted-by: Codex:GPT-6

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Safe, shell-free Git worktree operations for cockpit workspaces.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-cockpit-store)
(require 'agent-shell-cockpit-workspace)

(defvar-local agent-shell-cockpit-git--status-cache nil
  "Per-view cache of worktree status checks.")

(defun agent-shell-cockpit-git--stop-status ()
  "Cancel status processes owned by the current view."
  (dolist (entry agent-shell-cockpit-git--status-cache)
    (when-let* ((process (plist-get (cdr entry) :process)))
      (set-process-sentinel process #'ignore)
      (when (process-live-p process) (delete-process process))))
  (setq agent-shell-cockpit-git--status-cache nil))

(defun agent-shell-cockpit-git-status (directory)
  "Return cached dirty, clean, pending or unknown status for DIRECTORY.
Refresh asynchronously at most every five seconds.  Output is discarded;
only the presence of staged, unstaged or untracked changes is retained."
  (unless agent-shell-cockpit-git--status-cache
    (add-hook 'kill-buffer-hook #'agent-shell-cockpit-git--stop-status nil t)
    (add-hook 'change-major-mode-hook #'agent-shell-cockpit-git--stop-status nil t))
  (let* ((entry (or (assoc directory agent-shell-cockpit-git--status-cache)
                    (let ((entry (list directory :state 'pending :time 0)))
                      (push entry agent-shell-cockpit-git--status-cache)
                      entry)))
         (data (cdr entry)))
    (when (and (not (plist-get data :process))
               (> (- (float-time) (plist-get data :time)) 5))
      (setcdr entry (plist-put data :time (float-time)))
      (let ((default-directory (file-name-as-directory directory))
            (owner (current-buffer))
            (dirty nil))
        (condition-case nil
            (setcdr
             entry
             (plist-put
              (cdr entry) :process
              (make-process
               :name "cockpit-worktree-status" :buffer nil :noquery t
               :connection-type 'pipe
               :command '("git" "--no-optional-locks" "status"
                          "--porcelain=v1" "-z" "--untracked-files=normal")
               :filter (lambda (_process output)
                         (unless (string-empty-p output) (setq dirty t)))
               :sentinel
               (lambda (process _event)
                 (when (and (memq (process-status process) '(exit signal))
                            (buffer-live-p owner))
                   (setcdr entry
                           (list :time (float-time)
                                 :state (if (and (eq (process-status process) 'exit)
                                                 (zerop (process-exit-status process)))
                                            (if dirty 'dirty 'clean)
                                          'unknown)))
                   (run-hooks 'agent-shell-cockpit-session-change-hook))))))
          (error (setcdr entry (list :state 'unknown :time (float-time)))))))
    (plist-get (cdr entry) :state)))

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
    directory "status" "--porcelain" "--untracked-files=all" "--ignored")))

(defun agent-shell-cockpit-git--worktree-paths (directory)
  "Return worktree paths registered for DIRECTORY's repository."
  (let (paths)
    (dolist (line (split-string
                   (agent-shell-cockpit-git--run
                    directory "worktree" "list" "--porcelain" "-z")
                   "\0" t))
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
     (agent-shell-cockpit-workspace-active-worktrees workspace))))

(defun agent-shell-cockpit-git--with-mutation (workspace function)
  "Call FUNCTION on fresh WORKSPACE metadata while holding its write lock."
  (let ((root (map-elt workspace 'root)))
    (when (file-remote-p root) (user-error "Remote worktree operations are unsupported"))
    (agent-shell-cockpit-store--with-lock
     (agent-shell-cockpit-store-metadata-path root)
     (lambda ()
       (let ((fresh (agent-shell-cockpit-store-read root)))
         (unless (and (equal (map-elt fresh 'state) "active") (not (map-elt fresh 'operation)))
           (user-error "Restore or recover the workspace before changing worktrees"))
         (prog1 (funcall function fresh)
           (let ((updated (agent-shell-cockpit-store-read root)))
             (setcar workspace (car updated)) (setcdr workspace (cdr updated)))))))))

(cl-defun agent-shell-cockpit-git-add-worktree (&key workspace source name mode ref branch)
  "Add a worktree to WORKSPACE from SOURCE, named NAME.
MODE is `new', `existing' or `detached'; REF and BRANCH select its checkout."
  (when (file-remote-p source) (user-error "Remote repository sources are unsupported"))
  (agent-shell-cockpit-git--with-mutation
   workspace (lambda (fresh)
               (agent-shell-cockpit-git--add-worktree
                :workspace fresh :source source :name name :mode mode :ref ref :branch branch))))

(defun agent-shell-cockpit-git-remove-worktree (workspace repository &optional keep-record)
  "Safely remove REPOSITORY in WORKSPACE; KEEP-RECORD preserves its manifest."
  (agent-shell-cockpit-git--with-mutation
   workspace (lambda (fresh)
               (agent-shell-cockpit-git--remove-worktree fresh repository keep-record))))

(defun agent-shell-cockpit-git-adopt (workspace repository)
  "Adopt the existing linked REPOSITORY in WORKSPACE."
  (agent-shell-cockpit-git--with-mutation
   workspace (lambda (fresh) (agent-shell-cockpit-git--adopt fresh repository))))

(cl-defun agent-shell-cockpit-git--add-worktree
    (&key workspace source name mode ref branch)
  "Add a worktree to WORKSPACE from SOURCE.
NAME is the worktree directory name.  MODE is one of `new', `existing', or
`detached'.  REF is the base or detached ref and BRANCH names a new or
existing branch as appropriate."
  (unless (agent-shell-cockpit-git-repository-p source)
    (user-error "Not a Git repository: %s" source))
  (when (file-in-directory-p (agent-shell-cockpit-git-common-directory source) (map-elt workspace 'root))
    (user-error "Use a source repository outside this workspace so archives remain restorable"))
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
           (agent-shell-cockpit-workspace-worktrees-path workspace))))
    (unless (and (file-in-directory-p destination (map-elt workspace 'root))
                 (not (file-symlink-p destination)))
      (user-error "Worktree destination escapes the workspace"))
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
    (let ((details (agent-shell-cockpit-git-record destination name)))
      (agent-shell-cockpit-store-set details 'owned "cockpit")
      (agent-shell-cockpit-store-set details 'base (map-elt details 'head))
      (let ((fresh (agent-shell-cockpit-store-update
                    (map-elt workspace 'root)
                    (lambda (current)
                      (agent-shell-cockpit-store-set current 'worktrees
                                                     (append (map-elt current 'worktrees) (list details)))))))
        (setcar workspace (car fresh))
        (setcdr workspace (cdr fresh)))
      details)))

(defun agent-shell-cockpit-git-record (path name)
  "Discover repository NAME's durable Git facts at PATH."
  `((name . ,name)
    (source . ,(agent-shell-cockpit-git-common-directory path))
    (head . ,(agent-shell-cockpit-git--run path "rev-parse" "HEAD"))
    (branch . ,(agent-shell-cockpit-git--run path "branch" "--show-current"))))

(defun agent-shell-cockpit-git-check-removal (workspace repository)
  "Check that REPOSITORY in WORKSPACE can be removed without losing files."
  (let ((path (agent-shell-cockpit-workspace-repository-path workspace repository)))
    (unless (equal (map-elt repository 'owned) "cockpit")
      (user-error "Adopt repository %s before removing it" (map-elt repository 'name)))
    (unless (and (file-directory-p path)
                 (not (file-symlink-p path))
                 (file-in-directory-p path (map-elt workspace 'root))
                 (file-regular-p (expand-file-name ".git" path)))
      (user-error "Not a removable workspace worktree: %s" path))
    (when (file-in-directory-p (agent-shell-cockpit-git-common-directory path) (map-elt workspace 'root))
      (user-error "Repository history is inside this workspace; move its source before archiving"))
    (unless (agent-shell-cockpit-git-clean-p path)
      (user-error "Worktree has changed, untracked or ignored files: %s" path))
    (when (file-exists-p
           (expand-file-name "locked"
                             (expand-file-name (agent-shell-cockpit-git--run path "rev-parse" "--git-dir") path)))
      (user-error "Worktree is locked: %s" path))
    (when (file-exists-p (expand-file-name ".gitmodules" path))
      (user-error "Remove submodule worktrees through Git before archiving: %s" path))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and buffer-file-name (buffer-modified-p)
                   (file-in-directory-p buffer-file-name path))
          (user-error "Save or discard modified buffer %s before removal" (buffer-name)))))
    (when (and (fboundp 'agent-shell-cockpit-session-buffers-in-directory)
               (agent-shell-cockpit-session-buffers-in-directory path))
      (user-error "Stop agents working in %s before removal" path))))

(declare-function agent-shell-cockpit-session-buffers-in-directory
                  "agent-shell-cockpit-session")

(defun agent-shell-cockpit-git-retain (workspace repository)
  "Retain REPOSITORY commits for WORKSPACE and return an updated record."
  (let* ((path (agent-shell-cockpit-workspace-repository-path workspace repository))
         (record (append (agent-shell-cockpit-git-record path (map-elt repository 'name))
                         (agent-shell-cockpit-store--fields repository '(base owned))))
         (ref (format "refs/cockpit/%s/%s/%s"
                      (map-elt workspace 'id) (map-elt repository 'name) (org-id-uuid))))
    (agent-shell-cockpit-git--run path "update-ref" ref (map-elt record 'head))
    (when (map-elt record 'base)
      (agent-shell-cockpit-git--run path "update-ref" (concat ref "-base")
                                    (map-elt record 'base)))
    (agent-shell-cockpit-store-set record 'retention ref)
    record))

(defun agent-shell-cockpit-git--remove-worktree (workspace repository &optional keep-record)
  "Remove clean REPOSITORY from WORKSPACE, retaining its Git history.
KEEP-RECORD leaves the repository manifest for archive recovery."
  (agent-shell-cockpit-git-check-removal workspace repository)
  (let* ((path (agent-shell-cockpit-workspace-repository-path workspace repository))
         (retained (agent-shell-cockpit-git-retain workspace repository)))
    (agent-shell-cockpit-git--run
     (map-elt retained 'source) "worktree" "remove" path)
    (unless keep-record
      (let ((fresh (agent-shell-cockpit-store-update
                    (map-elt workspace 'root)
                    (lambda (current)
                      (agent-shell-cockpit-store-set current 'worktrees
                                                     (seq-remove (lambda (item)
                                                                   (equal (map-elt item 'name) (map-elt repository 'name)))
                                                                 (map-elt current 'worktrees)))))))
        (setcar workspace (car fresh)) (setcdr workspace (cdr fresh))))
    retained))

(defun agent-shell-cockpit-git--adopt (workspace repository)
  "Explicitly adopt a discovered REPOSITORY into WORKSPACE."
  (let* ((path (agent-shell-cockpit-workspace-repository-path workspace repository))
         (record (agent-shell-cockpit-git-record path (map-elt repository 'name))))
    (unless (and (file-regular-p (expand-file-name ".git" path))
                 (not (file-symlink-p path))
                 (file-in-directory-p path (map-elt workspace 'root)))
      (user-error "Only contained Git worktrees can be adopted"))
    (agent-shell-cockpit-store-set record 'owned "cockpit")
    (agent-shell-cockpit-store-update
     (map-elt workspace 'root)
     (lambda (current)
       (agent-shell-cockpit-store-set current 'worktrees
                                      (cons record (seq-remove
                                                    (lambda (item) (equal (map-elt item 'name) (map-elt record 'name)))
                                                    (map-elt current 'worktrees))))))
    record))

(provide 'agent-shell-cockpit-git)

;;; agent-shell-cockpit-git.el ends here
