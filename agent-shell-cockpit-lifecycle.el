;;; agent-shell-cockpit-lifecycle.el --- Recoverable worktree lifecycle -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak
;; Author: to-bak
;; Assisted-by: Codex:GPT-6
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Retain Git history before removal and journal archive/restore progress.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'org-id)
(require 'seq)
(require 'agent-shell-cockpit-store)
(require 'agent-shell-cockpit-workspace)
(require 'agent-shell-cockpit-git)
(require 'agent-shell-cockpit-session)

(defvar agent-shell-cockpit-lifecycle--running nil)

(defun agent-shell-cockpit-lifecycle--contained (root path)
  "Require PATH to stay inside local ROOT without symlink components."
  (when (or (file-remote-p root) (file-symlink-p (directory-file-name root))
            (not (file-in-directory-p path root)))
    (user-error "Lifecycle path escapes workspace: %s" path))
  (let ((parent (directory-file-name (expand-file-name path)))
        (base (directory-file-name (expand-file-name root))))
    (while (not (equal parent base))
      (when (file-symlink-p parent)
        (user-error "Symlinks are unsupported in lifecycle paths: %s" parent))
      (let ((next (directory-file-name (file-name-directory parent))))
        (when (equal next parent) (user-error "Path is outside workspace"))
        (setq parent next)))))

(defun agent-shell-cockpit-lifecycle--paths (workspace)
  "Validate all mutable paths in WORKSPACE before a lifecycle operation."
  (let ((root (map-elt workspace 'root)))
    (agent-shell-cockpit-lifecycle--contained
     root (agent-shell-cockpit-workspace-worktrees-path workspace))
    (agent-shell-cockpit-lifecycle--contained
     root (expand-file-name ".agent-shell-cockpit/preserved" root))
    (dolist (repository (map-elt workspace 'worktrees))
      (agent-shell-cockpit-lifecycle--contained
       root (agent-shell-cockpit-workspace-repository-path workspace repository))
      (agent-shell-cockpit-lifecycle--contained
       root (expand-file-name (concat ".agent-shell-cockpit/preserved/"
                                      (map-elt repository 'name)) root)))))

(defun agent-shell-cockpit-lifecycle--check-buffers (path)
  "Refuse to move PATH while it contains unsaved visiting buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and buffer-file-name (buffer-modified-p)
                 (or (equal buffer-file-name path)
                     (file-in-directory-p buffer-file-name path)))
        (user-error "Save or discard modified buffer %s first" (buffer-name))))))

(defun agent-shell-cockpit-lifecycle--rename (source destination)
  "Move SOURCE to DESTINATION and retarget affected Emacs buffers."
  (agent-shell-cockpit-lifecycle--check-buffers source)
  ;; Capture names before moving, while canonical path comparisons still work.
  (let ((directories (file-directory-p source)) moves)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (let ((file (and buffer-file-name
                         (or (equal buffer-file-name source)
                             (and directories (file-in-directory-p buffer-file-name source)))
                         (if directories (expand-file-name (file-relative-name buffer-file-name source) destination)
                           destination)))
              (directory (and directories (file-in-directory-p default-directory source)
                              (expand-file-name (file-relative-name default-directory source) destination))))
          (when (or file directory) (push (list buffer file directory) moves)))))
    (rename-file source destination)
    (dolist (move moves)
      (pcase-let ((`(,buffer ,file ,directory) move))
        (with-current-buffer buffer
          (when file (set-visited-file-name file t) (set-buffer-modified-p nil))
          (when directory (setq default-directory (file-name-as-directory directory))))))
    (run-hook-with-args 'agent-shell-cockpit-workspace-move-hook source destination)))

(defun agent-shell-cockpit-workspace--archive-preflight (workspace &optional force)
  "Validate WORKSPACE for archiving, allowing local file loss with FORCE."
  (unless (equal (map-elt workspace 'state) "active")
    (user-error "Workspace is not active"))
  (when (agent-shell-cockpit-session-buffers-in-directory (map-elt workspace 'root))
    (user-error "Stop all agents working inside the workspace before archiving"))
  (agent-shell-cockpit-lifecycle--paths workspace)
  (agent-shell-cockpit-lifecycle--check-buffers (map-elt workspace 'root))
  (dolist (repository (agent-shell-cockpit-workspace-active-worktrees workspace))
    (unless (and (equal (map-elt repository 'removed) "yes")
                 (not (file-exists-p (agent-shell-cockpit-workspace-repository-path workspace repository))))
      (let ((path (agent-shell-cockpit-workspace-repository-path workspace repository)))
        (if (and (map-elt repository 'retention) (not (file-exists-p path))
                 (equal (map-nested-elt workspace '(operation type)) "archive"))
            (unless (equal (map-elt repository 'head)
                           (agent-shell-cockpit-git--run (map-elt repository 'source)
                                                         "rev-parse" "--verify" (map-elt repository 'retention)))
              (user-error "Retained commit is unavailable; inspect archive manifest"))
          (agent-shell-cockpit-git-check-removal workspace repository force))))))

(defun agent-shell-cockpit-workspace-archive-confirm (workspace)
  "Confirm archive of WORKSPACE, including all destructive consequences."
  (setq workspace (agent-shell-cockpit-store-read (map-elt workspace 'root)))
  (unless (and (equal (map-elt workspace 'state) "archived")
               (equal (map-nested-elt workspace '(operation type)) "archive"))
    (agent-shell-cockpit-workspace--archive-preflight workspace t))
  (let (warnings)
    (dolist (repository (agent-shell-cockpit-workspace-active-worktrees workspace))
      (let ((path (agent-shell-cockpit-workspace-repository-path workspace repository)))
        (when (file-exists-p path)
          (dolist (warning (agent-shell-cockpit-git-removal-warnings path))
            (push (format "%s: %s" (map-elt repository 'name) warning) warnings)))))
    (when (yes-or-no-p
           (concat (format "Archive %s and remove its worktrees? Commits will be retained."
                           (map-elt workspace 'title))
                   (when warnings (concat "\n" (string-join (nreverse warnings) "\n")))
                   "\nProceed? "))
      (agent-shell-cockpit-workspace-archive workspace (and warnings t)))))

(defun agent-shell-cockpit-lifecycle--claim (workspace type destination)
  "Claim WORKSPACE's lifecycle operation TYPE with DESTINATION."
  (agent-shell-cockpit-store-update
   (map-elt workspace 'root)
   (lambda (fresh)
     (let ((operation (map-elt fresh 'operation)))
       (when operation
         (unless (equal (map-elt operation 'type) type)
           (user-error "Finish the existing %s operation first" (map-elt operation 'type)))
         (when (or (not (equal (map-elt operation 'host) (system-name)))
                   (and (not (equal (map-elt operation 'pid) (emacs-pid)))
                        (process-attributes (map-elt operation 'pid))))
           (user-error "Another Emacs owns this lifecycle operation")))
       ;; A new archive needs a new manifest.  Historical refs remain in Git.
       (unless operation
         (when (equal type "archive")
           (dolist (repository (map-elt fresh 'worktrees))
             (when (file-exists-p (agent-shell-cockpit-workspace-repository-path fresh repository))
               (agent-shell-cockpit-store-set repository 'retention nil)
               (agent-shell-cockpit-store-set repository 'removed nil)))))
       (agent-shell-cockpit-store-set fresh 'operation
                                      `((type . ,type) (destination . ,destination)
                                        (pid . ,(emacs-pid)) (host . ,(system-name))))))
   t))

(defun agent-shell-cockpit-lifecycle--same-device (source destination)
  "Require SOURCE and DESTINATION to be on the same filesystem."
  (unless (equal (file-attribute-device-number (file-attributes source))
                 (file-attribute-device-number (file-attributes destination)))
    (user-error "Cross-filesystem lifecycle operations are unsupported; choose a local archive")))

(defun agent-shell-cockpit-workspace-archive (workspace &optional force)
  "Archive WORKSPACE, retaining commits and recording recoverable progress.
FORCE permits deleting local files and locked worktrees after caller consent."
  (when agent-shell-cockpit-lifecycle--running (user-error "Lifecycle operation already running"))
  (let* ((agent-shell-cockpit-lifecycle--running t)
         (root (map-elt workspace 'root))
         (workspace (agent-shell-cockpit-store-read root))
         (archive-root (agent-shell-cockpit-store-archive-directory))
         (destination (or (map-nested-elt workspace '(operation destination))
                          (expand-file-name
                           (format "%s--%s" (map-elt workspace 'name) (org-id-uuid)) archive-root))))
    (if (and (equal (map-elt workspace 'state) "archived")
             (equal (map-nested-elt workspace '(operation type)) "archive"))
        (progn
          (agent-shell-cockpit-lifecycle--claim workspace "archive" destination)
          (agent-shell-cockpit-store-update
           root (lambda (fresh)
                  (agent-shell-cockpit-store-set fresh 'operation nil)
                  (agent-shell-cockpit-store-set fresh 'archivedAt (floor (float-time)))) t))
      (agent-shell-cockpit-workspace--archive-preflight workspace force)
      (make-directory archive-root t)
      (unless (and (file-writable-p archive-root)
                   (file-equal-p (file-name-directory (directory-file-name destination)) archive-root)
                   (not (file-exists-p destination)))
        (user-error "Archive destination is unavailable: %s" destination))
      (agent-shell-cockpit-lifecycle--same-device root archive-root)
      (setq workspace (agent-shell-cockpit-lifecycle--claim workspace "archive" destination))
      ;; Write every retention ref and the complete manifest before any removal.
      (let ((records
             (mapcar (lambda (repository)
                       (if (or (map-elt repository 'retention)
                               (equal (map-elt repository 'removed) "yes"))
                           repository
                         (agent-shell-cockpit-git-retain workspace repository)))
                     (agent-shell-cockpit-workspace-active-worktrees workspace))))
        (setq workspace
              (agent-shell-cockpit-store-update
               root (lambda (fresh)
                      (agent-shell-cockpit-store-set fresh 'worktrees records)) t)))
      (dolist (repository (map-elt workspace 'worktrees))
        (unless (equal (map-elt repository 'removed) "yes")
          (let ((path (agent-shell-cockpit-workspace-repository-path workspace repository)))
            ;; A crash can happen between successful Git removal and journaling it.
            (when (file-exists-p path)
              (agent-shell-cockpit-git-check-removal workspace repository force)
              (unless (equal (map-elt repository 'head)
                             (agent-shell-cockpit-git--run path "rev-parse" "HEAD"))
                (user-error "Repository HEAD changed during archive; inspect before retrying"))
              (apply #'agent-shell-cockpit-git--run (map-elt repository 'source)
                     "worktree" "remove"
                     (append (when force '("--force" "--force")) (list path))))
            (setq workspace
                  (agent-shell-cockpit-store-update
                   root (lambda (fresh)
                          (let ((entry (seq-find
                                        (lambda (item) (equal (map-elt item 'name) (map-elt repository 'name)))
                                        (map-elt fresh 'worktrees))))
                            (agent-shell-cockpit-store-set entry 'removed "yes"))) t)))))
      (agent-shell-cockpit-lifecycle--rename root destination)
      (agent-shell-cockpit-store-update
       destination
       (lambda (fresh)
         (agent-shell-cockpit-store-set fresh 'operation nil)
         (agent-shell-cockpit-store-set fresh 'archivedAt (floor (float-time)))) t))))

(defun agent-shell-cockpit-lifecycle-restore-files (workspace repository)
  "Restore preserved files for REPOSITORY in WORKSPACE, without overwriting."
  (let* ((root (map-elt workspace 'root))
         (path (agent-shell-cockpit-workspace-repository-path workspace repository))
         (storage (expand-file-name (concat ".agent-shell-cockpit/preserved/" (map-elt repository 'name)) root)))
    (agent-shell-cockpit-lifecycle--paths workspace)
    (when (file-directory-p storage)
      (unless (file-in-directory-p storage root) (user-error "Preservation storage escapes workspace"))
      (dolist (source (directory-files-recursively storage "."))
        (let ((destination (expand-file-name (file-relative-name source storage) path)))
          (unless (and (file-regular-p source) (not (file-symlink-p source))
                       (file-in-directory-p source storage) (file-in-directory-p destination path)
                       (not (file-exists-p destination)) (not (file-symlink-p destination)))
            (user-error "Preserved file conflicts with %s; inspect before retrying" destination))
          (make-directory (file-name-directory destination) t)
          (agent-shell-cockpit-lifecycle--rename source destination))))))

(defun agent-shell-cockpit-workspace-restore (workspace &optional name)
  "Restore archived WORKSPACE under NAME, defaulting to its logical name.
Reuse unchanged branches, otherwise recover detached at the saved commit.
Unavailable repositories remain recorded for a later recovery attempt."
  (when agent-shell-cockpit-lifecycle--running (user-error "Lifecycle operation already running"))
  (let* ((agent-shell-cockpit-lifecycle--running t)
         (workspace (agent-shell-cockpit-store-read (map-elt workspace 'root)))
         (name (or name (map-elt workspace 'name)))
         (root (map-elt workspace 'root))
         (destination (or (map-nested-elt workspace '(operation destination))
                          (expand-file-name (agent-shell-cockpit-workspace--validate-name name)
                                            agent-shell-cockpit-workspace-directory))))
    (unless (or (equal (map-elt workspace 'state) "archived")
                (seq-some (lambda (entry) (map-elt entry 'restoreError))
                          (map-elt workspace 'worktrees))
                (equal (map-nested-elt workspace '(operation type)) "restore"))
      (user-error "Workspace is not archived"))
    (agent-shell-cockpit-lifecycle--paths workspace)
    (agent-shell-cockpit-lifecycle--check-buffers root)
    (make-directory agent-shell-cockpit-workspace-directory t)
    (unless (file-equal-p (file-name-directory (directory-file-name destination))
                          agent-shell-cockpit-workspace-directory)
      (user-error "Restore destination must be a direct workspace child"))
    (unless (equal (file-name-as-directory destination) (file-name-as-directory root))
      (when (or (file-exists-p destination) (file-symlink-p destination)) (user-error "Workspace destination exists: %s" destination))
      (agent-shell-cockpit-lifecycle--same-device root agent-shell-cockpit-workspace-directory))
    (setq workspace (agent-shell-cockpit-lifecycle--claim workspace "restore" destination))
    (unless (equal (file-name-as-directory destination) (file-name-as-directory root))
      (agent-shell-cockpit-lifecycle--rename root destination))
    (setq root destination workspace (agent-shell-cockpit-store-read destination))
    (agent-shell-cockpit-lifecycle--paths workspace)
    (dolist (repository (map-elt workspace 'worktrees))
      (unless (and (equal (map-elt workspace 'state) "active")
                   (not (map-elt repository 'removed))
                   (not (map-elt repository 'restoreError)))
        (condition-case err
            (progn
              (agent-shell-cockpit-lifecycle--restore-worktree workspace repository)
              (agent-shell-cockpit-lifecycle-restore-files workspace repository)
              (agent-shell-cockpit-store-set repository 'removed nil)
              (agent-shell-cockpit-store-set repository 'restoreError nil))
          (error
           (agent-shell-cockpit-store-set repository 'restoreError (error-message-string err))
           (message "Could not restore %s: %s" (map-elt repository 'name)
                    (error-message-string err))))))
    (agent-shell-cockpit-store-update
     root (lambda (fresh)
            (agent-shell-cockpit-store-set fresh 'worktrees (map-elt workspace 'worktrees))
            (agent-shell-cockpit-store-set fresh 'operation nil)
            (agent-shell-cockpit-store-set fresh 'archivedAt nil)) t)))

(defun agent-shell-cockpit-lifecycle--restore-worktree (workspace repository)
  "Recover REPOSITORY in WORKSPACE without moving a changed branch."
  (let ((path (agent-shell-cockpit-workspace-repository-path workspace repository))
        (source (map-elt repository 'source))
        (head (map-elt repository 'head))
        (branch (map-elt repository 'branch)))
    (unless (and source head (file-directory-p source))
      (user-error "Repository source unavailable: %s" source))
    (agent-shell-cockpit-git--run source "cat-file" "-e" (concat head "^{commit}"))
    (if (file-exists-p path)
        (unless (and (file-regular-p (expand-file-name ".git" path))
                     (equal (agent-shell-cockpit-git-common-directory path) source)
                     (equal (agent-shell-cockpit-git--run path "rev-parse" "HEAD") head))
          (user-error "Restore found a conflicting path: %s" path))
      (unless
          (and branch (not (string-empty-p branch))
               (condition-case nil
                   (and (equal head (agent-shell-cockpit-git--run
                                     source "rev-parse" "--verify" (concat "refs/heads/" branch)))
                        (progn (agent-shell-cockpit-git--run source "worktree" "add" path branch) t))
                 (error nil)))
        (agent-shell-cockpit-git--run source "worktree" "add" "--detach" path head)))))

(defun agent-shell-cockpit-lifecycle-preserve (workspace repository files)
  "Move selected local FILES from REPOSITORY into WORKSPACE storage."
  (agent-shell-cockpit-git--with-mutation
   workspace
   (lambda (fresh)
     (agent-shell-cockpit-lifecycle--paths fresh)
     (let* ((root (map-elt fresh 'root))
            (path (agent-shell-cockpit-workspace-repository-path fresh repository))
            (storage (expand-file-name (concat ".agent-shell-cockpit/preserved/"
                                               (map-elt repository 'name)) root))
            (available (append
                        (split-string (agent-shell-cockpit-git--run path "ls-files" "--others" "--exclude-standard" "-z") "\0" t)
                        (split-string (agent-shell-cockpit-git--run path "ls-files" "--others" "--ignored" "--exclude-standard" "-z") "\0" t))))
       (when (agent-shell-cockpit-session-buffers-in-directory path)
         (user-error "Stop agents in this repository first"))
       (dolist (file files)
         (let ((source (expand-file-name file path)) (target (expand-file-name file storage)))
           (agent-shell-cockpit-lifecycle--contained root source)
           (agent-shell-cockpit-lifecycle--contained root target)
           (agent-shell-cockpit-lifecycle--check-buffers source)
           (unless (and (member file available) (file-regular-p source)
                        (not (file-exists-p target)))
             (user-error "Cannot preserve path safely: %s" file))))
       (dolist (file (delete-dups files))
         (let ((target (expand-file-name file storage)))
           (make-directory (file-name-directory target) t)
           (agent-shell-cockpit-lifecycle--rename (expand-file-name file path) target)))))))

(defun agent-shell-cockpit-workspace-delete-archive (workspace)
  "Permanently delete archived WORKSPACE after the caller confirms."
  (let* ((root (map-elt workspace 'root))
         (parent (file-name-directory (directory-file-name root))))
    (unless (and (not (file-remote-p root))
                 (not (file-symlink-p (directory-file-name root)))
                 (file-equal-p parent (agent-shell-cockpit-store-archive-directory)))
      (user-error "Refusing to delete path outside the archive: %s" root))
    (agent-shell-cockpit-store--with-lock
     (agent-shell-cockpit-store-metadata-path root)
     (lambda ()
       (let ((fresh (agent-shell-cockpit-store-read root)))
         (unless (and (equal (map-elt fresh 'state) "archived")
                      (not (map-elt fresh 'operation))
                      (equal (map-elt fresh 'id) (map-elt workspace 'id)))
           (user-error "Archive changed or needs recovery; refresh first"))
         (agent-shell-cockpit-lifecycle--check-buffers root)
         (when (agent-shell-cockpit-session-buffers-in-directory root)
           (user-error "Stop agents inside the archive first"))
         (delete-directory root t))))
    t))

(provide 'agent-shell-cockpit-lifecycle)

;;; agent-shell-cockpit-lifecycle.el ends here
