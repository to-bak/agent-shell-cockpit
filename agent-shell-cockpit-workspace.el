;;; agent-shell-cockpit-workspace.el --- Workspace lifecycle -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Create, edit, locate, and archive durable cockpit workspaces.

;;; Code:

(require 'map)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-cockpit-store)

(declare-function agent-shell-cockpit-git-clean-p "agent-shell-cockpit-git")
(declare-function agent-shell-cockpit-git-remove-worktree "agent-shell-cockpit-git")
(declare-function agent-shell-cockpit-session-live-buffers "agent-shell-cockpit-session")

(defun agent-shell-cockpit-workspace-slug (text)
  "Return a filesystem-friendly slug derived from TEXT."
  (let ((slug (downcase (string-trim text))))
    (setq slug (replace-regexp-in-string "[^[:alnum:]._-]+" "-" slug))
    (string-trim slug "[-_.]+" "[-_.]+")))

(defun agent-shell-cockpit-workspace--validate-name (name)
  "Validate workspace directory NAME and return it."
  (unless (and (stringp name)
               (string-match-p "\\`[[:alnum:]][[:alnum:]_.-]*\\'" name)
               (not (equal name ".archive")))
    (user-error "Invalid workspace name: %s" name))
  name)

(cl-defun agent-shell-cockpit-workspace-create (&key name title)
  "Create and return a workspace named NAME.
TITLE defaults to NAME and is written to its initial prompt file."
  (agent-shell-cockpit-workspace--validate-name name)
  (setq title (or title name))
  (when (string-empty-p (string-trim title))
    (user-error "Workspace title cannot be empty"))
  (let* ((parent (file-name-as-directory
                  (expand-file-name agent-shell-cockpit-workspace-directory)))
         (target (expand-file-name name parent))
         (temporary nil))
    (when (file-exists-p target)
      (user-error "Workspace already exists: %s" target))
    (make-directory parent t)
    (setq temporary (make-temp-file
                     (expand-file-name ".cockpit-create-" parent) t))
    (unwind-protect
         (let ((record
               (list
                (cons 'schemaVersion agent-shell-cockpit-store-schema-version)
                (cons 'sessions nil)
                (cons 'root (file-name-as-directory temporary))
                (cons 'kind 'workspace))))
          (make-directory
           (expand-file-name agent-shell-cockpit-repositories-directory-name
                             temporary))
          (make-directory
           (expand-file-name agent-shell-cockpit-prompts-directory-name
                             temporary))
          (with-temp-file
              (expand-file-name
               agent-shell-cockpit-default-prompt-filename
               (expand-file-name
                agent-shell-cockpit-prompts-directory-name temporary))
            (insert "#+TITLE: " title "\n"))
          (agent-shell-cockpit-store-write record)
          (rename-file temporary target)
          (setq temporary nil)
         (agent-shell-cockpit-store-set
           record 'root (file-name-as-directory target))
          (setq record (agent-shell-cockpit-store-read target))
          (ignore-errors
            (project-remember-project (list 'agent-shell-cockpit
                                            (map-elt record 'root))))
          record)
      (when (and temporary (file-directory-p temporary))
        (delete-directory temporary t)))))

(defun agent-shell-cockpit-workspace-prompts-path (workspace)
  "Return WORKSPACE's absolute prompt directory."
  (file-name-as-directory
   (expand-file-name agent-shell-cockpit-prompts-directory-name
                     (map-elt workspace 'root))))

(defun agent-shell-cockpit-workspace-prompt-paths (workspace)
  "Return WORKSPACE's prompt files in stable relative-name order."
  (let ((directory (agent-shell-cockpit-workspace-prompts-path workspace)))
    (when (file-directory-p directory)
      (sort (directory-files-recursively
             directory directory-files-no-dot-files-regexp)
            (lambda (left right)
              (string-lessp (file-relative-name left directory)
                            (file-relative-name right directory)))))))

(defun agent-shell-cockpit-workspace-prompt-name (workspace path)
  "Return a concise display name for prompt PATH in WORKSPACE."
  (file-relative-name path
                      (agent-shell-cockpit-workspace-prompts-path workspace)))

(defun agent-shell-cockpit-workspace--new-prompt-path (workspace)
  "Read and return a new prompt path inside WORKSPACE."
  (let* ((directory (agent-shell-cockpit-workspace-prompts-path workspace))
         (name (read-string "New prompt filename: "
                            agent-shell-cockpit-default-prompt-filename)))
    (unless (and (not (string-empty-p name))
                 (equal name (file-name-nondirectory name))
                 (not (member name '("." ".."))))
      (user-error "Prompt filename must be a plain filename"))
    (expand-file-name name directory)))

(defun agent-shell-cockpit-workspace-read-prompt (workspace)
  "Read and return a prompt file for WORKSPACE, allowing a new file."
  (let* ((paths (agent-shell-cockpit-workspace-prompt-paths workspace))
         (choices (mapcar
                   (lambda (path)
                     (cons (agent-shell-cockpit-workspace-prompt-name
                            workspace path)
                           path))
                   paths))
         (new "[New prompt]")
         (choice (completing-read "Prompt: " (cons new choices) nil t)))
    (if (equal choice new)
        (agent-shell-cockpit-workspace--new-prompt-path workspace)
      (cdr (assoc choice choices)))))

(defun agent-shell-cockpit-workspace-edit-prompt (workspace)
  "Select and edit one of WORKSPACE's prompt files."
  (find-file (agent-shell-cockpit-workspace-read-prompt workspace)))

(defun agent-shell-cockpit-workspace-repair (invalid-record)
  "Back up and reconstruct metadata for INVALID-RECORD.
Session metadata remains available only in the timestamped backup."
  (unless (eq (map-elt invalid-record 'kind) 'invalid)
    (user-error "Workspace metadata is not marked invalid"))
  (let* ((root (map-elt invalid-record 'root))
         (path (agent-shell-cockpit-store-metadata-path root))
         (title (agent-shell-cockpit-store--prompt-title root))
         (record
          (list (cons 'schemaVersion agent-shell-cockpit-store-schema-version)
                (cons 'sessions nil)
                (cons 'root root)
                (cons 'kind 'workspace))))
    (when (file-exists-p path)
      (copy-file path (format "%s.backup-%s" path
                              (format-time-string "%Y%m%dT%H%M%S")) t))
    (let* ((prompt-directory
            (agent-shell-cockpit-workspace-prompts-path record))
           (prompt (expand-file-name
                    agent-shell-cockpit-default-prompt-filename
                    prompt-directory)))
      (make-directory prompt-directory t)
      (unless (agent-shell-cockpit-workspace-prompt-paths record)
        (with-temp-file prompt
          (insert "#+TITLE: " title "\n"))))
    (agent-shell-cockpit-store-write record)
    (agent-shell-cockpit-store-read root)))

(defun agent-shell-cockpit-workspace-active-repositories (workspace)
  "Return repository records derived from WORKSPACE's worktree directory."
  (let ((directory
         (expand-file-name agent-shell-cockpit-repositories-directory-name
                           (map-elt workspace 'root))))
    (when (file-directory-p directory)
      (mapcar
       (lambda (path)
         `((name . ,(file-name-nondirectory (directory-file-name path)))))
       (seq-filter #'file-directory-p
                   (directory-files directory t
                                    directory-files-no-dot-files-regexp t))))))

(defun agent-shell-cockpit-workspace-repository-path (workspace repository)
  "Return absolute path for REPOSITORY within WORKSPACE."
  (expand-file-name
   (map-elt repository 'name)
   (expand-file-name agent-shell-cockpit-repositories-directory-name
                     (map-elt workspace 'root))))

(defun agent-shell-cockpit-workspace--archive-preflight (workspace)
  "Signal when WORKSPACE cannot be archived safely."
  (unless (equal (map-elt workspace 'state) "active")
    (user-error "Workspace is not active"))
  (when (and (fboundp 'agent-shell-cockpit-session-live-buffers)
             (agent-shell-cockpit-session-live-buffers workspace))
    (user-error "Stop the workspace's live agent sessions before archiving"))
  (let ((destination
         (expand-file-name (map-elt workspace 'name)
                           (agent-shell-cockpit-store-archive-directory))))
    (when (file-exists-p destination)
      (user-error "Archive destination already exists: %s" destination)))
  (dolist (repository
           (agent-shell-cockpit-workspace-active-repositories workspace))
    (let ((path (agent-shell-cockpit-workspace-repository-path
                 workspace repository)))
      (unless (file-directory-p path)
        (user-error "Worktree is missing: %s" path))
      (unless (agent-shell-cockpit-git-clean-p path)
        (user-error "Worktree has tracked or untracked changes: %s" path)))))

(defun agent-shell-cockpit-workspace-archive (workspace)
  "Safely archive WORKSPACE and return its updated record."
  (require 'agent-shell-cockpit-git)
  (agent-shell-cockpit-workspace--archive-preflight workspace)
  (let* ((old-root (map-elt workspace 'root))
         (archive-root (agent-shell-cockpit-store-archive-directory))
         (destination (expand-file-name (map-elt workspace 'name)
                                        archive-root)))
    (dolist (repository
             (copy-sequence
              (agent-shell-cockpit-workspace-active-repositories workspace)))
      (agent-shell-cockpit-git-remove-worktree workspace repository))
    (make-directory archive-root t)
    (rename-file old-root destination)
    (ignore-errors (project-forget-project old-root))
    (agent-shell-cockpit-store-read destination)))

(provide 'agent-shell-cockpit-workspace)

;;; agent-shell-cockpit-workspace.el ends here
