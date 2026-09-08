;;; agent-shell-cockpit-workspace.el --- Workspace lifecycle -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak
;; Author: to-bak
;; Assisted-by: Codex:GPT-6

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Create, edit, locate, and archive durable cockpit workspaces.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'org-id)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-cockpit-store)


(defvar agent-shell-cockpit-workspace-move-hook nil
  "Hook called with old and new paths after a Cockpit file or directory move.
Visiting file buffers have already been retargeted when this hook runs.")

(defun agent-shell-cockpit-workspace--validate-name (name)
  "Validate workspace directory NAME and return it."
  (unless (and (stringp name)
               (string-match-p "\\`[[:alnum:]][[:alnum:]_.-]*\\'" name)
               (not (equal name ".archive")))
    (user-error "Invalid workspace name: %s" name))
  name)

(cl-defun agent-shell-cockpit-workspace-create (&key name)
  "Create and return a workspace named NAME.
The explicit display title initially defaults to NAME."
  (agent-shell-cockpit-workspace--validate-name name)
  (agent-shell-cockpit-workspace--validate-name
   agent-shell-cockpit-worktrees-directory-name)
  (agent-shell-cockpit-workspace--validate-name agent-shell-cockpit-context-directory-name)
  (when (equal agent-shell-cockpit-context-directory-name agent-shell-cockpit-worktrees-directory-name)
    (user-error "Context and worktree directories must differ"))
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
                (cons 'id (org-id-uuid))
                (cons 'name name)
                (cons 'displayTitle name)
                (cons 'worktreeDirectory agent-shell-cockpit-worktrees-directory-name)
                (cons 'root (file-name-as-directory temporary))
                (cons 'kind 'workspace))))
          (make-directory
           (expand-file-name agent-shell-cockpit-worktrees-directory-name
                             temporary))
          (make-directory
           (expand-file-name agent-shell-cockpit-context-directory-name
                             temporary))
          (agent-shell-cockpit-store-write record)
          (rename-file temporary target)
          (setq temporary nil)
          (agent-shell-cockpit-store-set
           record 'root (file-name-as-directory target))
          (setq record (agent-shell-cockpit-store-read target))
          record)
      (when (and temporary (file-directory-p temporary))
        (delete-directory temporary t)))))

(defun agent-shell-cockpit-workspace-context-path (workspace)
  "Return WORKSPACE's absolute context directory."
  (agent-shell-cockpit-workspace--validate-name agent-shell-cockpit-context-directory-name)
  (file-name-as-directory
   (expand-file-name agent-shell-cockpit-context-directory-name
                     (map-elt workspace 'root))))

(defun agent-shell-cockpit-workspace-context-paths (workspace)
  "Return WORKSPACE's context files in stable relative-name order."
  (let ((directory (agent-shell-cockpit-workspace-context-path workspace)))
    (when (file-directory-p directory)
      (sort (directory-files-recursively
             directory directory-files-no-dot-files-regexp)
            (lambda (left right)
              (string-lessp (file-relative-name left directory)
                            (file-relative-name right directory)))))))

(defun agent-shell-cockpit-workspace-context-name (workspace path)
  "Return a concise display name for context PATH in WORKSPACE."
  (file-relative-name path
                      (agent-shell-cockpit-workspace-context-path workspace)))

(defun agent-shell-cockpit-workspace--new-context-path (workspace)
  "Read and return a new context path inside WORKSPACE."
  (let* ((directory (agent-shell-cockpit-workspace-context-path workspace))
         (name (read-string "New context filename: ")))
    (unless (and (not (string-empty-p name))
                 (equal name (file-name-nondirectory name))
                 (not (member name '("." ".."))))
      (user-error "Context filename must be a plain filename"))
    (expand-file-name name directory)))

(defun agent-shell-cockpit-workspace-read-context (workspace &optional allow-new)
  "Read and return a context file for WORKSPACE.
When ALLOW-NEW is non-nil, offer to create a new file."
  (let* ((paths (agent-shell-cockpit-workspace-context-paths workspace))
         (choices (mapcar
                   (lambda (path)
                     (cons (agent-shell-cockpit-workspace-context-name
                            workspace path)
                           path))
                   paths))
         (new "[New context]"))
    (cond
     ((and (not allow-new) (= (length paths) 1)) (car paths))
     ((and (not allow-new) (null paths))
      (user-error "Workspace has no context files"))
     (t
      (let ((choice (completing-read
                     "Context: " (if allow-new (cons new choices) choices)
                     nil t)))
        (if (equal choice new)
            (agent-shell-cockpit-workspace--new-context-path workspace)
          (cdr (assoc choice choices))))))))

(defun agent-shell-cockpit-workspace-edit-context (workspace)
  "Select and edit one of WORKSPACE's context files."
  (find-file (agent-shell-cockpit-workspace-read-context workspace t)))

(defun agent-shell-cockpit-workspace-repair (invalid-record)
  "Back up and reconstruct metadata for INVALID-RECORD.
Session metadata remains available only in the timestamped backup."
  (unless (eq (map-elt invalid-record 'kind) 'invalid)
    (user-error "Workspace metadata is not marked invalid"))
  (let* ((root (map-elt invalid-record 'root))
         (path (agent-shell-cockpit-store-metadata-path root))
         (record
          (list (cons 'schemaVersion agent-shell-cockpit-store-schema-version)
                (cons 'sessions nil)
                (cons 'id (org-id-uuid))
                (cons 'displayTitle (file-name-nondirectory (directory-file-name root)))
                (cons 'worktreeDirectory agent-shell-cockpit-worktrees-directory-name)
                (cons 'root root)
                (cons 'kind 'workspace))))
    (when (file-exists-p path)
      (copy-file path (format "%s.backup-%s" path
                              (format-time-string "%Y%m%dT%H%M%S")) t))
    (make-directory (agent-shell-cockpit-workspace-context-path record) t)
    (agent-shell-cockpit-store-write record t)
    (agent-shell-cockpit-store-read root)))

(defun agent-shell-cockpit-workspace-worktrees-path (workspace)
  "Return WORKSPACE's recorded worktree directory."
  (let* ((root (map-elt workspace 'root))
         (name (map-elt workspace 'worktreeDirectory)))
    (agent-shell-cockpit-workspace--validate-name name)
    (expand-file-name name root)))

(defun agent-shell-cockpit-workspace-active-worktrees (workspace)
  "Return recorded and discovered worktrees belonging to WORKSPACE."
  (let* ((directory (agent-shell-cockpit-workspace-worktrees-path workspace))
         (records (copy-tree (map-elt workspace 'worktrees))))
    (when (file-directory-p directory)
      (dolist (path (directory-files directory t directory-files-no-dot-files-regexp t))
        (let ((name (file-name-nondirectory path)))
          (when (and (file-directory-p path)
                     (not (seq-find (lambda (item) (equal (map-elt item 'name) name)) records)))
            (push `((name . ,name)) records)))))
    (sort records (lambda (left right)
                    (string-lessp (map-elt left 'name) (map-elt right 'name))))))

(defun agent-shell-cockpit-workspace-repository-path (workspace repository)
  "Return absolute path for REPOSITORY within WORKSPACE."
  (agent-shell-cockpit-workspace--validate-name (map-elt repository 'name))
  (expand-file-name
   (map-elt repository 'name)
   (agent-shell-cockpit-workspace-worktrees-path workspace)))

(autoload 'agent-shell-cockpit-workspace--archive-preflight "agent-shell-cockpit-lifecycle")
(autoload 'agent-shell-cockpit-workspace-archive "agent-shell-cockpit-lifecycle")
(autoload 'agent-shell-cockpit-workspace-restore "agent-shell-cockpit-lifecycle")

(autoload 'agent-shell-cockpit-workspace-delete-archive "agent-shell-cockpit-lifecycle")

(provide 'agent-shell-cockpit-workspace)

;;; agent-shell-cockpit-workspace.el ends here
