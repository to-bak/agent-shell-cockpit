;;; agent-shell-cockpit-store.el --- Workspace persistence for cockpit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak
;; Author: to-bak
;; Assisted-by: Codex:GPT-6

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Read, validate, discover, and atomically persist cockpit workspace records.

;;; Code:

(require 'cl-lib)
(require 'org-id)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)

(defconst agent-shell-cockpit-store-schema-version 4
  "Current workspace metadata schema version.")

(defcustom agent-shell-cockpit-context-directory-name "context"
  "Directory name used for context files inside each workspace."
  :type 'string
  :group 'agent-shell-cockpit)

(defcustom agent-shell-cockpit-worktrees-directory-name "worktrees"
  "Directory name used for Git worktrees inside each workspace."
  :type 'string
  :group 'agent-shell-cockpit)

(defconst agent-shell-cockpit-store-metadata-directory
  ".agent-shell-cockpit"
  "Directory containing cockpit-owned workspace metadata.")

(defconst agent-shell-cockpit-store-metadata-file "workspace.json"
  "File containing cockpit-owned workspace metadata.")

(defun agent-shell-cockpit-store-set (record key value)
  "Set KEY to VALUE in RECORD and return RECORD.
Unlike `map-put!', this function also appends missing keys to an alist in
place, which keeps references held by parent records valid."
  (if-let* ((entry (assq key record)))
      (setcdr entry value)
    (nconc record (list (cons key value))))
  record)

(defcustom agent-shell-cockpit-workspace-directory
  (expand-file-name "~/.workspaces/")
  "Directory containing active cockpit workspaces."
  :type 'directory
  :group 'agent-shell-cockpit)

(defcustom agent-shell-cockpit-archive-directory nil
  "Directory containing archived cockpit workspaces.
When nil, use a .archive directory below
`agent-shell-cockpit-workspace-directory'."
  :type '(choice (const :tag "Inside workspace directory" nil)
                 directory)
  :group 'agent-shell-cockpit)

(defun agent-shell-cockpit-store-archive-directory ()
  "Return the expanded cockpit archive directory."
  (file-name-as-directory
   (expand-file-name
    (or agent-shell-cockpit-archive-directory
        (expand-file-name ".archive"
                          agent-shell-cockpit-workspace-directory)))))

(defun agent-shell-cockpit-store-metadata-path (root)
  "Return the metadata path for workspace ROOT."
  (expand-file-name agent-shell-cockpit-store-metadata-file
                    (expand-file-name
                     agent-shell-cockpit-store-metadata-directory root)))

(defun agent-shell-cockpit-store--required-string (record key)
  "Validate that RECORD has a non-empty string at KEY."
  (unless (and (stringp (map-elt record key))
               (not (string-empty-p (map-elt record key))))
    (error "Missing or invalid %s" key)))

(defun agent-shell-cockpit-store--archived-root-p (root)
  "Return non-nil when ROOT is below the configured archive directory."
  (file-in-directory-p
   (file-truename root)
   (file-truename (agent-shell-cockpit-store-archive-directory))))

(defun agent-shell-cockpit-store--validate-session (session)
  "Validate a resumable SESSION record."
  (unless (listp session)
    (error "Session metadata is not a JSON object"))
  (dolist (key '(agentId sessionId))
    (agent-shell-cockpit-store--required-string session key))
  (when-let* ((settings (map-elt session 'settings)))
    (unless (and (sequencep settings) (not (stringp settings)))
      (error "Invalid session settings"))
    (let (seen)
      (seq-doseq (entry settings)
        (unless (and (listp entry) (= (length entry) 2))
          (error "Invalid session setting"))
        (dolist (key '(id value))
          (agent-shell-cockpit-store--required-string entry key))
        (when (member (map-elt entry 'id) seen)
          (error "Duplicate session setting"))
        (push (map-elt entry 'id) seen)))
    (agent-shell-cockpit-store-set session 'settings (append settings nil)))
  (when (and (map-elt session 'title)
             (not (stringp (map-elt session 'title))))
    (error "Invalid session title")))

(defun agent-shell-cockpit-store--validate (record root)
  "Validate RECORD loaded from workspace ROOT and return it."
  (unless (listp record)
    (error "Workspace metadata is not a JSON object"))
  (unless (equal (map-elt record 'schemaVersion)
                 agent-shell-cockpit-store-schema-version)
    (error "Unsupported schema version: %S"
           (map-elt record 'schemaVersion)))
  (unless (listp (map-elt record 'sessions))
    (error "Invalid sessions collection"))
  (when (and (map-elt record 'archivedAt)
             (not (numberp (map-elt record 'archivedAt))))
    (error "Invalid archive timestamp"))
  (mapc #'agent-shell-cockpit-store--validate-session
        (map-elt record 'sessions))
  (setq root (file-name-as-directory (expand-file-name root)))
  (let* ((archived (agent-shell-cockpit-store--archived-root-p root))
         (name
          (if archived
              (map-elt record 'name)
            (file-name-nondirectory (directory-file-name root)))))
    (agent-shell-cockpit-store-set record 'root root)
    (agent-shell-cockpit-store-set record 'kind 'workspace)
    (agent-shell-cockpit-store-set record 'name name)
    (agent-shell-cockpit-store-set
     record 'title
     (or (map-elt record 'displayTitle)
         name))
    (dolist (key '(worktrees))
      (unless (listp (map-elt record key))
        (error "Invalid %s collection" key)))
    (dolist (key '(id name displayTitle worktreeDirectory))
      (agent-shell-cockpit-store--required-string record key))
    (dolist (entry (map-elt record 'worktrees))
      (agent-shell-cockpit-store--required-string entry 'name)
      (unless (string-match-p "\\`[[:alnum:]][[:alnum:]_.-]*\\'" (map-elt entry 'name))
        (error "Invalid repository name")))
    (when-let* ((directory (map-elt record 'worktreeDirectory)))
      (unless (and (stringp directory)
                   (string-match-p "\\`[[:alnum:]][[:alnum:]_.-]*\\'" directory))
        (error "Invalid worktree directory")))
    (agent-shell-cockpit-store-set
     record 'state (if archived "archived" "active"))
    record))

(defun agent-shell-cockpit-store-read (root)
  "Read and validate workspace metadata below ROOT.
Signal an error when metadata is missing or invalid."
  (let ((path (agent-shell-cockpit-store-metadata-path root)))
    (unless (file-readable-p path)
      (error "Missing metadata file: %s" path))
    (with-temp-buffer
      (insert-file-contents path)
      (let ((record (json-parse-buffer :object-type 'alist :array-type 'array
                                       :null-object nil :false-object nil)))
        (dolist (key '(sessions worktrees))
          (when (vectorp (map-elt record key))
            (agent-shell-cockpit-store-set record key (append (map-elt record key) nil))))
        (agent-shell-cockpit-store--validate record root)
        (agent-shell-cockpit-store-set record 'revision (secure-hash 'sha256 (current-buffer)))
        record))))

(defun agent-shell-cockpit-store--invalid-record (root err)
  "Return an invalid workspace record for ROOT and ERR."
  `((kind . invalid)
    (root . ,(file-name-as-directory (expand-file-name root)))
    (name . ,(file-name-nondirectory (directory-file-name root)))
    (error . ,(error-message-string err))))

(defun agent-shell-cockpit-store--workspace-directories (parent)
  "Return candidate workspace directories immediately below PARENT."
  (when (file-directory-p parent)
    (seq-filter
     (lambda (path)
       (and (file-directory-p path)
            (not (member (file-name-nondirectory (directory-file-name path))
                         '("." ".." ".archive")))))
     (directory-files parent t directory-files-no-dot-files-regexp t))))

(defun agent-shell-cockpit-store-discover (&optional archived)
  "Discover cockpit workspaces.
When ARCHIVED is non-nil, scan the archive directory instead.  Invalid
workspace records are included so the UI can report them."
  (let ((parent (if archived
                    (agent-shell-cockpit-store-archive-directory)
                  (file-name-as-directory
                   (expand-file-name agent-shell-cockpit-workspace-directory)))))
    (mapcar
     (lambda (root)
       (condition-case err
           (agent-shell-cockpit-store-read root)
         (error (agent-shell-cockpit-store--invalid-record root err))))
     (agent-shell-cockpit-store--workspace-directories parent))))

(defun agent-shell-cockpit-store--fields (record fields)
  "Copy persistent FIELDS from RECORD, omitting absent values."
  (delq nil (mapcar (lambda (key)
                      (when (map-elt record key)
                        (cons key (map-elt record key)))) fields)))

(defun agent-shell-cockpit-store--serializable-record (record)
  "Return the validated persistent subset of RECORD."
  (append
   `((schemaVersion . ,agent-shell-cockpit-store-schema-version)
     (sessions . ,(vconcat
                   (mapcar
                    (lambda (session)
                      (append
                       (agent-shell-cockpit-store--fields
                        session '(agentId sessionId title cwd displayId))
                       (when (assq 'settings session)
                         `((settings . ,(vconcat (map-elt session 'settings)))))))
                    (map-elt record 'sessions)))))
   (agent-shell-cockpit-store--fields
    record '(id name displayTitle archivedAt operation worktreeDirectory))
   `((worktrees . ,(vconcat (map-elt record 'worktrees))))))

(defvar agent-shell-cockpit-store--locked-path nil
  "Metadata path locked by the current synchronous update.")

(defun agent-shell-cockpit-store--with-lock (path function)
  "Call FUNCTION while exclusively locking metadata PATH.
A surviving lock after a crash requires explicit inspection and removal."
  (if (equal path agent-shell-cockpit-store--locked-path)
      (funcall function)
    (let ((lock (concat path ".write-lock")))
      (condition-case nil
          (make-directory lock)
        (file-already-exists
         (user-error "Workspace is locked: %s (inspect before removing)" lock)))
      (unwind-protect
          (let ((agent-shell-cockpit-store--locked-path path))
            (funcall function))
        (delete-directory lock)))))

(defun agent-shell-cockpit-store--revision (path)
  "Return the content revision of PATH, or nil when missing."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (secure-hash 'sha256 (current-buffer)))))

(defun agent-shell-cockpit-store-write (record &optional repair)
  "Atomically persist workspace RECORD and return it.
Reject stale writes.  REPAIR explicitly permits replacing invalid metadata."
  (let ((root (map-elt record 'root)))
    (unless root (error "Workspace record has no runtime root"))
    (let* ((path (agent-shell-cockpit-store-metadata-path root))
           (directory (file-name-directory path)))
      (make-directory directory t)
      (agent-shell-cockpit-store--with-lock
       path
       (lambda ()
         (unless (or repair
                     (equal (map-elt record 'revision)
                            (agent-shell-cockpit-store--revision path)))
           (user-error "Workspace changed; refresh before retrying"))
         (agent-shell-cockpit-store--validate record root)
         (let ((temporary (make-temp-file
                           (expand-file-name ".workspace-" directory))))
           (unwind-protect
               (progn
                 (with-temp-file temporary
                   (insert (json-serialize
                            (agent-shell-cockpit-store--serializable-record record)
                            :null-object nil :false-object nil) "\n"))
                 (rename-file temporary path t)
                 (agent-shell-cockpit-store-set
                  record 'schemaVersion agent-shell-cockpit-store-schema-version)
                 (agent-shell-cockpit-store-set
                  record 'revision (agent-shell-cockpit-store--revision path)))
             (when (file-exists-p temporary) (delete-file temporary))))))
      record)))

(defun agent-shell-cockpit-store-update (root function &optional operation)
  "Apply FUNCTION to fresh metadata at ROOT and atomically persist it.
OPERATION permits an update during an explicitly coordinated lifecycle job."
  (let ((path (agent-shell-cockpit-store-metadata-path root)))
    (agent-shell-cockpit-store--with-lock
     path
     (lambda ()
       (let ((record (agent-shell-cockpit-store-read root)))
         (when (and (map-elt record 'operation) (not operation))
           (user-error "Workspace has an incomplete lifecycle operation"))
         (funcall function record)
         (agent-shell-cockpit-store-write record))))))

(provide 'agent-shell-cockpit-store)

;;; agent-shell-cockpit-store.el ends here
