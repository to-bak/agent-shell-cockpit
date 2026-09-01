;;; agent-shell-cockpit-store.el --- Workspace persistence for cockpit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Read, validate, discover, and atomically persist cockpit workspace records.

;;; Code:

(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)

(defconst agent-shell-cockpit-store-schema-version 2
  "Current workspace metadata schema version.")

(defcustom agent-shell-cockpit-prompts-directory-name "prompts"
  "Directory name used for prompt files inside each workspace."
  :type 'string
  :group 'agent-shell-cockpit)

(defcustom agent-shell-cockpit-default-prompt-filename "prompt.org"
  "Filename used for the initial prompt in a new workspace."
  :type 'string
  :group 'agent-shell-cockpit)

(defcustom agent-shell-cockpit-repositories-directory-name "repositories"
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

(defun agent-shell-cockpit-store--prompt-title (root)
  "Return the first Org prompt title below ROOT, or its directory name."
  (let* ((directory (expand-file-name
                     agent-shell-cockpit-prompts-directory-name root))
         (files (when (file-directory-p directory)
                  (sort (directory-files-recursively directory "\\.org\\'")
                        #'string-lessp))))
    (or (seq-some
         (lambda (file)
           (with-temp-buffer
             (insert-file-contents file)
             (goto-char (point-min))
             (when (re-search-forward
                    "^#\\+TITLE:[[:space:]]*\\(.+\\)$" nil t)
               (string-trim (match-string 1)))))
         files)
        (file-name-nondirectory (directory-file-name root)))))

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
  (mapc #'agent-shell-cockpit-store--validate-session
        (map-elt record 'sessions))
  (setq root (file-name-as-directory (expand-file-name root)))
  (agent-shell-cockpit-store-set record 'root root)
  (agent-shell-cockpit-store-set record 'kind 'workspace)
  (agent-shell-cockpit-store-set
   record 'name (file-name-nondirectory (directory-file-name root)))
  (agent-shell-cockpit-store-set
   record 'title (agent-shell-cockpit-store--prompt-title root))
  (agent-shell-cockpit-store-set
   record 'state (if (agent-shell-cockpit-store--archived-root-p root)
                     "archived" "active"))
  record)

(defun agent-shell-cockpit-store-read (root)
  "Read and validate workspace metadata below ROOT.
Signal an error when metadata is missing or invalid."
  (let ((path (agent-shell-cockpit-store-metadata-path root)))
    (unless (file-readable-p path)
      (error "Missing metadata file: %s" path))
    (with-temp-buffer
      (insert-file-contents path)
      (agent-shell-cockpit-store--validate
       (json-parse-buffer :object-type 'alist
                          :array-type 'list
                          :null-object nil
                          :false-object nil)
       root))))

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

(defun agent-shell-cockpit-store--serializable-record (record)
  "Return the deliberately small persistent subset of RECORD."
  `((schemaVersion . ,agent-shell-cockpit-store-schema-version)
    (sessions
     . ,(vconcat
         (mapcar
          (lambda (session)
            (let ((copy `((agentId . ,(map-elt session 'agentId))
                          (sessionId . ,(map-elt session 'sessionId)))))
              (when (map-elt session 'title)
                (setq copy
                      (append copy
                              `((title . ,(map-elt session 'title))))))
              copy))
          (map-elt record 'sessions))))))

(defun agent-shell-cockpit-store-write (record)
  "Atomically persist workspace RECORD and return it."
  (let* ((root (map-elt record 'root))
         (metadata-directory
          (expand-file-name agent-shell-cockpit-store-metadata-directory root))
         (path (agent-shell-cockpit-store-metadata-path root)))
    (unless root
      (error "Workspace record has no runtime root"))
    (make-directory metadata-directory t)
    (let ((temporary (make-temp-file
                      (expand-file-name ".workspace-" metadata-directory))))
      (unwind-protect
          (progn
            (with-temp-file temporary
              (insert (json-serialize
                       (agent-shell-cockpit-store--serializable-record record)
                       :null-object nil :false-object nil))
              (insert "\n"))
            (rename-file temporary path t))
        (when (file-exists-p temporary)
          (delete-file temporary))))
    record))

(provide 'agent-shell-cockpit-store)

;;; agent-shell-cockpit-store.el ends here
