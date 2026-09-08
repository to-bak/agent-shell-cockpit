;;; agent-shell-cockpit-instructions.el --- Declarative instructions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak
;; Author: to-bak
;; Assisted-by: Codex:GPT-6
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Configured instructions are literals or references.  Reference adapters
;; supply a short bootstrap once per selection and optionally visit sources.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-cockpit-workspace)

(declare-function org-id-find-id-file "org-id")
(declare-function org-id-goto "org-id")

(defcustom agent-shell-cockpit-instructions nil
  "Instruction catalog: (ID :title TITLE :source (TYPE ARG...)) entries.
IDs are unique symbols.  Sources are literal or registered reference adapters.
For example: (review :title \"Review\" :source (file \"~/review.md\")).
The cockpit identifier is reserved for the workspace layout instruction."
  :type '(repeat sexp)
  :group 'agent-shell-cockpit)

(defcustom agent-shell-cockpit-default-instructions nil
  "Instruction identifiers initially selected when launching an agent."
  :type '(repeat symbol)
  :group 'agent-shell-cockpit)

(defvar agent-shell-cockpit-instruction-adapters nil
  "Adapters registered with `agent-shell-cockpit-register-instruction-adapter'.")

(cl-defun agent-shell-cockpit-register-instruction-adapter
    (type &key reference bootstrap visit preview)
  "Register TYPE with REFERENCE, optional BOOTSTRAP, VISIT and PREVIEW callbacks.
REFERENCE receives (SOURCE WORKSPACE), where SOURCE is the complete source
list and WORKSPACE is a record or nil.  It returns a nonempty reference string,
never copied source contents.  BOOTSTRAP is nil, a string, or a function of
WORKSPACE returning text or nil.  It is emitted once per selected adapter.
VISIT optionally receives (SOURCE WORKSPACE) and opens the authoritative source.
PREVIEW optionally receives (SOURCE WORKSPACE BUFFER), fills the temporary
BUFFER with source content, selects its major mode and positions point.
It must not display windows or modify source buffers.
Re-registering replaces TYPE.  Callbacks are trusted Emacs configuration;
the literal type is reserved."
  (unless (and type (symbolp type) (not (keywordp type)) (not (eq type 'literal)))
    (error "Invalid reference adapter type: %S" type))
  (unless (functionp reference) (error "Adapter requires a reference function"))
  (unless (or (null bootstrap) (stringp bootstrap) (functionp bootstrap))
    (error "Adapter bootstrap must be text or a function"))
  (unless (or (null visit) (functionp visit))
    (error "Adapter visit must be a function"))
  (unless (or (null preview) (functionp preview))
    (error "Adapter preview must be a function"))
  (setf (alist-get type agent-shell-cockpit-instruction-adapters)
        (list :reference reference :bootstrap bootstrap :visit visit :preview preview))
  type)

(defun agent-shell-cockpit-unregister-instruction-adapter (type)
  "Remove the reference adapter named TYPE."
  (setq agent-shell-cockpit-instruction-adapters
        (assq-delete-all type agent-shell-cockpit-instruction-adapters)))

(defun agent-shell-cockpit-instructions--text (text)
  "Validate and return nonempty TEXT without its text properties."
  (unless (and (stringp text) (not (string-empty-p (string-trim text))))
    (user-error "Instruction text or reference must be a nonempty string"))
  (substring-no-properties text))

(defun agent-shell-cockpit-instructions--catalog (workspace)
  "Return the validated configured catalog for WORKSPACE."
  (let ((catalog (copy-tree agent-shell-cockpit-instructions)) ids)
    (dolist (entry catalog)
      (let ((id (car-safe entry)) (source (plist-get (cdr entry) :source)))
        (unless (and id (symbolp id) (not (keywordp id))
                     (not (memq id (cons 'cockpit ids))))
          (user-error "Duplicate or invalid instruction identifier: %S" id))
        (push id ids)
        (agent-shell-cockpit-instructions--text (plist-get (cdr entry) :title))
        (unless (and (proper-list-p source) (car source) (symbolp (car source)))
          (user-error "Invalid instruction source for %s" id))))
    (when workspace
      (push `(cockpit :title "Cockpit workspace"
                     :source (literal ,(format
                                       (concat "The working directory is a Cockpit workspace root.\n"
                                               "- %s/ contains independent Git worktrees.\n"
                                               "- %s/ contains user-owned context files; read only when relevant.\n"
                                               "- .agent-shell-cockpit/ contains internal metadata; do not edit unless asked.\n"
                                               "The workspace root is not necessarily a Git repository.")
                                       (file-name-nondirectory
                                        (agent-shell-cockpit-workspace-worktrees-path workspace))
                                       agent-shell-cockpit-context-directory-name)))
            catalog))
    catalog))

(defun agent-shell-cockpit-instructions--adapter (type)
  "Return the adapter for TYPE or explain how to enable it."
  (or (alist-get type agent-shell-cockpit-instruction-adapters)
      (user-error "Instruction adapter %s is not registered; load its integration first" type)))

(defun agent-shell-cockpit-instructions-render (identifiers &optional workspace)
  "Render IDENTIFIERS for WORKSPACE as bootstraps, literals, and references.
Preserve selection order and emit each adapter bootstrap once.  Resolve all
references before returning; failures do not produce partial agent input."
  (let ((catalog (agent-shell-cockpit-instructions--catalog workspace))
        seen bootstraps items)
    (dolist (id (delete-dups (copy-sequence identifiers)))
      (let* ((entry (or (assq id catalog) (user-error "Unknown instruction: %s" id)))
             (source (plist-get (cdr entry) :source))
             (type (car source)))
        (if (eq type 'literal)
            (progn
              (unless (= (length source) 2) (user-error "Literal expects one string"))
              (push (agent-shell-cockpit-instructions--text (cadr source)) items))
          (let* ((adapter (agent-shell-cockpit-instructions--adapter type))
                 (reference (funcall (plist-get adapter :reference) source workspace)))
            (unless (memq type seen)
              (push type seen)
              (let* ((bootstrap (plist-get adapter :bootstrap))
                     (text (if (functionp bootstrap) (funcall bootstrap workspace) bootstrap)))
                (when text
                  (push (agent-shell-cockpit-instructions--text text) bootstraps))))
            (push (format "Read and follow %s:\n%s"
                          (plist-get (cdr entry) :title)
                          (agent-shell-cockpit-instructions--text reference)) items)))))
    (when items
      (string-join (append (nreverse bootstraps) (nreverse items)) "\n\n"))))

(defcustom agent-shell-cockpit-instructions-read-function
  #'agent-shell-cockpit-instructions-read-default
  "Function selecting ordered instruction IDs.
Called with optional WORKSPACE and SINGLE arguments."
  :type 'function :group 'agent-shell-cockpit)

(defun agent-shell-cockpit-instructions-read (&optional workspace single)
  "Choose ordered instruction IDs for WORKSPACE, or one when SINGLE."
  (funcall agent-shell-cockpit-instructions-read-function workspace single))

(defun agent-shell-cockpit-instructions-read-default (&optional workspace single)
  "Choose configured instruction identifiers for WORKSPACE.
When SINGLE is non-nil, choose one identifier for visiting."
  (let* ((catalog (agent-shell-cockpit-instructions--catalog workspace))
         (table (mapcar (lambda (entry) (cons (symbol-name (car entry)) (car entry))) catalog)))
    (when table
      (let* ((completion-extra-properties
              `(:annotation-function
                ,(lambda (candidate)
                   (let ((entry (assq (cdr (assoc candidate table)) catalog)))
                     (format "  %s [%s]" (plist-get (cdr entry) :title)
                             (car (plist-get (cdr entry) :source)))))))
             (choices
              (if single
                  (list (completing-read "Visit instruction: " table nil t))
                (completing-read-multiple
                 "Instructions (empty for none): " table nil t
                 (mapconcat #'symbol-name
                            (seq-filter (lambda (id) (assq id catalog))
                                        agent-shell-cockpit-default-instructions) ",")))))
        (mapcar (lambda (choice) (cdr (assoc choice table))) choices)))))

;;;###autoload
(defun agent-shell-cockpit-insert-instruction ()
  "Insert selected literals and instruction references at point, without sending."
  (interactive)
  (barf-if-buffer-read-only)
  (when-let* ((text (agent-shell-cockpit-instructions-render
                    (agent-shell-cockpit-instructions-read))))
    (insert text)))

;;;###autoload
(defun agent-shell-cockpit-visit-instruction ()
  "Visit the authoritative source of a configured instruction."
  (interactive)
  (let* ((id (car (agent-shell-cockpit-instructions-read nil t)))
         (entry (assq id (agent-shell-cockpit-instructions--catalog nil)))
         (source (plist-get (cdr entry) :source)))
    (unless entry (user-error "No configured instructions"))
    (when (eq (car source) 'literal) (user-error "Literal instructions live in your Emacs configuration"))
    (let ((visit (plist-get (agent-shell-cockpit-instructions--adapter (car source)) :visit)))
      (unless visit (user-error "This adapter does not support visiting"))
      (funcall visit source nil))))

(defun agent-shell-cockpit-instructions--file (source _workspace)
  "Return the absolute file reference described by SOURCE."
  (unless (and (= (length source) 2) (stringp (cadr source))
               (file-name-absolute-p (cadr source)))
    (user-error "File instructions require an absolute path (~/ is allowed)"))
  (let ((path (expand-file-name (cadr source))))
    (unless (and (file-regular-p path) (file-readable-p path))
      (user-error "Instruction file is missing or unreadable: %s" path))
    path))

(defun agent-shell-cockpit-instructions--visit-file (source workspace)
  "Visit the file described by SOURCE in WORKSPACE."
  (find-file (agent-shell-cockpit-instructions--file source workspace)))

(defun agent-shell-cockpit-instructions--org-id (source _workspace)
  "Return an Org ID and its current file location from SOURCE."
  (unless (and (= (length source) 2) (stringp (cadr source)))
    (user-error "Org ID instructions require one ID string"))
  (require 'org-id)
  (let* ((id (agent-shell-cockpit-instructions--text (cadr source)))
         (file (org-id-find-id-file id)))
    (unless (and file (file-readable-p file)) (user-error "Org ID not found: %s" id))
    (format "id:%s\nCurrent file: %s" id (expand-file-name file))))

(defun agent-shell-cockpit-instructions--visit-org-id (source workspace)
  "Visit the Org ID described by SOURCE in WORKSPACE."
  (agent-shell-cockpit-instructions--org-id source workspace)
  (org-id-goto (cadr source)))

(defun agent-shell-cockpit-instructions-preview-file (file buffer &optional position)
  "Copy FILE into preview BUFFER with its major mode, at POSITION.
Use unsaved text from an existing visiting buffer when available.  No file-local
variables are applied, and BUFFER does not become a visiting file buffer."
  (let ((existing (find-buffer-visiting file)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if existing
            (insert (with-current-buffer existing
                      (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max)))))
          (insert-file-contents file))
        (setq default-directory (file-name-directory (expand-file-name file)))
        (let ((buffer-file-name file)) (set-auto-mode))
        (goto-char (min (point-max) (max (point-min) (or position 1))))))))

(defun agent-shell-cockpit-instructions--preview-file (source workspace buffer)
  "Preview the file SOURCE in WORKSPACE using BUFFER."
  (agent-shell-cockpit-instructions-preview-file
   (agent-shell-cockpit-instructions--file source workspace) buffer))

(defun agent-shell-cockpit-instructions--preview-org-id (source workspace buffer)
  "Preview the Org ID SOURCE in WORKSPACE using BUFFER."
  (agent-shell-cockpit-instructions--org-id source workspace)
  (agent-shell-cockpit-instructions-preview-file (org-id-find-id-file (cadr source)) buffer)
  (with-current-buffer buffer
    (when (re-search-forward (concat "^[ \t]*:ID:[ \t]+" (regexp-quote (cadr source)) "[ \t]*$") nil t)
      (beginning-of-line))))

(defun agent-shell-cockpit-instructions-preview (id workspace buffer)
  "Preview instruction ID's source for WORKSPACE in temporary BUFFER.
Adapter previews never affect the text produced for an agent."
  (let* ((entry (or (assq id (agent-shell-cockpit-instructions--catalog workspace))
                    (user-error "Unknown instruction: %s" id)))
         (source (plist-get (cdr entry) :source)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (fundamental-mode)
        (erase-buffer)
        (if (eq (car source) 'literal)
            (progn (insert (agent-shell-cockpit-instructions--text (cadr source))) (text-mode) (goto-char (point-min)))
          (let ((preview (plist-get (agent-shell-cockpit-instructions--adapter (car source)) :preview)))
            (unless preview (user-error "Adapter %s has no source preview" (car source)))
            (funcall preview source workspace buffer)))
        (font-lock-ensure)
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)))))

(agent-shell-cockpit-register-instruction-adapter
 'file :reference #'agent-shell-cockpit-instructions--file
 :visit #'agent-shell-cockpit-instructions--visit-file
 :preview #'agent-shell-cockpit-instructions--preview-file)

(agent-shell-cockpit-register-instruction-adapter
 'org-id :reference #'agent-shell-cockpit-instructions--org-id
 :bootstrap "Resolve the selected Org ID by its exact :ID: property in the indicated file. Read its containing entry (or the whole file for a file-level ID). Follow relevant links; do not modify instruction sources unless explicitly asked."
 :visit #'agent-shell-cockpit-instructions--visit-org-id
 :preview #'agent-shell-cockpit-instructions--preview-org-id)

(provide 'agent-shell-cockpit-instructions)
;;; agent-shell-cockpit-instructions.el ends here
