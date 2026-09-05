;;; agent-shell-cockpit-skills.el --- Stackable launch skills -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Register reusable textual skills and compose selected skills when starting
;; an agent from Cockpit.  Skill contents may be inline, file-backed, or
;; generated from the current workspace.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)

(declare-function agent-shell-cockpit-session-start-select
                  "agent-shell-cockpit-session")

(defconst agent-shell-cockpit-skills--cockpit
  '(cockpit
    :key "c"
    :title "Cockpit workspace"
    :description "Explain the workspace, repository, and context layout."
    :content
    "You are working inside an agent-shell Cockpit workspace.

The current working directory is the workspace root.

- `repositories/` contains Git worktrees.  Each child directory is an independent repository and project root.
- `context/` contains user-owned context and reference files.  Inspect them only when relevant to the request.
- `.agent-shell-cockpit/` contains Cockpit's internal metadata.  Do not edit it unless explicitly requested.
- The workspace root is an aggregate directory and is not necessarily a Git repository.")
  "Built-in skill describing the Cockpit workspace layout.")

(defvar agent-shell-cockpit-skills nil
  "User-defined Cockpit launch skills.
Register entries with `agent-shell-cockpit-register-skill'.")

(defcustom agent-shell-cockpit-default-skills nil
  "Skill identifiers selected by default when starting an agent.
Nil keeps an empty launch as the default.  The user can still add or remove
skills interactively for each new agent."
  :type '(repeat symbol)
  :group 'agent-shell-cockpit)

(defvar agent-shell-cockpit-skills--launch-workspace nil
  "Workspace associated with the active launch menu.")

(defvar agent-shell-cockpit-skills--launch-selection nil
  "Ordered skill identifiers enabled in the active launch menu.")

(defconst agent-shell-cockpit-skills--reserved-keys
  '("RET" "q" "C-g")
  "Keys reserved for launch actions or Transient itself.")

(cl-defun agent-shell-cockpit-register-skill
    (identifier &key key title description content file function)
  "Register a launch skill named IDENTIFIER.
KEY is its single-event launch-menu key.  TITLE is its menu label and
DESCRIPTION is an optional annotation.
Exactly one of CONTENT, FILE, or FUNCTION supplies the skill text.  CONTENT
is a string, FILE is read literally when selected, and FUNCTION receives the
current workspace and returns a string.  Re-registering IDENTIFIER replaces
the previous definition while retaining user registration order."
  (unless (symbolp identifier)
    (error "Skill identifier must be a symbol: %S" identifier))
  (when (eq identifier 'cockpit)
    (error "The cockpit skill identifier is reserved"))
  (unless (and (stringp key) (= (length (kbd key)) 1))
    (error "Skill key must describe one key event: %S" key))
  (setq key (key-description (kbd key)))
  (when (member key agent-shell-cockpit-skills--reserved-keys)
    (error "Skill key is reserved by the launch menu: %s" key))
  (dolist (skill (agent-shell-cockpit-skills-all))
    (when (and (not (eq (car skill) identifier))
               (equal (plist-get (cdr skill) :key) key))
      (error "Skill key %s is already used by %s" key (car skill))))
  (unless (and (stringp title) (not (string-empty-p title)))
    (error "Skill title must be a non-empty string"))
  (when (and description (not (stringp description)))
    (error "Skill description must be a string"))
  (unless (= (length (delq nil (list content file function))) 1)
    (error "Skill must define exactly one of :content, :file, or :function"))
  (unless (or (null content) (stringp content))
    (error "Skill content must be a string"))
  (unless (or (null file) (stringp file))
    (error "Skill file must be a string"))
  (unless (or (null function) (functionp function))
    (error "Skill function must be callable"))
  (let ((entry
         (list identifier
               :key key
               :title title
               :description description
               :content content
               :file file
               :function function))
        (existing (assq identifier agent-shell-cockpit-skills)))
    (if existing
        (setcdr existing (cdr entry))
      (setq agent-shell-cockpit-skills
            (append agent-shell-cockpit-skills (list entry))))
    identifier))

(defun agent-shell-cockpit-unregister-skill (identifier)
  "Remove the user-defined skill named IDENTIFIER."
  (setq agent-shell-cockpit-skills
        (assq-delete-all identifier agent-shell-cockpit-skills)))

(defun agent-shell-cockpit-skills-all ()
  "Return the built-in skill followed by registered user skills."
  (cons agent-shell-cockpit-skills--cockpit
        agent-shell-cockpit-skills))

(defun agent-shell-cockpit-skills--read-file (path)
  "Return the literal contents of skill file PATH."
  (with-temp-buffer
    (insert-file-contents (expand-file-name path))
    (buffer-string)))

(defun agent-shell-cockpit-skills-render (identifier workspace)
  "Render skill IDENTIFIER for WORKSPACE and return its text."
  (let ((skill (assq identifier (agent-shell-cockpit-skills-all))))
    (unless skill
      (user-error "Unknown Cockpit skill: %s" identifier))
    (let ((text
           (cond
            ((stringp (plist-get (cdr skill) :content))
             (plist-get (cdr skill) :content))
            ((plist-get (cdr skill) :file)
             (agent-shell-cockpit-skills--read-file
              (plist-get (cdr skill) :file)))
            ((plist-get (cdr skill) :function)
             (funcall (plist-get (cdr skill) :function) workspace)))))
      (unless (stringp text)
        (error "Cockpit skill %s did not produce text" identifier))
      (string-trim text))))

(defun agent-shell-cockpit-skills-compose (identifiers workspace)
  "Render IDENTIFIERS for WORKSPACE and join them in order.
Return nil when IDENTIFIERS is empty."
  (when identifiers
    (string-join
     (mapcar (lambda (identifier)
               (agent-shell-cockpit-skills-render identifier workspace))
             identifiers)
     "\n\n")))

(defun agent-shell-cockpit-skills--known-identifiers (identifiers)
  "Return only known skill IDENTIFIERS, preserving their order."
  (let ((skills (agent-shell-cockpit-skills-all)))
    (delq nil
          (mapcar (lambda (identifier)
                    (and (assq identifier skills) identifier))
                  identifiers))))

(defun agent-shell-cockpit-skills--toggle (identifier)
  "Toggle launch skill IDENTIFIER in the active selection."
  (if (memq identifier agent-shell-cockpit-skills--launch-selection)
      (setq agent-shell-cockpit-skills--launch-selection
            (delq identifier agent-shell-cockpit-skills--launch-selection))
    (setq agent-shell-cockpit-skills--launch-selection
          (append agent-shell-cockpit-skills--launch-selection
                  (list identifier)))))

(defun agent-shell-cockpit-skills--menu-children (_children)
  "Return dynamic Transient suffixes for all keyed launch skills."
  (transient-parse-suffixes
   'agent-shell-cockpit-skills-menu
   (delq
    nil
    (mapcar
     (lambda (skill)
       (when-let* ((key (plist-get (cdr skill) :key)))
         (let ((identifier (car skill))
               (title (plist-get (cdr skill) :title))
               (description (plist-get (cdr skill) :description)))
           (list
            key
            (lambda ()
              (interactive)
              (agent-shell-cockpit-skills--toggle identifier))
            :description
            (lambda ()
              (format "%s %s%s"
                      (if (memq identifier
                                agent-shell-cockpit-skills--launch-selection)
                          "[x]" "[ ]")
                      title
                      (if description (concat " — " description) "")))
            :transient t))))
     (agent-shell-cockpit-skills-all)))))

(transient-define-suffix agent-shell-cockpit-skills-start-agent ()
  "Choose an agent and start it with the selected launch skills."
  (interactive)
  (unless agent-shell-cockpit-skills--launch-workspace
    (user-error "No workspace is associated with this launch"))
  (let ((workspace agent-shell-cockpit-skills--launch-workspace)
        (selection agent-shell-cockpit-skills--launch-selection))
    (setq agent-shell-cockpit-skills--launch-workspace nil
          agent-shell-cockpit-skills--launch-selection nil)
    (agent-shell-cockpit-session-start-select
     workspace
     (agent-shell-cockpit-skills-compose selection workspace))))

(transient-define-prefix agent-shell-cockpit-skills-menu ()
  "Toggle launch skills, then choose and start an agent."
  :refresh-suffixes t
  ["Skills"
   :class transient-column
   :setup-children agent-shell-cockpit-skills--menu-children]
  ["Actions"
   ("RET" "Choose agent and start" agent-shell-cockpit-skills-start-agent)])

(defun agent-shell-cockpit-skills-launch (workspace)
  "Open the launch-skill menu for WORKSPACE."
  (setq agent-shell-cockpit-skills--launch-workspace workspace
        agent-shell-cockpit-skills--launch-selection
        (agent-shell-cockpit-skills--known-identifiers
         agent-shell-cockpit-default-skills))
  (transient-setup 'agent-shell-cockpit-skills-menu))

(provide 'agent-shell-cockpit-skills)

;;; agent-shell-cockpit-skills.el ends here
