;;; agent-shell-cockpit-session.el --- Agent sessions for cockpit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak
;; Author: to-bak
;; Assisted-by: Codex:GPT-6

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Associate live agent-shell buffers and resumable ACP sessions with cockpit
;; workspaces.

;;; Code:

(require 'agent-shell-cockpit-agent-shell)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-cockpit-store)

(declare-function agent-shell--resolved-agent-configs "agent-shell")
(declare-function agent-shell-new-shell "agent-shell")
(declare-function agent-shell-start "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-status "agent-shell")
(declare-function agent-shell-cockpit "agent-shell-cockpit-dashboard")
(declare-function agent-shell-cockpit-workspace-view
                  "agent-shell-cockpit-workspace-view")
(declare-function agent-shell-cockpit-agent-preview-close
                  "agent-shell-cockpit-agent")
(defvar agent-shell--state)
(defvar agent-shell-context-sources)
(defvar agent-shell-cwd-function)
(defvar agent-shell-session-strategy)
(defvar agent-shell-cockpit--buffer)

(defvar-local agent-shell-cockpit-session-workspace-root nil
  "Canonical root of the cockpit workspace associated with this agent buffer.")

(defvar-local agent-shell-cockpit-session--subscription nil
  "Cockpit event subscription token for this agent buffer.")

(defvar-local agent-shell-cockpit-session-return-buffer nil
  "Cockpit buffer to revisit when leaving this managed agent session.")

(defvar-keymap agent-shell-cockpit-session-mode-map
  :doc "Keymap active in agent buffers managed by Cockpit."
  "C-c C-b" #'agent-shell-cockpit-session-return)

(define-minor-mode agent-shell-cockpit-session-mode
  "Mark the current agent buffer as managed by Cockpit."
  :lighter " Cockpit"
  :keymap agent-shell-cockpit-session-mode-map)

(defun agent-shell-cockpit-session-visit (buffer)
  "Visit agent BUFFER and remember the current Cockpit buffer."
  (unless (buffer-live-p buffer)
    (user-error "Agent buffer is no longer live"))
  (let ((origin (current-buffer)))
    (when (fboundp 'agent-shell-cockpit-agent-preview-close)
      (agent-shell-cockpit-agent-preview-close))
    (with-current-buffer buffer
      (unless (eq origin buffer)
        (setq agent-shell-cockpit-session-return-buffer origin))
      (agent-shell-cockpit-session-mode 1))
    (switch-to-buffer buffer)))

(defun agent-shell-cockpit-session-return ()
  "Return from a managed agent buffer to its Cockpit context."
  (interactive)
  (cond
   ((buffer-live-p agent-shell-cockpit-session-return-buffer)
    (switch-to-buffer agent-shell-cockpit-session-return-buffer))
   ((agent-shell-cockpit-session-workspace (current-buffer))
    (agent-shell-cockpit-workspace-view
     (agent-shell-cockpit-session-workspace (current-buffer))))
   ((buffer-live-p agent-shell-cockpit--buffer)
    (switch-to-buffer agent-shell-cockpit--buffer))
   (t (agent-shell-cockpit))))


(defun agent-shell-cockpit-session--identifier ()
  "Return the current agent buffer's agent identifier as a string."
  (when-let* ((identifier
               (agent-shell-cockpit-agent-shell-state-value
                '(:agent-config :identifier))))
    (format "%s" identifier)))

(defun agent-shell-cockpit-session--session-id ()
  "Return the current agent buffer's ACP session ID."
  (agent-shell-cockpit-agent-shell-state-value '(:session :id)))

(defun agent-shell-cockpit-session--title ()
  "Return the current agent buffer's session title."
  (or (agent-shell-cockpit-agent-shell-state-value '(:session :title))
      (buffer-name)))

(defun agent-shell-cockpit-session-workspace (buffer)
  "Return the active cockpit workspace associated with BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when agent-shell-cockpit-session-workspace-root
        (condition-case nil
            (agent-shell-cockpit-store-read
             agent-shell-cockpit-session-workspace-root)
          (error nil))))))

(defun agent-shell-cockpit-session-live-buffers (workspace)
  "Return live agent buffers associated with WORKSPACE."
  (when-let* ((workspace-root
               (file-name-as-directory
                (file-truename (map-elt workspace 'root)))))
    (seq-filter
     (lambda (buffer)
       (with-current-buffer buffer
         (and agent-shell-cockpit-session-workspace-root
              (equal (file-name-as-directory
                      (file-truename
                       agent-shell-cockpit-session-workspace-root))
                     workspace-root))))
     (seq-filter #'buffer-live-p (agent-shell-buffers)))))

(defun agent-shell-cockpit-session-unassigned-buffers ()
  "Return live agent buffers not associated with a cockpit workspace."
  (seq-filter
   (lambda (buffer)
     (not (agent-shell-cockpit-session-workspace buffer)))
   (seq-filter #'buffer-live-p (agent-shell-buffers))))

(defun agent-shell-cockpit-session-status (buffer)
  "Return cockpit status symbol for agent BUFFER."
  (pcase (ignore-errors (agent-shell-status :shell-buffer buffer))
    ('blocked 'attention)
    ('busy 'working)
    ('ready 'ready)
    (_ 'starting)))

(defun agent-shell-cockpit-session-record (buffer workspace)
  "Return BUFFER's persisted session record in WORKSPACE, when available."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((agent-id (agent-shell-cockpit-session--identifier))
            (session-id (agent-shell-cockpit-session--session-id)))
        (seq-find
         (lambda (session)
           (and (equal (map-elt session 'agentId) agent-id)
                (equal (map-elt session 'sessionId) session-id)))
         (map-elt workspace 'sessions))))))

(defun agent-shell-cockpit-session-forget (workspace session)
  "Forget SESSION from fresh WORKSPACE metadata."
  (agent-shell-cockpit-store-update
   (map-elt workspace 'root)
   (lambda (fresh)
     (agent-shell-cockpit-store-set
      fresh 'sessions
      (seq-remove (lambda (candidate)
                    (and (equal (map-elt candidate 'agentId) (map-elt session 'agentId))
                         (equal (map-elt candidate 'sessionId) (map-elt session 'sessionId))))
                  (map-elt fresh 'sessions))))))

(defvar agent-shell-cockpit-session-change-hook nil
  "Hook run when a native agent reports a state change.")

(defvar-local agent-shell-cockpit-session--restoring-settings nil
  "Non-nil until saved settings finish their initialization pipeline.")

(defvar-local agent-shell-cockpit-session--saved-state nil
  "Last successfully persisted state, used to avoid redundant writes.")

(defvar-local agent-shell-cockpit-session--settings-timer nil
  "Buffer-owned observer for native setters that emit no change event.")

(defun agent-shell-cockpit-session--owner (workspace agent-id session-id &optional exclude)
  "Find AGENT-ID and SESSION-ID in WORKSPACE, ignoring buffer EXCLUDE."
  (seq-find
   (lambda (buffer)
     (with-current-buffer buffer
       (and (not (eq buffer exclude))
            (equal agent-id (agent-shell-cockpit-session--identifier))
            (equal session-id (agent-shell-cockpit-session--session-id)))))
   (agent-shell-cockpit-session-live-buffers workspace)))

(defun agent-shell-cockpit-session--check-owner (workspace)
  "Reject a second live writer for the current session in WORKSPACE."
  (when-let* ((id (agent-shell-cockpit-session--session-id))
              (owner (agent-shell-cockpit-session--owner
                      workspace (agent-shell-cockpit-session--identifier) id (current-buffer))))
    (user-error "Session already managed by %s" (buffer-name owner))))

(defun agent-shell-cockpit-session--observe (buffer)
  "Persist confirmed changes in live managed BUFFER, without blocking timers."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (condition-case err
          (agent-shell-cockpit-session--upsert-current)
        (error (message "Cockpit could not save session: %s" (error-message-string err)))))))

(defun agent-shell-cockpit-session--upsert-current ()
  "Persist the current buffer's session using a fresh metadata transaction."
  (when-let* ((root agent-shell-cockpit-session-workspace-root)
              ((file-exists-p (agent-shell-cockpit-store-metadata-path root)))
              (agent-id (agent-shell-cockpit-session--identifier))
              (session-id (agent-shell-cockpit-session--session-id)))
    (let ((title (agent-shell-cockpit-session--title))
          (settings (agent-shell-cockpit-agent-shell-settings))
          (cwd (file-relative-name default-directory root)))
      (agent-shell-cockpit-session--check-owner `((root . ,root)))
      (let ((snapshot (list agent-id session-id title settings cwd
                            agent-shell-cockpit-session--restoring-settings)))
        (unless (equal snapshot agent-shell-cockpit-session--saved-state)
          (agent-shell-cockpit-store-update
           root
           (lambda (workspace)
             (let* ((sessions (map-elt workspace 'sessions))
                    (existing (seq-find
                               (lambda (item)
                                 (and (equal (map-elt item 'agentId) agent-id)
                                      (equal (map-elt item 'sessionId) session-id)))
                               sessions)))
               (unless existing
                 (setq existing `((agentId . ,agent-id) (sessionId . ,session-id)
                                  (displayId . ,(substring (secure-hash 'sha256
                                                                        (concat agent-id session-id)) 0 6))))
                 (agent-shell-cockpit-store-set workspace 'sessions
                                                (append sessions (list existing))))
               (agent-shell-cockpit-store-set existing 'title title)
               (unless agent-shell-cockpit-session--restoring-settings
                 (agent-shell-cockpit-store-set existing 'settings settings))
               (agent-shell-cockpit-store-set existing 'cwd cwd))))
          (setq agent-shell-cockpit-session--saved-state (copy-tree snapshot)))))))

(defun agent-shell-cockpit-session--on-event (event)
  "Handle an agent-shell EVENT for an attached buffer."
  (when (and agent-shell-cockpit-session-workspace-root
             (memq (map-elt event :event) '(init-session session-restored init-finished)))
    (condition-case err
        (agent-shell-cockpit-session--check-owner
         `((root . ,agent-shell-cockpit-session-workspace-root)))
      (user-error
       (setq agent-shell-cockpit-session-workspace-root nil)
       (agent-shell-cockpit-session--unsubscribe)
       (message "Cockpit detached duplicate session: %s" (error-message-string err)))))
  (pcase (map-elt event :event)
    ('init-finished
     (setq agent-shell-cockpit-session--restoring-settings nil)
     (agent-shell-cockpit-session--upsert-current))
    ('config-option-update
     (agent-shell-cockpit-session--upsert-current))
    ((or 'init-session 'session-restored 'session-title-changed 'turn-complete)
     (agent-shell-cockpit-session--upsert-current))
    ('clean-up (agent-shell-cockpit-session--finish)))
  (when (memq (map-elt event :event)
              '(init-finished init-session session-restored session-title-changed
                              config-option-update turn-complete clean-up permission-request
                              permission-response input-submitted))
    (run-hooks 'agent-shell-cockpit-session-change-hook)))

(defun agent-shell-cockpit-session--subscribe ()
  "Subscribe the current agent buffer to cockpit persistence events."
  (setq-local agent-shell-cwd-function (let ((directory default-directory)) (lambda () directory)))
  (unless agent-shell-cockpit-session--subscription
    (setq agent-shell-cockpit-session--subscription
          (agent-shell-subscribe-to
           :shell-buffer (current-buffer)
           :on-event #'agent-shell-cockpit-session--on-event)))
  (when (and agent-shell-cockpit-session-workspace-root
             (not (timerp agent-shell-cockpit-session--settings-timer)))
    (setq agent-shell-cockpit-session--settings-timer
          (run-with-timer 2 2 #'agent-shell-cockpit-session--observe (current-buffer))))
  (add-hook 'kill-buffer-hook #'agent-shell-cockpit-session--finish nil t)
  (add-hook 'change-major-mode-hook #'agent-shell-cockpit-session--finish nil t))

(defun agent-shell-cockpit-session--unsubscribe ()
  "Release the current buffer's native event subscription."
  (when (timerp agent-shell-cockpit-session--settings-timer)
    (cancel-timer agent-shell-cockpit-session--settings-timer))
  (setq agent-shell-cockpit-session--settings-timer nil)
  (when agent-shell-cockpit-session--subscription
    (agent-shell-unsubscribe :subscription agent-shell-cockpit-session--subscription)
    (setq agent-shell-cockpit-session--subscription nil)))

(defun agent-shell-cockpit-session--finish ()
  "Save while native state still exists, then release resources unconditionally."
  (unwind-protect
      (agent-shell-cockpit-session--observe (current-buffer))
    (agent-shell-cockpit-session--unsubscribe)))

(defun agent-shell-cockpit-session-attach (buffer workspace)
  "Attach compatible agent BUFFER to WORKSPACE and return BUFFER."
  (unless (buffer-live-p buffer)
    (user-error "Agent buffer is no longer live"))
  (with-current-buffer buffer
    (unless (file-in-directory-p (file-truename default-directory)
                                 (file-truename (map-elt workspace 'root)))
      (user-error "Agent CWD is outside workspace %s"
                  (map-elt workspace 'name)))
    (agent-shell-cockpit-session--check-owner workspace)
    (setq agent-shell-cockpit-session-workspace-root
          (file-name-as-directory
           (file-truename (map-elt workspace 'root))))
    (agent-shell-cockpit-session-mode 1)
    (agent-shell-cockpit-session--subscribe)
    (agent-shell-cockpit-session--upsert-current))
  buffer)

(defun agent-shell-cockpit-session--new-buffer (before result)
  "Return newly created shell buffer from BEFORE buffers and RESULT."
  (or (and (bufferp result) result)
      (seq-find (lambda (buffer) (not (memq buffer before)))
                (agent-shell-buffers))))

(defun agent-shell-cockpit-session-start (workspace command &optional initial-input)
  "Start COMMAND at WORKSPACE root with optional INITIAL-INPUT.
Attach the resulting agent buffer to WORKSPACE."
  (agent-shell-cockpit-session-target (map-elt workspace 'root) workspace)
  (unless (commandp command)
    (user-error "Agent command is not interactive: %S" command))
  (let ((origin (current-buffer))
        (before (agent-shell-buffers))
        (default-directory (map-elt workspace 'root)))
    (let ((agent-shell-cwd-function (let ((directory default-directory)) (lambda () directory)))
          (agent-shell-context-sources
           (and initial-input (list (lambda () initial-input)))))
      (let* ((result (call-interactively command))
             (buffer (agent-shell-cockpit-session--new-buffer before result)))
        (unless buffer
          (user-error "Agent command did not create a shell buffer"))
        (agent-shell-cockpit-session-attach buffer workspace)
        (with-current-buffer buffer
          (setq agent-shell-cockpit-session-return-buffer origin))
        buffer))))

(defun agent-shell-cockpit-session-start-select (workspace &optional initial-input)
  "Prompt for and start an agent for WORKSPACE with optional INITIAL-INPUT."
  (let ((agent-shell-session-strategy 'new))
    (agent-shell-cockpit-session-start
     workspace #'agent-shell-new-shell initial-input)))


(defcustom agent-shell-cockpit-session-restore-verbosity nil
  "History displayed on resume, or nil to use agent-shell's setting.
The value `ask' prompts on every resume.  A prefix argument when opening
a history row also prompts, regardless of this setting.  This controls
displayed history, not the agent's remembered conversation."
  :type '(choice (const :tag "Use agent-shell setting" nil)
                 (const ask) (const minimal) (const last)
                 (const first-last) (const full))
  :group 'agent-shell-cockpit)

(defvar agent-shell-session-restore-verbosity)

(defun agent-shell-cockpit-session-resume (workspace session)
  "Resume SESSION inside WORKSPACE and return the new live buffer."
  (agent-shell-cockpit-session-target (map-elt workspace 'root) workspace)
  (when (equal (map-elt workspace 'state) "archived")
    (user-error "Archived workspaces cannot resume sessions"))
  (when-let* ((owner (agent-shell-cockpit-session--owner
                      workspace (map-elt session 'agentId) (map-elt session 'sessionId))))
    (user-error "Session is already live in %s" (buffer-name owner)))
  (let ((origin (current-buffer))
        (config (agent-shell-cockpit-agent-shell-config
                 (map-elt session 'agentId)))
        (default-directory
         (expand-file-name (or (map-elt session 'cwd) ".") (map-elt workspace 'root))))
    (unless (and (file-directory-p default-directory)
                 (file-in-directory-p default-directory (map-elt workspace 'root)))
      (user-error "Session working directory is missing or outside its workspace"))
    (unless config
      (user-error "Agent configuration is unavailable: %s"
                  (map-elt session 'agentId)))
    (let* ((verbosity
            (if (or current-prefix-arg
                    (eq agent-shell-cockpit-session-restore-verbosity 'ask))
                (intern (completing-read
                         "Display restored history: "
                         '("minimal" "last" "first-last" "full") nil t nil nil
                         (symbol-name agent-shell-session-restore-verbosity)))
              (or agent-shell-cockpit-session-restore-verbosity
                  agent-shell-session-restore-verbosity)))
           (agent-shell-session-restore-verbosity verbosity)
           (agent-shell-cwd-function (let ((directory default-directory)) (lambda () directory)))
           (buffer (agent-shell-start
                    :config (agent-shell-cockpit-agent-shell-resume-config
                             config (map-elt session 'settings))
                    :session-id (map-elt session 'sessionId))))
      (with-current-buffer buffer
        (setq agent-shell-cockpit-session--restoring-settings
              (and (map-elt session 'settings)
                   (not (agent-shell-cockpit-agent-shell-state-value '(:set-config-options))))))
      (agent-shell-cockpit-session-attach buffer workspace)
      (with-current-buffer buffer
        ;; Replay continues asynchronously after `agent-shell-start' returns.
        (setq-local agent-shell-session-restore-verbosity verbosity)
        (setq agent-shell-cockpit-session-return-buffer origin))
      buffer)))

(defvar-local agent-shell-cockpit-session-standalone-p nil
  "Non-nil for a Cockpit-launched standalone agent.")

(defun agent-shell-cockpit-session-target (directory &optional workspace)
  "Validate DIRECTORY and return its launch target with optional WORKSPACE."
  (when (file-remote-p directory) (user-error "Remote targets are not supported"))
  (unless (file-directory-p directory) (user-error "Directory does not exist"))
  (when workspace
    (setq workspace (agent-shell-cockpit-store-read (map-elt workspace 'root)))
    (unless (and (equal (map-elt workspace 'state) "active")
                 (not (map-elt workspace 'operation)))
      (user-error "Restore or recover the workspace before starting agents")))
  `((directory . ,(file-name-as-directory (file-truename directory)))
    (workspace . ,workspace)))

(defun agent-shell-cockpit-session-start-target (target &optional input)
  "Start a native agent for TARGET with optional editable INPUT."
  (if-let* ((workspace (map-elt target 'workspace)))
      (agent-shell-cockpit-session-start-select workspace input)
    (let* ((agent-shell-session-strategy 'new)
           (origin (current-buffer))
           (default-directory (map-elt target 'directory))
           (agent-shell-cwd-function (let ((directory default-directory)) (lambda () directory)))
           (agent-shell-context-sources (and input (list (lambda () input))))
           (before (agent-shell-buffers))
           (buffer (agent-shell-cockpit-session--new-buffer
                    before (call-interactively #'agent-shell-new-shell))))
      (unless (buffer-live-p buffer) (user-error "Agent did not create a buffer"))
      (with-current-buffer buffer
        (setq agent-shell-cockpit-session-standalone-p t
              agent-shell-cockpit-session-return-buffer origin)
        (agent-shell-cockpit-session-mode 1)
        (agent-shell-cockpit-session--subscribe))
      buffer)))

(defun agent-shell-cockpit-session-buffers-in-directory (directory)
  "Return all native agents working inside DIRECTORY, regardless of membership."
  (seq-filter
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (file-in-directory-p (buffer-local-value 'default-directory buffer)
                               directory)))
   (agent-shell-buffers)))

(defun agent-shell-cockpit-session-invoke (buffer command)
  "Run native COMMAND in BUFFER, adopting replacement and child buffers."
  (unless (buffer-live-p buffer) (user-error "Agent buffer is no longer live"))
  (let ((before (agent-shell-buffers))
        (workspace (agent-shell-cockpit-session-workspace buffer))
        (origin (buffer-local-value 'agent-shell-cockpit-session-return-buffer buffer))
        (standalone (buffer-local-value 'agent-shell-cockpit-session-standalone-p buffer))
        (restart (memq command '(agent-shell-reload agent-shell-fork)))
        settings)
    (when restart
      (with-current-buffer buffer
        (when agent-shell-cockpit-session--restoring-settings
          (user-error "Wait for session initialization before restarting"))
        (setq settings (agent-shell-cockpit-agent-shell-settings))
        (agent-shell-cockpit-session--upsert-current)
        (agent-shell-cockpit-agent-shell-set-resume-config settings)))
    (unwind-protect
        (with-current-buffer buffer (call-interactively command))
      (dolist (new (seq-difference (agent-shell-buffers) before))
        (with-current-buffer new
          (setq agent-shell-cockpit-session--restoring-settings
                (and restart settings
                     (not (agent-shell-cockpit-agent-shell-state-value '(:set-config-options))))))
        (if workspace
            (agent-shell-cockpit-session-attach new workspace)
          (with-current-buffer new
            (setq agent-shell-cockpit-session-standalone-p standalone)
            (agent-shell-cockpit-session-mode 1)
            (agent-shell-cockpit-session--subscribe)))
        (with-current-buffer new
          (setq agent-shell-cockpit-session-return-buffer origin))))))

(provide 'agent-shell-cockpit-session)

;;; agent-shell-cockpit-session.el ends here
