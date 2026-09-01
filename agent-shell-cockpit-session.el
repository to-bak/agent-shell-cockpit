;;; agent-shell-cockpit-session.el --- Agent sessions for cockpit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Associate live agent-shell buffers and resumable ACP sessions with cockpit
;; workspaces.

;;; Code:

(require 'agent-shell)
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
(defvar agent-shell--state)
(defvar agent-shell-cockpit--buffer)

(defcustom agent-shell-cockpit-default-command #'agent-shell-new-shell
  "Interactive command used to start a workspace's default agent."
  :type 'function
  :group 'agent-shell-cockpit)

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
    (with-current-buffer buffer
      (setq agent-shell-cockpit-session-return-buffer origin)
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

(defun agent-shell-cockpit-session-allow-once (buffer)
  "Allow the latest pending permission request in agent BUFFER once.

This invokes agent-shell's own permission action without displaying BUFFER."
  (unless (buffer-live-p buffer)
    (user-error "Agent buffer is no longer live"))
  (with-current-buffer buffer
    (save-excursion
      (let ((position (point-min))
            permission-position)
        (while (< position (point-max))
          (when (get-text-property
                 position 'agent-shell-permission-button)
            (setq permission-position position))
          (setq position
                (or (next-single-property-change
                     position 'agent-shell-permission-button
                     nil (point-max))
                    (point-max))))
        (let* ((match permission-position)
             (command
              (when match
                (goto-char match)
                (key-binding (kbd "y") t))))
          (unless (commandp command)
            (user-error "Agent has no pending permission request"))
          (call-interactively command))))))

(defun agent-shell-cockpit-session--state-value (path)
  "Return agent-shell's private state value at PATH.
All compatibility-sensitive state access is isolated in this function."
  (when (boundp 'agent-shell--state)
    (map-nested-elt agent-shell--state path)))

(defun agent-shell-cockpit-session--identifier ()
  "Return the current agent buffer's agent identifier as a string."
  (when-let* ((identifier
              (agent-shell-cockpit-session--state-value
               '(:agent-config :identifier))))
    (symbol-name identifier)))

(defun agent-shell-cockpit-session--session-id ()
  "Return the current agent buffer's ACP session ID."
  (agent-shell-cockpit-session--state-value '(:session :id)))

(defun agent-shell-cockpit-session--title ()
  "Return the current agent buffer's session title."
  (or (agent-shell-cockpit-session--state-value '(:session :title))
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

(defun agent-shell-cockpit-session--upsert-current ()
  "Persist the current agent buffer's session in its workspace."
  (when-let* ((workspace-root agent-shell-cockpit-session-workspace-root)
              (workspace (condition-case nil
                             (agent-shell-cockpit-store-read workspace-root)
                           (error nil)))
              (agent-id (agent-shell-cockpit-session--identifier))
              (session-id (agent-shell-cockpit-session--session-id)))
    (let* ((sessions (map-elt workspace 'sessions))
           (existing
            (seq-find
             (lambda (session)
               (and (equal (map-elt session 'agentId) agent-id)
                    (equal (map-elt session 'sessionId) session-id)))
             sessions)))
      (if existing
          (agent-shell-cockpit-store-set
           existing 'title (agent-shell-cockpit-session--title))
        (agent-shell-cockpit-store-set
         workspace 'sessions
         (append
          sessions
          (list (list (cons 'agentId agent-id)
                      (cons 'sessionId session-id)
                      (cons 'title (agent-shell-cockpit-session--title)))))))
      (agent-shell-cockpit-store-write workspace))))

(defun agent-shell-cockpit-session--on-event (event)
  "Handle an agent-shell EVENT for an attached buffer."
  (pcase (map-elt event :event)
    ((or 'init-session 'session-restored 'session-title-changed 'turn-complete)
     (agent-shell-cockpit-session--upsert-current))
    ('clean-up
     (setq agent-shell-cockpit-session--subscription nil))))

(defun agent-shell-cockpit-session--subscribe ()
  "Subscribe the current agent buffer to cockpit persistence events."
  (unless agent-shell-cockpit-session--subscription
    (setq agent-shell-cockpit-session--subscription
          (agent-shell-subscribe-to
           :shell-buffer (current-buffer)
           :on-event #'agent-shell-cockpit-session--on-event))))

(defun agent-shell-cockpit-session-attach (buffer workspace)
  "Attach compatible agent BUFFER to WORKSPACE and return BUFFER."
  (unless (buffer-live-p buffer)
    (user-error "Agent buffer is no longer live"))
  (with-current-buffer buffer
    (unless (file-in-directory-p (file-truename default-directory)
                                 (file-truename (map-elt workspace 'root)))
      (user-error "Agent CWD is outside workspace %s"
                  (map-elt workspace 'name)))
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

(defun agent-shell-cockpit-session-start (workspace command)
  "Start COMMAND at WORKSPACE root and attach the resulting agent buffer."
  (unless (commandp command)
    (user-error "Agent command is not interactive: %S" command))
  (let ((origin (current-buffer))
        (before (agent-shell-buffers))
        (default-directory (map-elt workspace 'root)))
    (let* ((result (call-interactively command))
           (buffer (agent-shell-cockpit-session--new-buffer before result)))
      (unless buffer
        (user-error "Agent command did not create a shell buffer"))
      (agent-shell-cockpit-session-attach buffer workspace)
      (with-current-buffer buffer
        (setq agent-shell-cockpit-session-return-buffer origin))
      buffer)))

(defun agent-shell-cockpit-session-start-default (workspace)
  "Start the configured default agent for WORKSPACE."
  (agent-shell-cockpit-session-start
   workspace agent-shell-cockpit-default-command))

(defun agent-shell-cockpit-session-start-select (workspace)
  "Prompt for and start an agent for WORKSPACE."
  (agent-shell-cockpit-session-start workspace #'agent-shell-new-shell))

(defun agent-shell-cockpit-session--config (identifier)
  "Return agent configuration identified by IDENTIFIER."
  (seq-find
   (lambda (config)
     (equal (symbol-name (map-elt config :identifier)) identifier))
   (agent-shell--resolved-agent-configs)))

(defun agent-shell-cockpit-session-resume (workspace session)
  "Resume SESSION inside WORKSPACE and return the new live buffer."
  (when (equal (map-elt workspace 'state) "archived")
    (user-error "Archived workspaces cannot resume sessions"))
  (let ((origin (current-buffer))
        (config (agent-shell-cockpit-session--config
                 (map-elt session 'agentId)))
        (default-directory (map-elt workspace 'root)))
    (unless config
      (user-error "Agent configuration is unavailable: %s"
                  (map-elt session 'agentId)))
    (let ((buffer (agent-shell-start
                   :config config :session-id (map-elt session 'sessionId))))
      (agent-shell-cockpit-session-attach buffer workspace)
      (with-current-buffer buffer
        (setq agent-shell-cockpit-session-return-buffer origin))
      buffer)))

(provide 'agent-shell-cockpit-session)

;;; agent-shell-cockpit-session.el ends here
