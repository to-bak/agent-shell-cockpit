;;; agent-shell-cockpit-agent.el --- Shared Cockpit agent UI -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared agent rows, native agent-shell actions, and point-driven previews.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'agent-shell-cockpit-session)
(require 'agent-shell-cockpit-ui)

(declare-function agent-shell-cockpit-refresh "agent-shell-cockpit-ui")

(declare-function agent-shell-clear-buffer "agent-shell")
(declare-function agent-shell-copy-last-output "agent-shell")
(declare-function agent-shell-copy-session-id "agent-shell")
(declare-function agent-shell-cycle-session-mode "agent-shell")
(declare-function agent-shell-fork "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function agent-shell-open-transcript "agent-shell")
(declare-function agent-shell-reload "agent-shell")
(declare-function agent-shell-set-session-config-option "agent-shell")
(declare-function agent-shell-set-session-mode "agent-shell")
(declare-function agent-shell-set-session-model "agent-shell")
(declare-function agent-shell-set-session-thought-level "agent-shell")
(declare-function agent-shell-show-usage "agent-shell-usage")

(defcustom agent-shell-cockpit-agent-preview-width 0.4
  "Width of the temporary right-side agent preview window."
  :type 'number
  :group 'agent-shell-cockpit)

(defvar agent-shell-cockpit-agent--action-buffer nil
  "Live agent buffer targeted by the active action menu.")

(defvar-local agent-shell-cockpit-agent--preview-window nil
  "Temporary side window owned by the current Cockpit buffer.")

(defvar-local agent-shell-cockpit-agent--preview-buffer nil
  "Agent currently previewed from the current Cockpit buffer.")

(defun agent-shell-cockpit-agent-live-at-point-p ()
  "Return non-nil when point is on a live agent row."
  (memq (agent-shell-cockpit-ui-object-type-at-point)
        '(session workspace-session live-session)))

(defun agent-shell-cockpit-agent-buffer-at-point ()
  "Return the live agent buffer at point or signal a user error."
  (unless (agent-shell-cockpit-agent-live-at-point-p)
    (user-error "Point is not on a live agent"))
  (let ((buffer (agent-shell-cockpit-ui-object-at-point)))
    (unless (buffer-live-p buffer)
      (user-error "Agent buffer is no longer live"))
    buffer))

(defun agent-shell-cockpit-agent--identifier-name (identifier)
  "Return a readable agent name for IDENTIFIER."
  (capitalize
   (replace-regexp-in-string "[-_]+" " " (or identifier "agent"))))

(defun agent-shell-cockpit-agent--ordinal (workspace agent-id session-id)
  "Return SESSION-ID's one-based ordinal for AGENT-ID in WORKSPACE."
  (let ((ordinal 0)
        found)
    (dolist (session (map-elt workspace 'sessions))
      (when (equal (map-elt session 'agentId) agent-id)
        (setq ordinal (1+ ordinal))
        (when (equal (map-elt session 'sessionId) session-id)
          (setq found ordinal))))
    (or found (1+ ordinal))))

(defun agent-shell-cockpit-agent--name (agent-id session-id workspace)
  "Return a stable display name for AGENT-ID and SESSION-ID in WORKSPACE."
  (format "%s #%d"
          (agent-shell-cockpit-agent--identifier-name agent-id)
          (if workspace
              (agent-shell-cockpit-agent--ordinal
               workspace agent-id session-id)
            1)))

(defun agent-shell-cockpit-agent--status-column (status)
  "Return a fixed-width status column for STATUS."
  (let* ((label (string-trim-left
                 (agent-shell-cockpit-ui-status-label status)))
         (padding (max 1 (- 13 (string-width label)))))
    (concat label (make-string padding ?\s))))

(defun agent-shell-cockpit-agent--workspace-tag (workspace)
  "Return a compact tag for WORKSPACE, or the unassigned tag."
  (propertize
   (format "[%-12s] "
           (agent-shell-cockpit-ui-one-line
            (if workspace
                (or (map-elt workspace 'title) (map-elt workspace 'name))
              "unassigned")
            12))
   'face 'agent-shell-cockpit-secondary))

(defun agent-shell-cockpit-agent--heading
    (status name title &optional workspace-tag)
  "Return an agent heading from STATUS, NAME, TITLE, and WORKSPACE-TAG."
  (concat
   (agent-shell-cockpit-agent--status-column status)
   (or workspace-tag "")
   (propertize (format "%-18s "
                       (agent-shell-cockpit-ui-one-line name 17))
               'face 'default)
   (propertize (agent-shell-cockpit-ui-one-line title 42)
               'face 'agent-shell-cockpit-secondary)))

(defun agent-shell-cockpit-agent-insert-live
    (buffer workspace kind &optional show-workspace)
  "Insert live BUFFER in WORKSPACE as section KIND.
When SHOW-WORKSPACE is non-nil, include a workspace tag."
  (let* ((agent-id (with-current-buffer buffer
                     (agent-shell-cockpit-session--identifier)))
         (session-id (with-current-buffer buffer
                       (agent-shell-cockpit-session--session-id)))
         (record (and workspace
                      (agent-shell-cockpit-session-record buffer workspace)))
         (title (or (and record (map-elt record 'title))
                    (with-current-buffer buffer
                      (agent-shell-cockpit-session--title))))
         (name (agent-shell-cockpit-agent--name
                agent-id session-id workspace)))
    (magit-insert-section
        (agent-shell-cockpit-section buffer t :kind kind :object buffer)
      (magit-insert-heading
        (agent-shell-cockpit-agent--heading
         (agent-shell-cockpit-session-status buffer) name title
         (and show-workspace
              (agent-shell-cockpit-agent--workspace-tag workspace))))
      (magit-insert-section-body
        (agent-shell-cockpit-ui-insert-detail "Agent" (or agent-id "unknown"))
        (when session-id
          (agent-shell-cockpit-ui-insert-detail "Session" session-id))
        (agent-shell-cockpit-ui-insert-detail
         "Directory"
         (abbreviate-file-name
          (buffer-local-value 'default-directory buffer)))))))

(defun agent-shell-cockpit-agent-insert-history (session workspace)
  "Insert historical SESSION from WORKSPACE."
  (let* ((agent-id (map-elt session 'agentId))
         (session-id (map-elt session 'sessionId))
         (name (agent-shell-cockpit-agent--name
                agent-id session-id workspace)))
    (magit-insert-section
        (agent-shell-cockpit-section session-id t
                                     :kind 'session-history
                                     :object session)
      (magit-insert-heading
        (agent-shell-cockpit-agent--heading
         'history name (or (map-elt session 'title) session-id)))
      (magit-insert-section-body
        (agent-shell-cockpit-ui-insert-detail "Agent" agent-id)
        (agent-shell-cockpit-ui-insert-detail "Session" session-id)))))

(defun agent-shell-cockpit-agent-preview-close ()
  "Close the temporary agent preview owned by the current Cockpit buffer."
  (when (window-live-p agent-shell-cockpit-agent--preview-window)
    (delete-window agent-shell-cockpit-agent--preview-window))
  (setq agent-shell-cockpit-agent--preview-window nil
        agent-shell-cockpit-agent--preview-buffer nil))

(defun agent-shell-cockpit-agent-preview-update ()
  "Preview the live agent at point in a temporary right-side window."
  (let ((buffer (and (agent-shell-cockpit-agent-live-at-point-p)
                     (agent-shell-cockpit-ui-object-at-point))))
    (if (not (buffer-live-p buffer))
        (agent-shell-cockpit-agent-preview-close)
      (unless (and (eq buffer agent-shell-cockpit-agent--preview-buffer)
                   (window-live-p agent-shell-cockpit-agent--preview-window))
        (agent-shell-cockpit-agent-preview-close)
        (let ((window
               (display-buffer-in-side-window
                buffer
                `((side . right)
                  (slot . 99)
                  (window-width . ,agent-shell-cockpit-agent-preview-width)
                  (window-parameters
                   . ((agent-shell-cockpit-preview . t)))))))
          (setq agent-shell-cockpit-agent--preview-window window
                agent-shell-cockpit-agent--preview-buffer buffer)
          (set-window-point window
                            (with-current-buffer buffer (point-max))))))))

(define-minor-mode agent-shell-cockpit-agent-preview-mode
  "Preview the live agent at point in a right-side window."
  :lighter nil
  (if agent-shell-cockpit-agent-preview-mode
      (progn
        (add-hook 'post-command-hook
                  #'agent-shell-cockpit-agent-preview-update nil t)
        (add-hook 'kill-buffer-hook
                  #'agent-shell-cockpit-agent-preview-close nil t))
    (remove-hook 'post-command-hook
                 #'agent-shell-cockpit-agent-preview-update t)
    (remove-hook 'kill-buffer-hook
                 #'agent-shell-cockpit-agent-preview-close t)
    (agent-shell-cockpit-agent-preview-close)))

(defun agent-shell-cockpit-agent--invoke (command)
  "Invoke native agent-shell COMMAND in the selected agent buffer."
  (unless (buffer-live-p agent-shell-cockpit-agent--action-buffer)
    (user-error "Selected agent buffer is no longer live"))
  (agent-shell-cockpit-agent-preview-close)
  (with-current-buffer agent-shell-cockpit-agent--action-buffer
    (call-interactively command)))

(defmacro agent-shell-cockpit-agent--define-action (name command)
  "Define Cockpit action NAME delegating to native COMMAND."
  `(defun ,name ()
     ,(format "Run `%s' in the selected agent buffer." command)
     (interactive)
     (agent-shell-cockpit-agent--invoke #',command)))

(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-interrupt agent-shell-interrupt)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-fork agent-shell-fork)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-reload agent-shell-reload)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-cycle-mode agent-shell-cycle-session-mode)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-set-mode agent-shell-set-session-mode)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-set-model agent-shell-set-session-model)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-set-thought agent-shell-set-session-thought-level)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-set-option agent-shell-set-session-config-option)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-usage agent-shell-show-usage)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-copy-session-id agent-shell-copy-session-id)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-copy-output agent-shell-copy-last-output)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-open-transcript agent-shell-open-transcript)
(agent-shell-cockpit-agent--define-action
 agent-shell-cockpit-agent-clear agent-shell-clear-buffer)

(defun agent-shell-cockpit-agent--permission-available-p (key)
  "Return non-nil when the selected agent offers permission KEY."
  (and (buffer-live-p agent-shell-cockpit-agent--action-buffer)
       (agent-shell-cockpit-session-permission-action-available-p
        agent-shell-cockpit-agent--action-buffer key)))

(defun agent-shell-cockpit-agent--permission (key)
  "Invoke native permission KEY in the selected agent."
  (unless (buffer-live-p agent-shell-cockpit-agent--action-buffer)
    (user-error "Selected agent buffer is no longer live"))
  (agent-shell-cockpit-session-permission-action
   agent-shell-cockpit-agent--action-buffer key)
  (agent-shell-cockpit-refresh))

(defun agent-shell-cockpit-agent-permission-allow-once ()
  "Run the selected agent's native allow-once permission action."
  (interactive)
  (agent-shell-cockpit-agent--permission "y"))

(defun agent-shell-cockpit-agent-permission-allow-always ()
  "Run the selected agent's native always-allow permission action."
  (interactive)
  (agent-shell-cockpit-agent--permission "!"))

(defun agent-shell-cockpit-agent-permission-reject ()
  "Run the selected agent's native reject and interrupt action."
  (interactive)
  (agent-shell-cockpit-agent--permission "C-c C-c"))

(defun agent-shell-cockpit-agent-permission-view-diff ()
  "Run the selected agent's native permission-diff action."
  (interactive)
  (agent-shell-cockpit-agent--permission "v"))

(defun agent-shell-cockpit-agent-kill ()
  "Kill the live agent at point after confirmation."
  (interactive)
  (let ((buffer (agent-shell-cockpit-agent-buffer-at-point)))
    (when (yes-or-no-p (format "Kill agent session %s? "
                               (buffer-name buffer)))
      (with-current-buffer buffer
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer)))
      (agent-shell-cockpit-refresh))))

(transient-define-prefix agent-shell-cockpit-agent-actions-menu ()
  "Run native commands in a live agent-shell buffer."
  [["Permission"
    ("y" "Allow once" agent-shell-cockpit-agent-permission-allow-once
     :inapt-if-not
     (lambda () (agent-shell-cockpit-agent--permission-available-p "y")))
    ("!" "Always allow" agent-shell-cockpit-agent-permission-allow-always
     :inapt-if-not
     (lambda () (agent-shell-cockpit-agent--permission-available-p "!")))
    ("x" "Reject / interrupt" agent-shell-cockpit-agent-permission-reject
     :inapt-if-not
     (lambda ()
       (agent-shell-cockpit-agent--permission-available-p "C-c C-c")))
    ("d" "View diff" agent-shell-cockpit-agent-permission-view-diff
     :inapt-if-not
     (lambda () (agent-shell-cockpit-agent--permission-available-p "v")))]
   ["Control"
    ("i" "Interrupt" agent-shell-cockpit-agent-interrupt)
    ("f" "Fork" agent-shell-cockpit-agent-fork)
    ("R" "Reload" agent-shell-cockpit-agent-reload)]
   ["Session"
    ("m" "Cycle mode" agent-shell-cockpit-agent-cycle-mode)
    ("M" "Set mode" agent-shell-cockpit-agent-set-mode)
    ("v" "Set model" agent-shell-cockpit-agent-set-model)
    ("t" "Set thought level" agent-shell-cockpit-agent-set-thought)
    ("o" "Set option" agent-shell-cockpit-agent-set-option)]
   ["Inspect"
    ("u" "Usage" agent-shell-cockpit-agent-usage)
    ("c" "Copy session ID" agent-shell-cockpit-agent-copy-session-id)
    ("w" "Copy last output" agent-shell-cockpit-agent-copy-output)
    ("T" "Open transcript" agent-shell-cockpit-agent-open-transcript)
    ("C" "Clear buffer" agent-shell-cockpit-agent-clear)]])

(defun agent-shell-cockpit-agent-actions ()
  "Open the native action menu for the live agent at point."
  (interactive)
  (setq agent-shell-cockpit-agent--action-buffer
        (agent-shell-cockpit-agent-buffer-at-point))
  (transient-setup 'agent-shell-cockpit-agent-actions-menu))

(provide 'agent-shell-cockpit-agent)

;;; agent-shell-cockpit-agent.el ends here
