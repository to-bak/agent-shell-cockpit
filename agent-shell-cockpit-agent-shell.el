;;; agent-shell-cockpit-agent-shell.el --- Native agent integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 to-bak
;; Author: to-bak
;; Assisted-by: Codex:GPT-6
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Native state and command boundary for sessions and agent controls.

;;; Code:

(require 'agent-shell)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

(defun agent-shell-cockpit-agent-shell-permission-position (buffer)
  "Return the latest native permission-button position in BUFFER."
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
        permission-position))))

(defun agent-shell-cockpit-agent-shell-state-value (path)
  "Return agent-shell's private state value at PATH."
  (when (boundp 'agent-shell--state)
    (map-nested-elt agent-shell--state path)))


(defun agent-shell-cockpit-agent-shell-ready-p ()
  "Return non-nil when a session exists and native initialization has settled.
Inspect in-flight requests without reevaluating user configuration functions."
  (and (agent-shell-cockpit-agent-shell-state-value '(:initialized))
       (agent-shell-cockpit-agent-shell-state-value '(:session :id))
       (not (seq-some
             (lambda (request)
               (member (map-elt request :method)
                       '("initialize" "authenticate" "session/new" "session/load"
                         "session/resume" "session/fork")))
             (agent-shell-cockpit-agent-shell-state-value '(:active-requests))))
       (not (agent-shell-cockpit-agent-shell-configuration-pending-p))))

(defun agent-shell-cockpit-agent-shell-configuration-pending-p ()
  "Return non-nil while a native setting request awaits its response."
  (seq-some (lambda (request)
              (member (map-elt request :method)
                      '("session/set_config_option" "session/set_mode" "session/set_model")))
            (agent-shell-cockpit-agent-shell-state-value '(:active-requests))))

(defun agent-shell-cockpit-agent-shell-set-resume-config (settings)
  "Prepare the current native buffer's configuration for restarting SETTINGS."
  (setf (map-elt agent-shell--state :agent-config)
        (agent-shell-cockpit-agent-shell-resume-config
         (map-elt agent-shell--state :agent-config) settings)))

(defun agent-shell-cockpit-agent-shell-config (identifier)
  "Return the native agent configuration named IDENTIFIER."
  (seq-find (lambda (config)
              (equal (format "%s" (map-elt config :identifier)) identifier))
            (agent-shell--resolved-agent-configs)))

(defun agent-shell-cockpit-agent-shell-settings ()
  "Return confirmed select-option IDs and values, with model first.
Include native model and mode IDs when no categorized option supplies them."
  (let ((options (or (agent-shell-cockpit-agent-shell-state-value '(:session :config-options))
                     (agent-shell-cockpit-agent-shell-state-value '(:config-options))))
        model mode other)
    (dolist (option options)
      (let ((id (map-elt option :id)) (value (map-elt option :current-value)))
        (when (and (equal (map-elt option :type) "select")
                   (stringp id) (not (string-empty-p id))
                   (stringp value) (not (string-empty-p value)))
          (let ((entry `((id . ,id) (value . ,value))))
            (pcase (or (map-elt option :category)
                       (and (member id '("model" "mode")) id))
              ("model" (setq model entry))
              ("mode" (setq mode entry))
              (_ (push entry other)))))))
    (dolist (spec '(("model" :model-id) ("mode" :mode-id)))
      (when-let* ((value (agent-shell-cockpit-agent-shell-state-value
                          (list :session (cadr spec))))
                  ((stringp value)) ((not (string-empty-p value))))
        (let ((entry `((id . ,(car spec)) (value . ,value))))
          (if (equal (car spec) "model")
              (unless model (setq model entry))
            (unless mode (setq mode entry))))))
    (append (delq nil (list model mode)) (nreverse other))))

(defun agent-shell-cockpit-agent-shell-resume-config (config settings)
  "Copy CONFIG with saved SETTINGS as its native initialization options.
Suppress launch defaults for this resume only.  Native initialization checks
each advertised value and reports refusals without blocking the session."
  (if (not settings) config
    (let ((copy (copy-tree config))
          (options (mapcar (lambda (entry)
                             (cons (map-elt entry 'id) (map-elt entry 'value)))
                           settings)))
      (setf (map-elt copy :default-model-id) nil
            (map-elt copy :default-session-mode-id) nil
            (map-elt copy :default-config-options) (lambda () options))
      copy)))

(defun agent-shell-cockpit-agent-shell-permission-choices (buffer)
  "Return native permission choices for the latest request in BUFFER.
Each choice contains a label, position marker, keymap and native command."
  (when-let* ((latest (agent-shell-cockpit-agent-shell-permission-position buffer)))
    (with-current-buffer buffer
      (let* ((latest-map (get-text-property latest 'keymap))
             (parent (keymap-parent latest-map))
             (position (point-min)) choices)
        (while (< position (point-max))
          (when (get-text-property position 'agent-shell-permission-button)
            (let* ((map (get-text-property position 'keymap))
                   (command (and map (lookup-key map (kbd "RET")))))
              (when (and (commandp command)
                         (or (eq map latest-map)
                             (and parent (eq (keymap-parent map) parent))))
                (push (list (or (get-text-property position 'help-echo) "Permission")
                            (copy-marker position) map command)
                      choices))))
          (setq position (next-single-property-change
                          position 'agent-shell-permission-button nil (point-max))))
        (nreverse choices)))))

(defun agent-shell-cockpit-agent-shell-invoke-choice (buffer choice)
  "Invoke a previously captured native permission CHOICE in BUFFER."
  (unless (buffer-live-p buffer) (user-error "Agent buffer is no longer live"))
  (pcase-let ((`(,_label ,marker ,map ,command) choice))
    (with-current-buffer buffer
      (unless (and (eq (marker-buffer marker) buffer)
                   (eq (get-text-property marker 'keymap) map)
                   (get-text-property marker 'agent-shell-permission-button)
                   (eq (lookup-key map (kbd "RET")) command)
                   (seq-some (lambda (current) (eq (nth 2 current) map))
                             (agent-shell-cockpit-agent-shell-permission-choices buffer)))
        (user-error "Permission request changed; reopen its actions"))
      (save-excursion
        (goto-char marker)
        (call-interactively command)))))

(provide 'agent-shell-cockpit-agent-shell)

;;; agent-shell-cockpit-agent-shell.el ends here
