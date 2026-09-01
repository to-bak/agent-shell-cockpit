;;; agent-shell-cockpit-store-test.el --- Store tests -*- lexical-binding: t; -*-

(require 'agent-shell-cockpit-test-helper)

(ert-deftest agent-shell-cockpit-store-creates-round-trips-and-discovers ()
  (agent-shell-cockpit-test-with-root
    (let* ((workspace (agent-shell-cockpit-workspace-create
                       :name "alpha" :title "Alpha"))
           (loaded (agent-shell-cockpit-store-read (map-elt workspace 'root))))
      (should (equal (map-elt loaded 'title) "Alpha"))
      (should (equal (map-elt loaded 'name) "alpha"))
      (should (equal (map-elt loaded 'state) "active"))
      (should (equal (map-elt loaded 'sessions) nil))
      (should (equal (map-elt (car (agent-shell-cockpit-store-discover)) 'root)
                     (map-elt workspace 'root)))
      (with-temp-buffer
        (insert-file-contents
         (car (agent-shell-cockpit-workspace-prompt-paths loaded)))
        (should (equal (buffer-string) "#+TITLE: Alpha\n"))))))

(ert-deftest agent-shell-cockpit-workspace-defaults-title-to-directory-name ()
  (agent-shell-cockpit-test-with-root
    (let ((workspace (agent-shell-cockpit-workspace-create :name "alpha")))
      (should (equal (map-elt workspace 'title) "alpha"))
      (should (file-directory-p
               (agent-shell-cockpit-workspace-prompts-path workspace)))
      (should (equal
               (mapcar (lambda (path) (file-name-nondirectory path))
                       (agent-shell-cockpit-workspace-prompt-paths workspace))
               '("prompt.org"))))))

(ert-deftest agent-shell-cockpit-store-persists-only-schema-and-sessions ()
  (agent-shell-cockpit-test-with-root
    (let* ((workspace (agent-shell-cockpit-workspace-create
                       :name "alpha" :title "Alpha"))
           (root (map-elt workspace 'root))
           (path (agent-shell-cockpit-store-metadata-path root)))
      (agent-shell-cockpit-store-set workspace 'createdAt "discard me")
      (agent-shell-cockpit-store-set
       workspace 'sessions
       '(((agentId . "codex") (sessionId . "one") (title . "One")
          (lastSeenAt . "discard me"))))
      (agent-shell-cockpit-store-write workspace)
      (with-temp-buffer
        (insert-file-contents path)
        (let* ((json (json-parse-buffer :object-type 'alist
                                        :array-type 'list))
               (session (car (map-elt json 'sessions))))
          (should (equal (mapcar #'car json) '(schemaVersion sessions)))
          (should (equal (mapcar #'car session)
                         '(agentId sessionId title))))))))

(ert-deftest agent-shell-cockpit-store-exposes-invalid-workspace ()
  (agent-shell-cockpit-test-with-root
    (let ((root (expand-file-name "broken" agent-shell-cockpit-workspace-directory)))
      (make-directory (file-name-directory
                       (agent-shell-cockpit-store-metadata-path root)) t)
      (with-temp-file (agent-shell-cockpit-store-metadata-path root)
        (insert "{not json}"))
      (let ((record (car (agent-shell-cockpit-store-discover))))
        (should (eq (map-elt record 'kind) 'invalid))
        (should (stringp (map-elt record 'error)))))))

(ert-deftest agent-shell-cockpit-workspace-rejects-invalid-or-duplicate-name ()
  (agent-shell-cockpit-test-with-root
    (should-error (agent-shell-cockpit-workspace-create
                   :name "bad/name" :title "Bad") :type 'user-error)
    (agent-shell-cockpit-workspace-create :name "alpha" :title "Alpha")
    (should-error (agent-shell-cockpit-workspace-create
                   :name "alpha" :title "Again") :type 'user-error)))

(ert-deftest agent-shell-cockpit-workspace-rolls-back-failed-creation ()
  (agent-shell-cockpit-test-with-root
    (cl-letf (((symbol-function 'agent-shell-cockpit-store-write)
               (lambda (_) (error "simulated write failure"))))
      (should-error
       (agent-shell-cockpit-workspace-create :name "alpha" :title "Alpha")))
    (should-not (file-exists-p
                 (expand-file-name "alpha"
                                   agent-shell-cockpit-workspace-directory)))
    (should-not
     (seq-some
      (lambda (path)
        (string-prefix-p ".cockpit-create-"
                         (file-name-nondirectory path)))
      (directory-files agent-shell-cockpit-workspace-directory t
                       directory-files-no-dot-files-regexp)))))

(ert-deftest agent-shell-cockpit-store-rejects-unknown-schema ()
  (agent-shell-cockpit-test-with-root
    (let* ((workspace (agent-shell-cockpit-workspace-create
                       :name "alpha" :title "Alpha"))
           (path (agent-shell-cockpit-store-metadata-path
                  (map-elt workspace 'root))))
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (search-forward "\"schemaVersion\":2")
        (replace-match "\"schemaVersion\":99" t t)
        (write-region nil nil path nil 'silent))
      (should-error (agent-shell-cockpit-store-read
                     (map-elt workspace 'root)))
      (should (eq (map-elt (car (agent-shell-cockpit-store-discover)) 'kind)
                  'invalid)))))

(ert-deftest agent-shell-cockpit-workspace-repair-backs-up-invalid-metadata ()
  (agent-shell-cockpit-test-with-root
    (let* ((workspace (agent-shell-cockpit-workspace-create
                       :name "alpha" :title "Alpha"))
           (path (agent-shell-cockpit-store-metadata-path
                  (map-elt workspace 'root))))
      (with-temp-file path (insert "broken"))
      (let* ((invalid (car (agent-shell-cockpit-store-discover)))
             (repaired (agent-shell-cockpit-workspace-repair invalid)))
        (should (equal (map-elt repaired 'title) "Alpha"))
        (should (file-exists-p path))
        (should (seq-some
                 (lambda (candidate)
                   (string-prefix-p "workspace.json.backup-" candidate))
                 (directory-files (file-name-directory path))))))))

(provide 'agent-shell-cockpit-store-test)

;;; agent-shell-cockpit-store-test.el ends here
