;;; build-package.el --- Build the working source with the MELPA recipe -*- lexical-binding: t; -*-

;; Development-only dependency: package-build 5.0.2 (tested).
;; Build a disposable copy; never fetch, check out, or clean the real worktree.
(require 'package-build)
(let* ((source default-directory)
       (temporary (make-temp-file "cockpit-melpa-build-" t))
       (package-build-working-dir temporary)
       (package-build-recipes-dir (expand-file-name "recipes" source))
       (package-build-archive-dir
        (expand-file-name (or (getenv "COCKPIT_PACKAGE_OUTPUT") "dist") source))
       (package-build-badge-data nil)
       (working (expand-file-name "agent-shell-cockpit" temporary))
       (recipe (package-recipe-lookup "agent-shell-cockpit")))
  (unwind-protect
      (progn
        (make-directory working)
        (make-directory package-build-archive-dir t)
        (dolist (file (append (directory-files source t "\\`agent-shell-cockpit.*\\.el\\'")
                              (list (expand-file-name "README.org" source)
                                    (expand-file-name "LICENSE" source))))
          (copy-file file (expand-file-name (file-name-nondirectory file) working)))
        (oset recipe version
              (with-temp-buffer
                (insert-file-contents (expand-file-name "agent-shell-cockpit.el" source))
                (lm-header "version")))
        (oset recipe time (floor (float-time)))
        (let* ((default-directory (file-name-as-directory working))
               (files (package-build-expand-files-spec recipe t)))
          (princ (format "Building %d recipe files from the working source\n" (length files)))
          (package-build--build-package recipe files)))
    (delete-directory temporary t)))

;;; build-package.el ends here
