;;; checkdoc-runner.el --- Fail the build on documentation warnings -*- lexical-binding: t; -*-
(require 'checkdoc)
(let* ((failed nil)
       (observer (lambda (&rest _) (setq failed t))))
  (advice-add 'checkdoc-error :before observer)
  (unwind-protect
      (mapc #'checkdoc-file command-line-args-left)
    (advice-remove 'checkdoc-error observer))
  (kill-emacs (if failed 1 0)))
;;; checkdoc-runner.el ends here
