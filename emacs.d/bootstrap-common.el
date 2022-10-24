(setq make-backup-files nil
      color-theme-obsolete nil
      load-prefer-newer t)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (find-file (expand-file-name "install-straight.el" user-emacs-directory))
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(let ((inhibit-message t))
  (straight-use-package 'auto-compile)
  (auto-compile-on-load-mode))

(straight-use-package 'use-package)

(defun silence-messages (orig-fun &rest r)
  "Silence messages from ORIG-FUN with args R."
  (let ((inhibit-message t))
    (apply orig-fun r)))
(advice-add 'sh-set-shell :around #'silence-messages)

(defun silent-straight-print (orig-fun &rest r)
  "straight--output that obeys inhibit-message"
  (unless inhibit-message
    (apply orig-fun r)))
(advice-add 'straight--output :around #'silent-straight-print)

(defun straight-do-thaw ()
  "Make sure we use the straight package versions specified in versions/default.el."
  (let ((inhibit-message t))
    (straight-thaw-versions)
    (straight--save-build-cache)))

(use-package git :straight t)
(use-package org :straight t)
(use-package htmlize :straight t)

(use-package dash :straight t)
