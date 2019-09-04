(setq user-emacs-directory (file-name-directory
                            (file-truename (or load-file-name buffer-file-name)))
      make-backup-files nil
      color-theme-obsolete nil)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (find-file (expand-file-name "install-straight.el" user-emacs-directory))
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)


; The use of color-theme is deprecated, but couldn't figure out how to make
; enable-theme apply in batch mode
; Seems I'm not the only one:
; https://emacs.stackexchange.com/questions/25009/loading-a-theme-and-fontifying-in-batch-mode
(use-package color-theme
  :straight t)

(use-package color-theme-solarized
 :straight (solarized :type git :host github
                      :repo "tohojo/emacs-color-theme-solarized"
                      :upstream (:host github
                                       :repo "sellout/emacs-color-theme-solarized"))
 :config
 (color-theme-solarized-dark))

(use-package git
  :straight t)

(use-package org-version
  :requires git
  :load-path "lisp")

(use-package org
  :straight t)

(use-package org-re-reveal
  :straight t
  :after org
  :config
  (setq org-re-reveal-plugins '(markdown notes zoom)))

(use-package htmlize
  :straight t)

; Make sure we use the versions specified in versions/default.el
(straight-thaw-versions)

(defun silence-messages (orig-fun &rest r)
  "Silence messages from ORIG-FUN with args R."
  (let ((inhibit-message t))
    (apply orig-fun r)))
(advice-add 'sh-set-shell :around #'silence-messages)


(defun export-slides-file (filename)
  "Export slides from FILENAME using org-reveal."
  (let ((enable-local-variables :all))
    (with-current-buffer
        (find-file filename)
      (org-re-reveal-export-to-html))))
