(setq make-backup-files nil
      user-emacs-directory (file-name-directory
                            (file-truename (or load-file-name buffer-file-name)))
      comp-eln-load-path `(,(concat user-emacs-directory "/eln-cache")))

(let ((inhibit-message t))
  (load-file (concat user-emacs-directory "/bootstrap-common.el")))

; The use of color-theme is deprecated, but couldn't figure out how to make
; enable-theme apply in batch mode
; Seems I'm not the only one:
; https://emacs.stackexchange.com/questions/25009/loading-a-theme-and-fontifying-in-batch-mode
(use-package color-theme :straight t)
(use-package color-theme-solarized
  :straight (solarized :type git :host github
                       :repo "tohojo/emacs-color-theme-solarized")
  :config
  (color-theme-solarized-dark))

(use-package org-re-reveal :straight t
             :config (setq org-re-reveal-plugins '(markdown notes zoom)))


(straight-do-thaw)

(defun export-slides-file (filename)
  "Export slides from FILENAME using org-reveal."
  (let ((enable-local-variables :all))
    (with-current-buffer
        (find-file filename)
      (org-re-reveal-export-to-html))))
