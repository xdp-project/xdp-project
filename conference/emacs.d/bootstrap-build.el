(setq user-emacs-directory (file-name-directory
                            (file-truename (or load-file-name buffer-file-name)))
      make-backup-files nil)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (find-file "install-straight.el" user-emacs-directory)
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

(use-package ox-reveal
  :straight t
  :after org)

(use-package htmlize
  :straight t)


(defun export-slides-file (filename)
  (let ((enable-local-variables :all))
    (with-current-buffer
        (find-file filename)
      (org-reveal-export-to-html))))
