(setq user-emacs-directory (file-name-directory
                            (file-truename (or load-file-name buffer-file-name))))

(let ((inhibit-message t))
  (load-file (concat user-emacs-directory "/bootstrap-common.el")))

(straight-do-thaw)

(require 'ox-publish)
(require 'org-id)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "DELEGATED(D@)")
        (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|"  "CANCELLED(c@/!)" "MEETING")))

(setq export-html-head-readtheorg
      (concat
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"/styles/readtheorg/css/htmlize.css\"/>\n"
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"/styles/readtheorg/css/readtheorg.css\"/>\n"
       "<script type=\"text/javascript\" src=\"/styles/lib/js/jquery.min.js\"></script>\n"
       "<script type=\"text/javascript\" src=\"/styles/lib/js/bootstrap.min.js\"></script>\n"
       "<script type=\"text/javascript\" src=\"/styles/lib/js/jquery.stickytableheaders.min.js\"></script>\n"
       "<script type=\"text/javascript\" src=\"/styles/readtheorg/js/readtheorg.js\"></script>\n")
      export-html-head-bigblow
      (concat
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"/styles/bigblow/css/htmlize.css\"/>\n"
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"/styles/bigblow/css/bigblow.css\"/>\n"
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"/styles/bigblow/css/hideshow.css\"/>\n"
       "<script type=\"text/javascript\" src=\"/styles/bigblow/js/jquery-1.11.0.min.js\"></script>\n"
       "<script type=\"text/javascript\" src=\"/styles/bigblow/js/jquery-ui-1.10.2.min.js\"></script>\n"
       "<script type=\"text/javascript\" src=\"/styles/bigblow/js/jquery.localscroll-min.js\"></script>\n"
       "<script type=\"text/javascript\" src=\"/styles/bigblow/js/jquery.scrollTo-1.4.3.1-min.js\"></script>\n"
       "<script type=\"text/javascript\" src=\"/styles/bigblow/js/jquery.zclip.min.js\"></script>\n"
       "<script type=\"text/javascript\" src=\"/styles/bigblow/js/bigblow.js\"></script>\n"
       "<script type=\"text/javascript\" src=\"/styles/bigblow/js/hideshow.js\"></script>\n"
       "<script type=\"text/javascript\" src=\"/styles/lib/js/jquery.stickytableheaders.min.js\"></script>\n"))

(defun sitemap-func (title list)
  "Default site map, as a string.
TITLE is the title of the site map.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (org-list-to-org (-filter (lambda (entry) (or (not (listp entry))
                                                (not (string-match-p "SKIP" (car entry)))))
                            list)))

(defun sitemap-entry (entry style project)
  "Default format for site map ENTRY, as a string.
ENTRY is a file name.  STYLE is the style of the sitemap.
PROJECT is the current project."
  (cond ((not (directory-name-p entry))
         (if (org-publish-find-property entry :with-planning project)
             "SKIP"
           (format "[[file:%s][%s]]"
                   entry
                   (org-publish-find-title entry project))))
	((eq style 'tree)
	 ;; Return only last subdir.
	 (file-name-nondirectory (directory-file-name entry)))
	(t entry)))


(defun export-repo (destdir)
  "Export repository to DESTDIR using org-publish-project."
  (let* ((enable-local-variables :all)
         (destdir (file-truename destdir))
         (project
          `("xdp-project"
            :base-directory ,(file-truename (concat user-emacs-directory "/../"))
            :publishing-directory ,destdir
            :publishing-function org-html-publish-to-html
            :exclude "emacs\\.d\\|README\\|conference/\\|styles/"
            :exclude-tags ("noexport")
            :with-broken-links t
            :with-sub-superscript nil
            :section-numbers nil
            :headline-levels 3
            :with-drawers nil
            :with-tags nil
            :with-toc t
            :html-head-include-default-style nil
            :html-head-include-scripts nil
            :html-head-extra ,export-html-head-readtheorg
            :recursive t
            :auto-sitemap t
            :sitemap-title "The XDP project"
            :sitemap-filename "sitemap.org"
            :sitemap-function sitemap-func
            :sitemap-format-entry sitemap-entry
            :htmlized-source nil))
         (styles
          `("styles"
            :base-directory ,(file-truename (concat user-emacs-directory "/../styles/"))
            :publishing-directory ,(file-truename (concat destdir "/styles"))
            :base-extension "js\\|css"
            :publishing-function org-publish-attachment
            :recursive t))
         (org-publish-project-alist (list project styles))
         (org-id-track-globally t)
         (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
         (org-id-locations-file "/dev/null")
         (org-id-extra-files (org-publish-get-base-files project)))
    (org-publish-all t)))

(provide 'export-repo)
