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


; Useful HTML anchors - from unpackaged.el:
; https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
(defun unpackaged/org-export-get-reference (datum info)
  "Like `org-export-get-reference', except uses heading titles instead of random numbers."
  (let ((cache (plist-get info :internal-references)))
    (or (car (rassq datum cache))
        (let* ((crossrefs (plist-get info :crossrefs))
               (cells (org-export-search-cells datum))
               ;; Preserve any pre-existing association between
               ;; a search cell and a reference, i.e., when some
               ;; previously published document referenced a location
               ;; within current file (see
               ;; `org-publish-resolve-external-link').
               ;;
               ;; However, there is no guarantee that search cells are
               ;; unique, e.g., there might be duplicate custom ID or
               ;; two headings with the same title in the file.
               ;;
               ;; As a consequence, before re-using any reference to
               ;; an element or object, we check that it doesn't refer
               ;; to a previous element or object.
               (new (or (cl-some
                         (lambda (cell)
                           (let ((stored (cdr (assoc cell crossrefs))))
                             (and (not (assoc stored cache)) stored)))
                         cells)
                        (when (org-element-property :raw-value datum)
                          ;; Heading with a title
                          (unpackaged/org-export-new-title-reference datum cache))
                        ;; NOTE: This probably breaks some Org Export
                        ;; feature, but if it does what I need, fine.
                        (org-export-format-reference
                         (org-export-new-reference cache))))
               (reference-string new))
          ;; Cache contains both data already associated to
          ;; a reference and in-use internal references, so as to make
          ;; unique references.
          (dolist (cell cells) (push (cons cell new) cache))
          ;; Retain a direct association between reference string and
          ;; DATUM since (1) not every object or element can be given
          ;; a search cell (2) it permits quick lookup.
          (push (cons reference-string datum) cache)
          (plist-put info :internal-references cache)
          reference-string))))

(defun unpackaged/org-export-new-title-reference (datum cache)
  "Return new reference for DATUM that is unique in CACHE."
  (cl-macrolet ((inc-suffixf (place)
                             `(progn
                                (string-match (rx bos
                                                  (minimal-match (group (1+ anything)))
                                                  (optional "--" (group (1+ digit)))
                                                  eos)
                                              ,place)
                                ;; HACK: `s1' instead of a gensym.
                                (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                           (match-string 2 ,place)))
                                        (suffix (if suffix
                                                    (string-to-number suffix)
                                                  0)))
                                  (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
    (let* ((title (org-element-property :raw-value datum))
           (ref (url-dashify-string (substring-no-properties title)))
           (parent (org-element-property :parent datum)))
      (while (--any (equal ref (car it))
                    cache)
        ;; Title not unique: make it so.
        (if parent
            ;; Append ancestor title.
            (setf title (concat (org-element-property :raw-value parent)
                                "--" title)
                  ref (url-dashify-string (substring-no-properties title))
                  parent (org-element-property :parent parent))
          ;; No more ancestors: add and increment a number.
          (inc-suffixf ref)))
      ref)))

(advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)

(require 'url)
(defun url-dashify-string (string &optional allowed-chars)
  "URI-encode STRING and return the result.
If STRING is multibyte, it is first converted to a utf-8 byte
string.  Each byte corresponding to an allowed character is left
as-is, while all other bytes are converted to dashes.

The allowed characters are specified by ALLOWED-CHARS.  If this
argument is nil, the list `url-unreserved-chars' determines the
allowed characters.  Otherwise, ALLOWED-CHARS should be a vector
whose Nth element is non-nil if character N is allowed."
  (unless allowed-chars
    (setq allowed-chars (url--allowed-chars url-unreserved-chars)))
  (mapconcat (lambda (byte)
	       (if (aref allowed-chars byte)
		   (char-to-string byte)
		 "-"))
	     (if (multibyte-string-p string)
		 (encode-coding-string string 'utf-8)
	       string)
	     ""))

(defun custom/org-publish-resolve-external-link (search file &optional prefer-custom)
  "Return reference for element matching string SEARCH in FILE.

Return value is an internal reference, as a string.

This function allows resolving external links with a search
option, e.g.,

  [[file:file.org::*heading][description]]
  [[file:file.org::#custom-id][description]]
  [[file:file.org::fuzzy][description]]

When PREFER-CUSTOM is non-nil, and SEARCH targets a headline in
FILE, return its custom ID, if any.

It only makes sense to use this if export back-end builds
references with `org-export-get-reference'."
  (cond
   ((not org-publish-cache)
    (progn
      (message "Reference %S in file %S cannot be resolved without publishing"
	       search
	       file)
      "MissingReference"))
   (t
    (let* ((filename (file-truename file))
	   (crossrefs
            (or
             (org-publish-cache-get-file-property filename :crossrefs nil t)
             ; If there is no list of crossrefs try publishing the file first -
             ; this will make unpackaged/org-export-get-reference (above)
             ; generate references for all headings in that file, which will
             ; subsequently be available as crossrefs. This will make the target
             ; file be exported twice, but we can live with that.
             (progn
               (org-publish-file filename nil t)
               (org-publish-cache-get-file-property filename :crossrefs nil t))))
	   (cells (org-export-string-to-search-cell search)))
      (or
       ;; Look for reference associated to search cells triggered by
       ;; LINK.  It can match when targeted file has been published
       ;; already.
       (cdr (cl-some (lambda (c) (assoc c crossrefs)) cells))
       ;; Search cell is unknown so far.  Generate a new internal
       ;; reference that will be used when the targeted file will be
       ;; published.
       (let ((new (org-export-new-reference crossrefs)))
	 (dolist (cell cells) (push (cons cell new) crossrefs))
	 (org-publish-cache-set-file-property filename :crossrefs crossrefs)
	 (org-export-format-reference new)))))))

(advice-add #'org-publish-resolve-external-link :override #'custom/org-publish-resolve-external-link)

(defun git-get-modif-date (filename)
  "Get date of last git commit touching FILENAME."
  (let* ((git-repo default-directory)
         (git-date (git--clean (git-run "log" "--follow"
                         "--format='%cI'" "-n" "1" "--" filename))))
    (when git-date (date-to-time git-date))))

(defun custom/org-export-get-date (info &optional fmt)
  (let* ((filename (plist-get info :input-file))
         (date (or
                (git-get-modif-date filename)
                (plist-get info :date)))
	(fmt (or fmt org-export-date-timestamp-format)))
    (when date
      (format-time-string fmt date))))
(advice-add #'org-export-get-date :override #'custom/org-export-get-date)

(unless (getenv "DEBUG")
  (advice-add 'org-publish-needed-p :override '(lambda (&rest args) t))
  (advice-add 'org-id-update-id-locations :around #'silence-messages)
  (advice-add 'org-id-locations-load :around #'silence-messages)
  (advice-add 'org-publish-reset-cache :around #'silence-messages)
  (advice-add 'org-publish-sitemap :around #'silence-messages))


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

(setq sitemap-header "* Site navigation

The repository contains a number of files that track more specific areas of XDP
development. These are included in the following list:
- [[file:index.org][Top-level XDP project management]]
\n")

(defun filter-sitemap (entry)
  "Recursively filter sitemap ENTRY.
Excludes any entries marked as \"SKIP\" by the sitemap-entry
format function."
  (cond ((not (listp entry))
         entry)
        ((and (stringp (car entry))
              (string-match-p "^SKIP$" (car entry)))
         nil)
        ((cdr entry)
         (cons (car entry) (-keep 'filter-sitemap (cdr entry))))
        (t entry)))

(defun sitemap-func (title list)
  "Default site map, as a string.
TITLE is the title of the site map.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (concat sitemap-header
  (org-list-to-org (-keep 'filter-sitemap list))))

(defun sitemap-entry (entry style project)
  "Default format for site map ENTRY, as a string.
ENTRY is a file name.  STYLE is the style of the sitemap.
PROJECT is the current project."
  (cond ((string-match-p "areas/[^/]+/.+\\.org" entry)
         "SKIP")
        ((not (directory-name-p entry))
         (if (org-publish-find-property entry :with-planning project)
             "SKIP"
           (format "[[file:%s][%s]]"
                   entry
                   (org-publish-find-title entry project))))
        ((string-match-p "areas/[^/]+" entry)
         "SKIP")
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
            :headline-levels 4
            :with-drawers nil
            :with-tags t
            :with-toc t
            :html-head-include-default-style nil
            :html-head-include-scripts nil
            :html-self-link-headlines t
            :html-head-extra ,export-html-head-bigblow
            :recursive t
            :auto-sitemap t
            :time-stamp-file nil
            :with-date t
            :with-author nil
            :sitemap-title "The XDP project"
            :sitemap-filename "sitemap.org"
            :sitemap-function sitemap-func
            :sitemap-format-entry sitemap-entry
            :htmlized-source nil))
         (styles
          `("styles"
            :base-directory ,(file-truename (concat user-emacs-directory "/../styles/"))
            :publishing-directory ,(file-truename (concat destdir "/styles"))
            :exclude "readtheorg/"
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
