;; This file contains the org-mode project management and TODO setup used in the
;; .org files in this repository.
;;
;; The setup (and associated work flow) is based on Bernt Hansen's setup
;; described here: http://doc.norang.ca/org-mode.html - except we omit the
;; clocking of tasks. It is probably a good idea to at least read sections 5-8
;; and 11 from that document to get an idea of the logic behind the setup.
;;
;; To use this file either load it directly from the repository, or copy it to
;; your ~/.emacs.d/ and load it from there. If loading from the repository, you
;; may want to adjust some of the settings at the top. Do this by copying them
;; to your .emacs and resetting them after loading the main file (they are just
;; variable assignments, so a later assignment will override an earlier one).
;;
;; To load the file, do something like this (WARNING: this will override your
;; existing setup such as agenda commands, TODO keywords etc):
;;
;;   (load (expand-file-name "~/git/xdp-project/org-setup.el"))
;;
;; Using a fairly recent version of org-mode is recommended. See the doc
;; referenced above for how to setup and use a git checkout of org-mode itself.

(require 'org)
(require 'org-tempo)

; Settings that you may want to customise
(setq org-directory "~/git/org"
      org-default-notes-file (concat org-directory "/notes.org")

      ; ~/git/org is for your personal org files, ~/git/xdp-project will pick up
      ; the xdp-project repository and areas/ directory
      org-agenda-files '("~/git/org/" "~/git/xdp-project/" "~/git/xdp-project/areas/")

      ; This is your username; used for filtering tasks and projects by OWNER
      ; (set OWNER property on an entry to take ownership, and that entry will
      ; be filtered out in other people's agenda)
      org-user-name user-login-name)


; Some useful keybindings
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f5>") 'bh/org-todo)
(global-set-key (kbd "<S-f5>") 'bh/widen)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> n") 'bh/toggle-next-task-display)
(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)


;; Everything below here is setup and helper functions
(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (org-shifttab)
    (org-reveal)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (make-directory "/tmp/publish" t))

(run-at-time "00:59" 3600 'org-save-all-org-buffers)

(setq org-cycle-emulate-tab 'whitestart
      org-agenda-todo-ignore-scheduled nil
      org-agenda-todo-list-sublevels nil
      org-agenda-sticky t
      org-log-done 'time
      org-log-into-drawer t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-outline-path-complete-in-steps t
      org-agenda-window-setup 'current-window
      org-show-following-heading t
      org-show-hierarchy-above t
      org-show-siblings (quote ((default)))
      org-enforce-todo-dependencies t)


(setq org-capture-templates
      '(("t" "todo" entry (file "~/git/org/refile.org")
         "* TODO %?\n%U\n%a\n  %i\n")
        ("r" "respond" entry (file "~/git/org/refile.org")
         "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :immediate-finish t)
        ("n" "note" entry (file "~/git/org/refile.org")
         "* %? :NOTE:\n%U\n%a\n  %i")
        ("m" "Meeting" entry (file "~/git/org/refile.org")
         "* MEETING with %? :MEETING:\n%U")
        ("j" "Journal" entry (file+datetree "~/git/org/diary.org")
         "* %?\n%U\n  %i")
        ("w" "org-protocol" entry (file "~/git/org/refile.org")
         "* TODO Review %c\n%U\n  %i" :immediate-finish t)
        ("q" "org-protocol bookmark" entry (file "~/git/org/bookmarks.org")
         "* %c\n%U\n  %i" :immediate-finish t)
        ("b" "Bookmark" entry (file "~/git/org/bookmarks.org")
         "* %a\n%U\n\n%i")
        ("a" "Appointment" entry (file+datetree+prompt "~/git/org/calendar.org")
         "* %?\n%^T  %i")))
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "DELEGATED(D@)")
        (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|"  "CANCELLED(c@/!)" "MEETING")))
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAIT" ("WAITING" . t))
              ("HOLD" ("WAITING" . t) ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-special-ctrl-k t)
(setq org-special-ctrl-a/e t)
(setq org-yank-adjusted-subtrees t)
(setq org-catch-invisible-edits 'error)

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(setq org-highlight-latex-and-related '(latex script entities))

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Stop using paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path nil)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@short" . ?s)
                            ("@medium" . ?m)
                            ("@long" . ?l)
                            (:endgroup)
                            ("HOLD" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("REFILE" . ?r)
                            ("crypt" . ?E)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??)
                            ("ignoreheading" . ?i)
                            ("export" . ?x)
                            ("noexport". ?X))))

(defvar-local org-extra-file-tags nil
  "Extra file tags to be applied to this file. Can be set from
  .dir-locals.el, for instance.")

(defun thj/org-add-extra-file-tags (orig-fun &rest r)
  "Add extra-file-tags before calling ORIG-FUN with args R."
  (let ((org-file-tags (delete-dups (append org-extra-file-tags org-file-tags))))
    (apply orig-fun r)))
(advice-add 'org-scan-tags :around #'thj/org-add-extra-file-tags)
(advice-add 'org-get-tags :around #'thj/org-add-extra-file-tags)

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)


(setq org-agenda-skip-function-global 'thj/skip-not-mine)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("a" "Agenda"
               ((agenda ""
                        ((org-agenda-skip-function 'thj/skip-agenda-items)))))
              (" " "Agenda"
               ((agenda ""
                        ((org-agenda-skip-function 'thj/skip-agenda-items)))
                (tags-todo "-REFILE-CANCELLED/!-HOLD-WAIT"
                           ((org-agenda-overriding-header "Tasks")
                            (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-WAIT-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header "Next Tasks")
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-agenda-todo-ignore-scheduled 'future)
                            (org-agenda-todo-ignore-deadlines 'future)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil))))
               nil)
              ("r" "Tasks to Refile" tags "REFILE"
               ((org-agenda-overriding-header "Tasks to Refile")
                (org-tags-match-list-sublevels nil)))
              ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
               ((org-agenda-overriding-header "Stuck Projects")
                (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
              ("n" "Next Tasks" tags-todo "-WAIT-CANCELLED/!NEXT"
               ((org-agenda-overriding-header "Next Tasks")
                (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                (org-agenda-todo-ignore-scheduled t)
                (org-agenda-todo-ignore-deadlines t)
                (org-tags-match-list-sublevels t)
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAIT"
               ((org-agenda-overriding-header "Tasks")
                (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("p" "Projects" tags-todo "-CANCELLED/!"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-skip-function 'bh/skip-non-projects)
                (org-agenda-todo-ignore-scheduled 'future)
                (org-agenda-todo-ignore-deadlines 'future)
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("w" "Waiting Tasks" tags-todo "-CANCELLED/!WAIT|HOLD"
               ((org-agenda-overriding-header "Waiting and Postponed tasks"))
               (org-agenda-skip-function 'bh/skip-projects-and-habits)
               (org-agenda-todo-ignore-scheduled 'future)
               (org-agenda-todo-ignore-deadlines 'future))
              ("d" "Upcoming deadlines" agenda ""
               ((org-agenda-overriding-header "Upcoming deadlines")
                (org-agenda-time-grid nil)
                (org-agenda-ndays 1)
                (org-deadline-warning-days 365)
                (org-agenda-entry-types '(:deadline))))
              ("A" "Tasks to Archive" tags "-REFILE/"
               ((org-agenda-overriding-header "Tasks to Archive")
                (org-agenda-skip-function 'bh/skip-non-archivable-tasks))))))

(setq org-agenda-prefix-format
      '((agenda . "  %?-12t% s%(thj/entry-location)")
        (timeline . "  % s")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))

(defun thj/org-date-to-absolute (date)
  (calendar-absolute-from-gregorian
   (org-date-to-gregorian
    (org-parse-time-string date))))

(defun thj/skip-not-mine ()
  "Skip agenda items that are assigned an OWNER other than myself"
  (let ((owner (or (thj/get-property "OWNER")
                   (save-excursion
                     (bh/find-project-task)
                     (thj/get-property "OWNER")))))
      (when (and owner (not (string= owner org-user-name)))
        (org-end-of-subtree t))))

(defun thj/skip-after-end-date ()
  "Skip agenda items after their END_DATE property"
  (when (and (boundp 'date) date)
    (let ((end-date (thj/get-property "END_DATE")))
      (when (and end-date (< (thj/org-date-to-absolute end-date)
                             (calendar-absolute-from-gregorian date)))
        (org-end-of-subtree t)))))

(defun thj/skip-excluded ()
  "Skip agenda items on dates in EXCLUDE property"
  (when (and (boundp 'date) date)
    (let ((excluded-string (thj/get-property "EXCLUDE"))
          exclusions)
      (when excluded-string
        (setq exclusions (mapcar
                          (lambda (ex-date-string)
                            (equal date (org-date-to-gregorian
                                          (org-parse-time-string ex-date-string))))
                          (split-string excluded-string ",")))
        (when (memq t exclusions)
          (org-end-of-subtree t))))))

(defun thj/skip-agenda-items ()
  "Skip excluded or after end date agenda items"
  (or (thj/skip-after-end-date)
      (thj/skip-excluded)))

(defun thj/entry-location ()
  "Get an entry location"
  (let ((location       (thj/get-property "LOCATION")))
    (if location
        (format "[%-15s] " (substring location 0 (min 15 (length location))))
      "")))

(defun thj/get-property (property)
  "Get (first) property value"
  (let ((value (assoc-string property (org-entry-properties nil 'standard))))
    (when value
      (cdr value))))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next (save-excursion
                             (forward-line 1)
                             (and (< (point) subtree-end)
                                  (re-search-forward "^\\*+ \\(NEXT\\) " subtree-end t)))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (if (bh/is-project-p)
              nil
            subtree-end)))
    (org-end-of-subtree t)))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
;       ((org-is-habit-p)
;        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
;       ((org-is-habit-p)
;        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
;       ((org-is-habit-p)
;        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
;       ((org-is-habit-p)
;        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      ;; Consider only tasks with done todo headings as archivable candidates
      (if (member (org-get-todo-state) org-done-keywords)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (daynr (string-to-number (format-time-string "%d" (current-time))))
                 (a-month-ago (* 60 60 24 (+ daynr 1)))
                 (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                 (this-month (format-time-string "%Y-%m-" (current-time)))
                 (subtree-is-current (save-excursion
                                       (forward-line 1)
                                       (and (< (point) subtree-end)
                                            (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
            (if subtree-is-current
                next-headline ; Has a date in this month or last month, skip it
              nil))  ; available to archive
        (or next-headline (point-max))))))


;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda habit-down time-up user-defined-up priority-down effort-up category-keep)
              (todo category-up priority-down effort-up)
              (tags category-up priority-down effort-up)
              (search category-up))))

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid (quote ((daily today remove-match)
                                   (1000 1200 1400 1600 1800 2000 2200)
                                   "......" "----------------")))

;; Display tags farther right
(setq org-agenda-tags-column -87)

;;
;; Agenda sorting functions
;;
(setq org-agenda-cmp-user-defined 'bh/agenda-sort)

(defun bh/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
     ; time specific items are already sorted first by org-agenda-sorting-strategy

     ; non-deadline and non-scheduled items next
     ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

     ; deadlines for today next
     ((bh/agenda-sort-test 'bh/is-due-deadline a b))

     ; late deadlines next
     ((bh/agenda-sort-test-num 'bh/is-late-deadline '< a b))

     ; scheduled items for today next
     ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

     ; late scheduled items next
     ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

     ; pending deadlines last
     ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

     ; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro bh/agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(cond
    ; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
          (apply ,fn (list ,b)))
     (setq result nil))
    ; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
    ; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
    ; if none match leave them unsorted
    (t nil)))

(defmacro bh/agenda-sort-test-num (fn compfn a b)
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
         (progn
           (setq num-b (string-to-number (match-string 1 ,b)))
           (setq result (if (apply ,compfn (list num-a num-b))
                            -1
                          1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun bh/is-not-scheduled-or-deadline (date-str)
  (and (not (bh/is-deadline date-str))
       (not (bh/is-scheduled date-str))))

(defun bh/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun bh/is-late-deadline (date-str)
  (string-match "In *\\(-.*\\)d\.:" date-str))

(defun bh/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun bh/is-deadline (date-str)
  (or (bh/is-due-deadline date-str)
      (bh/is-late-deadline date-str)
      (bh/is-pending-deadline date-str)))

(defun bh/is-scheduled (date-str)
  (or (bh/is-scheduled-today date-str)
      (bh/is-scheduled-late date-str)))

(defun bh/is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun bh/is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

;; TODO keyword colours - plain version
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold))))

;; TODO keyword colours - solarized version; uncomment if using the solarized
;; colour scheme
;; flet taken from solarized-definitions.el
;(cl-flet ((find-color (name)
;                   (let* ((index (if window-system
;                                     (if solarized-degrade
;                                         3
;                                       (if solarized-broken-srgb 2 1))
;                                   (case (display-color-cells)
;                                     (16 4)
;                                     (8  5)
;                                     (otherwise 3)))))
;                     (nth index (assoc name solarized-colors)))))
;  (setq org-todo-keyword-faces
;        `(("TODO" :foreground ,(find-color 'green) :weight bold)
;          ("NEXT" :foreground ,(find-color 'base2) :weight bold)
;          ("DONE" :foreground ,(find-color 'base01) :weight bold)
;          ("WAIT" :foreground ,(find-color 'orange) :weight bold)
;          ("HOLD" :foreground ,(find-color 'magenta) :weight bold)
;          ("CANCELLED" :foreground ,(find-color 'base01) :weight bold)
;          ("MEETING" :foreground ,(find-color 'base01) :weight bold))))



;; Narrowing etc

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (widen)
        (org-narrow-to-subtree)
        (org-show-todo-tree nil))
    (widen)
    (org-narrow-to-subtree)
    (org-show-todo-tree nil)))

(defun bh/widen ()
  (interactive)
  (widen)
  (org-agenda-remove-restriction-lock))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W" 'bh/widen))
          'append)

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
        (bh/narrow-to-org-subtree))
    (bh/narrow-to-org-subtree)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
          'append)

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
        (bh/narrow-up-one-org-level))
    (bh/narrow-up-one-org-level)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
          'append)

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
        (bh/narrow-to-org-project))
    (bh/narrow-to-org-project)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
          'append)

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
          'append)

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (org-get-at-bol 'org-hd-marker))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((equal major-mode 'org-agenda-mode)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type)))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (t
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))

(setq org-show-entry-below (quote ((default))))


;; Indent mode
(setq org-startup-indented t)

;; Hide blank lines
(setq org-cycle-separator-lines 0)

;; Notes at the top
(setq org-reverse-note-order nil)

;; Speed commands

(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (("0" . ignore)
                                      ("1" . ignore)
                                      ("2" . ignore)
                                      ("3" . ignore)
                                      ("4" . ignore)
                                      ("5" . ignore)
                                      ("6" . ignore)
                                      ("7" . ignore)
                                      ("8" . ignore)
                                      ("9" . ignore)

                                      ("a" . ignore)
                                      ("d" . ignore)
                                      ("h" . bh/hide-other)
                                      ("i" progn
                                       (forward-char 1)
                                       (call-interactively 'org-insert-heading-respect-content))
                                      ("k" . org-kill-note-or-show-branches)
                                      ("l" . ignore)
                                      ("m" . ignore)
                                      ("q" . bh/show-org-agenda)
                                      ("r" . ignore)
                                      ("s" . org-save-all-org-buffers)
                                      ("w" . org-refile)
                                      ("x" . ignore)
                                      ("y" . ignore)
                                      ("z" . org-add-note)

                                      ("A" . ignore)
                                      ("B" . ignore)
                                      ("E" . ignore)
                                      ("F" . ignore)
                                      ("G" . ignore)
                                      ("H" . ignore)
                                      ("J" . org-clock-goto)
                                      ("K" . ignore)
                                      ("L" . ignore)
                                      ("M" . org-agenda-month-view)
                                      ("N" . bh/narrow-to-subtree)
                                      ("P" . bh/narrow-to-project)
                                      ("Q" . ignore)
                                      ("R" . ignore)
                                      ("S" . ignore)
                                      ("T" . (org-show-todo-tree nil))
                                      ("U" . bh/narrow-up-one-level)
                                      ("V" . ignore)
                                      ("W" . bh/widen)
                                      ("X" . ignore)
                                      ("Y" . ignore)
                                      ("Z" . ignore))))

(defun bh/show-org-agenda ()
  (interactive)
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda( )*")
    (switch-to-buffer "*Org Agenda*")))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "M" 'org-agenda-month-view))
          'append)

(require 'org-id)
(setq org-id-method 'uuidgen
      org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(provide 'org-setup)
