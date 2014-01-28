;;; -*- Mode: Emacs-Lisp -*-
;;;;---------------------------------------------------------------------------
;; org-mode-setup.el configuration file
;; author: Dylan Schwilk
;;
;;; Customizations for org-mode
;;;;---------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personalization and local settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; My org agenda files
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
(setq org-directory "~/org/")
;; catchall file (used for caldav-sync with google calendar as well)
(setq org-default-notes-file (concat org-directory "refile.org"))

;; google calendar synchronization using org-caldev
;; see https://github.com/dengste/org-caldav
(require 'org-id)
(setq org-id-method (quote uuidgen))
(require 'org-caldav) ;; in ~/.emacs.d/contrib/
(setq org-caldav-url "https://www.google.com/calendar/dav")
(setq org-caldav-calendar-id "dschwilk@gmail.com")
(setq org-caldav-inbox org-default-notes-file)
(setq my-journal-file "~/org/*journal.org")
(setq my-exclude-from-caldav-sync (file-expand-wildcards "~/org/*journal.org") )
(setq org-caldav-files (my-set-difference org-agenda-files my-exclude-from-caldav-sync) )
(setq org-caldav-save-directory org-directory)
(setq org-icalendar-include-sexps nil)
;; org-contacts
(require 'org-contacts)
(setq my-contacts-file "~/org/contacts.org")
(setq org-contacts-files (file-expand-wildcards my-contacts-file) )

;; org-protocol
(require 'org-protocol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General settings to invoke for org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'org-mode-hook
          (lambda ()
            (setq line-mode-visual 1) 
            ;; flyspell mode for spell checking everywhere
            (flyspell-mode 1)
            ;; Undefine C-c [ and C-c ] since this breaks my org-agenda files
            ;; when directories are include It expands the files in the
            ;; directories individually
            (org-defkey org-mode-map "\C-c["    'undefined)
            (org-defkey org-mode-map "\C-c]"    'undefined)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; filetypes
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(setq org-archive-save-context-info 1)
;(setq require-final-newline t)
(setq org-startup-indented 1)
(setq org-hide-leading-stars t)
(setq org-agenda-include-diary 0)
(setq org-startup-folded t)
(setq org-alphabetical-lists t)
(setq org-clone-delete-id t)
(setq org-cycle-include-plain-lists t)
(setq org-src-fontify-natively t)
(setq org-reverse-note-order nil)
(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-hierarchy-below t)
(setq org-show-siblings t)
(setq org-yank-adjusted-subtrees t)  ;; adjusts level during paste of subtree
(setq org-deadline-warning-days 30)
(setq org-export-with-timestamps nil) ;;?
(setq org-return-follows-link t)
(setq org-special-ctrl-a/e t) ;; begin/end of heading rather than line in org-mode
(setq org-special-ctrl-k t) ;; special kill-line
; Use the current window for C-c ' source editing:
(setq org-src-window-setup 'current-window) 
(setq org-enforce-todo-dependencies t)
(setq org-cycle-separator-lines 0)
(setq org-table-export-default-format "orgtbl-to-csv") ; tables exported csv, not tab
;;(setq org-table-use-standard-references 'from)

;; refile options:
;; allow to create new nodes (must be confirmed by the user) as
;; refile targets
(setq org-refile-allow-creating-parent-nodes 'confirm)
; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(setq org-catch-invisible-edits 'error) ;; catch edits to hidden parts. Try this

;; (setq org-src-preserve-indentation nil)
;; (setq org-edit-src-content-indentation 0)
;; (setq org-export-coding-system 'utf-8)

;; TODO states, selection
(setq org-use-fast-todo-selection t)
;(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; logging and timestamps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-habit setup
(require 'org-habit)
;; ; position the habit graph on the agenda to the right of the default
;; (setq org-habit-graph-column 50)
;; (run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))
;; (global-auto-revert-mode t)

(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)
(setq org-use-fast-todo-selection t)

;; Remove empty LOGBOOK drawers on clock out
(defun dws/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'dws/remove-empty-drawer-on-clock-out 'append)

(defvar dws/insert-inactive-timestamp t)

(defun dws/toggle-insert-inactive-timestamp ()
  (interactive)
  (setq dws/insert-inactive-timestamp (not dws/insert-inactive-timestamp))
  (message "Heading timestamps are %s" (if dws/insert-inactive-timestamp "ON" "OFF")))

(defun dws/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun dws/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun dws/insert-heading-inactive-timestamp ()
  (save-excursion
    (when dws/insert-inactive-timestamp
      (org-return)
      (org-cycle)
      (dws/insert-inactive-timestamp))))


;; this adds timestamps to everything
;;(add-hook 'org-insert-heading-hook 'dws/insert-heading-inactive-timestamp 'append)

;(custom-set-faces
 ;'(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; babel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (sh . t)
         (org . t)
         (latex . t))))

(add-hook 'org-babel-after-execute-hook 'dws/display-inline-images 'append)

; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

;; still need to get org mode complete bound to a key
;(setq org-fallback-completion-command 'hippie-expand)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-crypt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(require 'org-crypt)
; Encrypt all entries before saving
;(org-crypt-use-before-save-magic)
;(setq org-tags-exclude-from-inheritance (quote ("crypt")))
; GPG key to use for encryption
;(setq org-crypt-key "")
;(setq org-crypt-disable-auto-save nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dws org functions, TODO states, tags taetc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add thunderlink link type for linking to emails in thunderbird
;; https://addons.mozilla.org/en-us/thunderbird/addon/thunderlink/
;; Assumes that link of form:
;; thunderlink://messageid=CC0A9B0B9C20AB4C95BD90FAEA6DACA52CDE1AE5@centaur08.ttu.edu
(org-add-link-type "thunderlink" 'org-thunderlink-open)
(defun org-thunderlink-open (path)
"Opens a specified email in Thunderbird with the help of the add-on ThunderLink." (start-process "schwilk" nil "thunderbird" "-thunderlink" (concat "thunderlink:" path))) 

(defun dws/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun dws/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

;; org keywords  and TODO states
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

 (setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :inherit font-lock-builtin-face :weight bold)
              ("DONE" :inherit font-lock-constant-face :weight bold)
              ("WAITING" :inherit font-lock-warning-face :weight bold)
              ("HOLD" :inherit font-lock-variable-name-face :weight bold)
              ("CANCELLED" :inherit font-lock-doc-string-face :weight bold)
              ("OPEN" :inherit font-lock-function-name-face :weight bold)
              ("CLOSED" :inherit: font-lock-comment-face :weight bold)
              ("PHONE" :inherit font-lock-doc-face  :weight bold))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING" . t) ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file org-default-notes-file)
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file org-default-notes-file)
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file org-default-notes-file)
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree my-journal-file)
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "Org-protocol" entry (file org-default-notes-file)
               "* %^{Title}\n\n  Source: %u, %c\n\n  %i" :empty-lines 1)
              ("p" "Phone call" entry (file org-default-notes-file)
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file org-default-notes-file)
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
              ("a" "Appointment" entry (file+headline org-default-notes-file "Appointments") 
               "* APPT %^{Description} %^t %^g %? Added: %U") 
              ;; ("m" "Meeting" entry (file+headline "~/org/work.org" "Calendar") 
              ;;  "* APPT %^{Description} %^t %^g %? Added: %U") 
              ("c" "Contacts" entry (file my-contacts-file)
               "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:")
)))


; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?H)
                              (:endgroup)
                            ("PHONE" . ?p)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("personal" . ?P)
                            ("WORK" . ?W)
                            ("ORG" . ?O)
                            ("MARK" . ?M)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))

; Allow setting single tags without the menu
;(setq org-fast-tag-selection-single-key (quote expert))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Refile settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exclude DONE state tasks from refile targets
(defun dws/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'dws/verify-refile-target)

;; refiling and IDO mode 
(setq org-completion-use-ido 1)
; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)
; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; ; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
;; ; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clocking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; Show lots of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 36)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'dws/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq dws/keep-clock-running nil)

(defun dws/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (dws/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (dws/is-project-p))
      "TODO"))))

(defun dws/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun dws/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq dws/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (dws/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (dws/clock-in-organization-task-as-default)))))

(defun dws/punch-out ()
  (interactive)
  (setq dws/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun dws/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun dws/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when dws/keep-clock-running
            (dws/clock-in-default-task)))))))

(defun dws/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find dws/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun dws/clock-out-maybe ()
  (when (and dws/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (dws/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'dws/clock-out-maybe 'append)

(defun dws/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun dws/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(setq org-time-stamp-rounding-minutes (quote (1 1)))

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00"))))

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

; Set default column view headings: Task Effort Clock_Summary
;(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

; global Effort estimate values
; global STYLE property values for completion
;(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
;                                    ("STYLE_ALL" . "habit"))))



;(require 'bbdb)
;(require 'bbdb-com)

;; Phone capture template handling with BBDB lookup
;; Adapted from code by Gregory J. Grubbs
;; (defun dws/phone-call ()
;;   "Return name and company info for caller from bbdb lookup"
;;   (interactive)
;;   (let* (name rec caller)
;;     (setq name (completing-read "Who is calling? "
;;                                 (bbdb-hashtable)
;;                                 'bbdb-completion-predicate
;;                                 'confirm))
;;     (when (> (length name) 0)
;;       ; Something was supplied - look it up in bbdb
;;       (setq rec
;;             (or (first
;;                  (or (bbdb-search (bbdb-records) name nil nil)
;;                      (bbdb-search (bbdb-records) nil name nil)))
;;                 name)))

;;     ; Build the bbdb link if we have a bbdb record, otherwise just return the name
;;     (setq caller (cond ((and rec (vectorp rec))
;;                         (let ((name (bbdb-record-name rec))
;;                               (company (bbdb-record-company rec)))
;;                           (concat "[[bbdb:"
;;                                   name "]["
;;                                   name "]]"
;;                                   (when company
;;                                     (concat " - " company)))))
;;                        (rec)
;;                        (t "NameOfCaller")))
;;     (insert caller)))

(defun dws/is-project-p ()
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

(defun dws/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (dws/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun dws/is-task-p ()
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

(defun dws/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun dws/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun dws/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun dws/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (dws/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun dws/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (dws/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (dws/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun dws/skip-non-projects ()
  "Skip trees that are not projects"
  (dws/list-sublevels-for-projects-indented)
  (if (save-excursion (dws/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((and (dws/is-project-p)
                 (marker-buffer org-agenda-restrict-begin))
            nil)
           ((and (dws/is-project-p)
                 (not (marker-buffer org-agenda-restrict-begin))
                 (not (dws/is-project-subtree-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun dws/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((dws/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun dws/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single
non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((dws/is-project-p)
        next-headline)
       ((and (dws/is-task-p) (not (dws/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun dws/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks,
habits, NEXT tasks, and loose tasks. When not restricted, skip
project and sub-project tasks, habits, and project related
tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((dws/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (dws/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (dws/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun dws/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((dws/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun dws/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (dws/is-subproject-p)
        nil
      next-headline)))

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(defun dws/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      ;; Consider only tasks with done todo headings as archivable candidates
      (if (member (org-get-todo-state) org-done-keywords)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (daynr (string-to-int (format-time-string "%d" (current-time))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Export-related settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dws/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

;; Explicitly load required exporters
(require 'ox-html)
(require 'ox-ascii)
(require 'ox-latex)
(require 'ox-beamer)

;;for code syntax highlighting
(require 'htmlize)
(setq org-html-htmlize-output-type 'css)

;; org2blog
(require 'org2blog-autoloads)
(setq org2blog/wp-blog-alist
      '(("schwilk.org"
         :url "http://www.schwilk.org/wordpress/xmlrpc.php"
         :username "firepulaski"
         :default-title "Hello World"
         :default-categories ("Ecology")
         :tags-as-categories nil)))


(add-to-list 'org-latex-classes
                '("pres"
                  "\\documentclass[presentation]{beamer}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\[EXTRA]"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

; Export with LaTeX fragments
(setq org-export-with-LaTeX-fragments t)
; Increase default number of headings to export
;(setq org-export-headline-levels 6)
(setq org-export-latex-listings t)

;; use okular with org-mode pdf viewer.  Run after org.el or overwritten
(eval-after-load "org"
  '(setq  org-file-apps (quote ((auto-mode . emacs)
        ("\\.mm\\'" . system)
        ("\\.x?html?\\'" . system)
        ("\\.pdf\\'" . "okular --unique %s")))))

;; HTML export
; Inline images in HTML instead of producting links to the image
(setq org-html-inline-images t)
(setq org-html-head-include-default-style nil)

;;(setq org-export-allow-BIND t)  ;; purpose?

;; ; Erase all reminders and rebuilt reminders for today from the agenda
;; (defun dws/org-agenda-to-appt ()
;;   (interactive)
;;   (setq appt-time-msg-list nil)
;;   (org-agenda-to-appt))

;; ; Rebuild the reminders everytime the agenda is displayed
;; (add-hook 'org-finalize-agenda-hook 'dws/org-agenda-to-appt 'append)

;; ; This is at the end of my .emacs - so appointments are set up when Emacs starts
;; (dws/org-agenda-to-appt)

;; ; Activate appointments so we get notifications
;; (appt-activate t)

;; ; If we leave Emacs running overnight - reset the appointments one minute after midnight
;; (run-at-time "24:01" nil 'dws/org-agenda-to-appt)

;; ;; Enable abbrev-mode
;; (add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

(defun dws/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (dws/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (dws/narrow-to-org-subtree)
    (org-show-todo-tree nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Agenda functions, options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Overwrite the current window with the agenda
(setq org-agenda-window-setup 'current-window)

;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items (quote (closed state)))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"  ;; space bar give whole list
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'dws/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(priority-down category-keep))))
                (tags-todo "-WAITING-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header "Next Tasks")
                            (org-agenda-skip-function 'dws/skip-projects-and-habits-and-single-tasks)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-agenda-todo-ignore-with-date t)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(priority-down todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
                           ((org-agenda-overriding-header "Tasks")
                            (org-agenda-skip-function 'dws/skip-project-tasks-maybe)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-agenda-todo-ignore-with-date t)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'dws/skip-non-projects)
                            (org-agenda-sorting-strategy
                             '(priority-down category-keep))))
                (tags-todo "-CANCELLED+WAITING/!"
                           ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                            (org-agenda-skip-function 'dws/skip-stuck-projects)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled 'future)
                            (org-agenda-todo-ignore-deadlines 'future)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'dws/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil)
              ("r" "Tasks to Refile" tags "REFILE"
               ((org-agenda-overriding-header "Tasks to Refile")
                (org-tags-match-list-sublevels nil)))
              ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
               ((org-agenda-overriding-header "Stuck Projects")
                (org-agenda-skip-function 'dws/skip-non-stuck-projects)))
              ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
               ((org-agenda-overriding-header "Next Tasks")
                (org-agenda-skip-function 'dws/skip-projects-and-habits-and-single-tasks)
                (org-agenda-todo-ignore-scheduled t)
                (org-agenda-todo-ignore-deadlines t)
                (org-agenda-todo-ignore-with-date t)
                (org-tags-match-list-sublevels t)
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
               ((org-agenda-overriding-header "Tasks")
                (org-agenda-skip-function 'dws/skip-project-tasks-maybe)
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("p" "Projects" tags-todo "-HOLD-CANCELLED/!"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-skip-function 'dws/skip-non-projects)
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("w" "Waiting Tasks" tags-todo "-CANCELLED+WAITING/!"
               ((org-agenda-overriding-header "Waiting and Postponed tasks"))
               (org-tags-match-list-sublevels nil))
              ("A" "Tasks to Archive" tags "-REFILE/"
               ((org-agenda-overriding-header "Tasks to Archive")
                (org-agenda-skip-function 'dws/skip-non-archivable-tasks)
                (org-tags-match-list-sublevels nil))))))


(defun dws/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t)
        ((string= tag "@home")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'dws/org-auto-exclude-function)

(defun dws/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-agenda-remove-restriction-lock)
    (widen)
    (org-agenda-remove-restriction-lock)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W" 'dws/widen))
          'append)

(defun dws/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (widen)
    (dws/set-agenda-restriction-lock 4)
    (org-agenda-redo)
    (beginning-of-buffer)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "F" 'dws/restrict-to-file-or-follow))
          'append)

(defun dws/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree))

(defun dws/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
        (dws/narrow-to-org-subtree)
        (save-restriction
          (org-agenda-set-restriction-lock)))
    (dws/narrow-to-org-subtree)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "N" 'dws/narrow-to-subtree))
          'append)

(defun dws/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (dws/narrow-to-org-subtree)))

(defun dws/get-pom-from-agenda-restriction-or-point ()
  (or (org-get-at-bol 'org-hd-marker)
      (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun dws/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (dws/get-pom-from-agenda-restriction-or-point)
        (dws/narrow-up-one-org-level))
    (dws/narrow-up-one-org-level)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U" 'dws/narrow-up-one-level))
          'append)

(defun dws/narrow-to-org-project ()
  (widen)
  (save-excursion
    (dws/find-project-task)
    (dws/narrow-to-org-subtree)))

(defun dws/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (dws/get-pom-from-agenda-restriction-or-point)
        (dws/narrow-to-org-project)
        (save-restriction
          (org-agenda-set-restriction-lock)))
    (dws/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P" 'dws/narrow-to-project))
          'append)

(defvar dws/current-view-project nil)

(defun dws/view-next-project ()
  (interactive)
  (unless (marker-position org-agenda-restrict-begin)
    (goto-char (point-min))
    (re-search-forward "Tasks to Refile")
    (setq dws/current-view-project (point)))
  (dws/widen)
  (goto-char dws/current-view-project)
  (forward-visible-line 1)
  (while (and (< (point) (point-max))
              (or (not (org-get-at-bol 'org-hd-marker))
                  (org-with-point-at (org-get-at-bol 'org-hd-marker)
                    (or (not (dws/is-project-p))
                        (dws/is-project-subtree-p)))))
    (forward-visible-line 1))
  (setq dws/current-view-project (point))
  (if (org-get-at-bol 'org-hd-marker)
      (progn
        (dws/narrow-to-project)
        (org-agenda-redo)
        (beginning-of-buffer))
    (beginning-of-buffer)
    (error "All projects viewed.")))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "V" 'dws/view-next-project))
          'append)

(setq org-show-entry-below (quote ((default))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'dws/set-agenda-restriction-lock))
          'append)

(defun dws/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (dws/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type)))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))

;; Always highlight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; agenda options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-tags-match-list-sublevels t)
(setq org-agenda-persistent-filter t)
(setq org-agenda-span 'day)
(setq org-stuck-projects (quote ("" nil nil "")))

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

; (setq org-agenda-include-diary nil)
; (setq org-agenda-diary-file "~/git/org/diary.org")
; (setq org-agenda-insert-diary-extract-time t)

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;;
(setq org-agenda-skip-additional-timestamps-same-entry t)

;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid (quote ((daily today remove-match)
                                   #("----------------" 0 16 (org-heading t))
                                   (0900 1100 1300 1500 1700))))

;; Display tags farther right
(setq org-agenda-tags-column -102)

(setq org-read-date-prefer-future 'time)
(setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "-"))))

;;
;; Agenda sorting functions
;;
(setq org-agenda-cmp-user-defined 'dws/agenda-sort)

(defun dws/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
     ; time specific items are already sorted first by org-agenda-sorting-strategy

     ; non-deadline and non-scheduled items next
     ((dws/agenda-sort-test 'dws/is-not-scheduled-or-deadline a b))

     ; deadlines for today next
     ((dws/agenda-sort-test 'dws/is-due-deadline a b))

     ; late deadlines next
     ((dws/agenda-sort-test-num 'dws/is-late-deadline '< a b))

     ; scheduled items for today next
     ((dws/agenda-sort-test 'dws/is-scheduled-today a b))

     ; late scheduled items next
     ((dws/agenda-sort-test-num 'dws/is-scheduled-late '> a b))

     ; pending deadlines last
     ((dws/agenda-sort-test-num 'dws/is-pending-deadline '< a b))

     ; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro dws/agenda-sort-test (fn a b)
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

(defmacro dws/agenda-sort-test-num (fn compfn a b)
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

(defun dws/is-not-scheduled-or-deadline (date-str)
  (and (not (dws/is-deadline date-str))
       (not (dws/is-scheduled date-str))))

(defun dws/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun dws/is-late-deadline (date-str)
  (string-match "In *\\(-.*\\)d\.:" date-str))

(defun dws/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun dws/is-deadline (date-str)
  (or (dws/is-due-deadline date-str)
      (dws/is-late-deadline date-str)
      (dws/is-pending-deadline date-str)))

(defun dws/is-scheduled (date-str)
  (or (dws/is-scheduled-today date-str)
      (dws/is-scheduled-late date-str)))

(defun dws/is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun dws/is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (define-key org-agenda-mode-map "q" 'bury-buffer))
          'append)

(defun dws/show-org-agenda ()
  (interactive)
  (switch-to-buffer "*Org Agenda*")
  (delete-other-windows))


;;;;;;;;;;;; end agenda section

(defun dws/prepare-meeting-notes ()
  "Prepare meeting notes for email
   Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
  (interactive)
  (let (prefix)
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
          (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
        (goto-char (point-min))
        (kill-ring-save (point-min) (point-max))))))

;; smex, not used
;; (require 'smex)
;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "C-x x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Bookmark handling
;;
;(global-set-key (kbd "<C-f6>") '(lambda () (interactive) (bookmark-set "SAVED")))
;(global-set-key (kbd "<f6>") '(lambda () (interactive) (bookmark-jump "SAVED")))

(require 'org-mime)

(setq org-structure-template-alist
      (quote (("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
              ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
              ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
              ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
              ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n</center>")
              ("l" "#+begin_latex\n?\n#+end_latex" "<literal style=\"latex\">\n?\n</literal>")
              ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
              ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
              ("H" "#+html: " "<literal style=\"html\">?</literal>")
              ("a" "#+begin_ascii\n?\n#+end_ascii")
              ("A" "#+ascii: ")
              ("i" "#+index: ?" "#+index: ?")
              ("I" "#+include %file ?" "<include file=%file markup=\"?\">")
              ("r" "#+begin_src R :exports none\n\n#+end_src"))))

(defun dws/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
            (org-todo "TODO")))))))

(add-hook 'org-after-todo-state-change-hook 'dws/mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'dws/mark-next-parent-tasks-todo 'append)
(add-hook 'message-mode-hook 'orgstruct++-mode 'append)
(add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
(add-hook 'message-mode-hook 'bbdb-define-all-aliases 'append)
(add-hook 'message-mode-hook 'orgtbl-mode 'append)
(add-hook 'message-mode-hook 'turn-on-flyspell 'append)
(add-hook 'message-mode-hook
          '(lambda () (setq fill-column 72))
          'append)


;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c M-o") 'dws/mail-subtree))
;;           'append)

;; (defun dws/mail-subtree ()
;;   (interactive)
;;   (org-mark-subtree)
;;   (org-mime-subtree))


(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(setq org-emphasis-alist (quote (("*" bold "<b>" "</b>")
                                 ("/" italic "<i>" "</i>")
                                 ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                                 ("=" org-code "<code>" "</code>" verbatim)
                                 ("~" org-verbatim "<code>" "</code>" verbatim))))

(run-at-time "00:59" 3600 'org-save-all-org-buffers)
