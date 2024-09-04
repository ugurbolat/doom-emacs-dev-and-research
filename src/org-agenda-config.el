;;; src/org-agenda-config.el -*- lexical-binding: t; -*-

(use-package! org
  ;;:after org-agenda
  :config

  ;; recursive directory files
  (setq ub/roam-all-files-list (ub/directory-files-recursively ub/roam-root-dir "\.org$" 0 ".sync-conflict"))
  (setq ub/doom-emacs-files-list (directory-files-recursively ub/doom-emacs-root-dir "\.org$"))
  (setq ub/roam-dailies-main-files-list (ub/directory-files-recursively ub/roam-dailies-dir "\.org$" 0 ".sync-conflict"))
  (setq ub/roam-dailies-archive-files-list (ub/directory-files-recursively ub/roam-dailies-dir "\.org$" 0 ".sync-conflict"))

  ;; utils
  (defun ub/days-to-next-sunday()
    (let ((dayspan 0)
          (today (string-to-number (format-time-string "%u"))))
      (cond
       ((> today 0) ; from today till sunday, today included
        (setq dayspan (- 8 today)))
       ((= today 0) ; sunday to sunday
        (setq dayspan 8)))))


  (defun ub/days-to-next-sunday-string()
    (let ((dayspan 0)
          (today (string-to-number (format-time-string "%u"))))
      (cond
       ((> today 0) ; from today till sunday, today included
        (setq dayspan (- 8 today)))
       ((= today 0) ; sunday to sunday
        (setq dayspan 8)))
      (format "+%sd" dayspan)))



  ;; file blocks
  (setq ub/gtd-all-files-list (append
                               `(
                                 ,ub/gtd-horizon-file
                                 ,ub/gtd-bookmark-file
                                 ,ub/gtd-me-file
                                 ,ub/gtd-work-file
                                 ,ub/gtd-inbox-file
                                 ,ub/gtd-inbox-laptop-file
                                 )
                               ub/doom-emacs-files-list
                               ))
  (defun ub/gtd-all-files-list-func ()
    ub/gtd-all-files-list)
  (setq ub/gtd-main-files-list (append
                                `(
                                  ,ub/gtd-horizon-file
                                  ,ub/gtd-me-file
                                  ,ub/gtd-work-file
                                  ,ub/gtd-inbox-file
                                  ,ub/gtd-inbox-laptop-file
                                  )
                                ))
  (defun ub/gtd-main-files-list-func ()
    ub/gtd-main-files-list)
  (setq ub/org-agenda-files-gtd-main-code-block
        `((org-agenda-files ',ub/gtd-main-files-list)))
  (setq ub/org-agenda-files-gtd-main-journal-main (append ub/gtd-main-files-list
                                                          ub/roam-dailies-main-files-list
                                                          ))
  (setq ub/org-agenda-files-gtd-main-journal-main-code-block
        `((org-agenda-files ',ub/org-agenda-files-gtd-main-journal-main)))
  (setq ub/org-agenda-files-gtd-main-journal-main-zk-captures (append
                                                               `(,ub/gtd-inbox-file
                                                                 ,ub/gtd-inbox-laptop-file
                                                                 )
                                                               ub/gtd-main-files-list
                                                               ub/roam-dailies-main-files-list
                                                               ))
  (setq ub/org-agenda-files-gtd-main-journal-main-zk-captures-code-block
        `((org-agenda-files ',ub/org-agenda-files-gtd-main-journal-main-zk-captures)))
  (setq ub/org-agenda-files-gtd-all-code-block
        `((org-agenda-files ',ub/gtd-all-files-list)))
  (setq ub/org-agenda-files-gtd-all-journal-main (append ub/gtd-all-files-list
                                                         ub/roam-dailies-main-files-list
                                                         ))
  (setq ub/org-agenda-files-gtd-all-journal-main-code-block
        `((org-agenda-files ',ub/org-agenda-files-gtd-all-journal-main)))
  (setq ub/org-agenda-files-gtd-all-journal-all (append ub/gtd-all-files-list
                                                        ub/roam-dailies-main-files-list
                                                        ub/roam-dailies-archive-files-list
                                                        ))
  (setq ub/org-agenda-files-gtd-all-journal-all-code-block
        `((org-agenda-files ',ub/org-agenda-files-gtd-all-journal-all)))
  (setq ub/org-agenda-files-gtd-all-journal-all-zk-captures (append
                                                             `(,ub/gtd-inbox-file
                                                               ,ub/gtd-inbox-laptop-file
                                                               )
                                                             ub/gtd-all-files-list
                                                             ub/roam-dailies-main-files-list
                                                             ub/roam-dailies-archive-files-list
                                                             ))
  (setq ub/org-agenda-files-gtd-all-journal-all-zk-captures-code-block
        `((org-agenda-files ',ub/org-agenda-files-gtd-all-journal-all-zk-captures)))

  (setq ub/org-agenda-files-gtd-all-journal-main-zk-all (append ub/gtd-all-files-list
                                                                ub/roam-dailies-main-files-list
                                                                ub/roam-all-files-list
                                                                ))
  (setq ub/org-agenda-files-gtd-all-journal-main-zk-all-code-block
        `((org-agenda-files ',ub/org-agenda-files-gtd-all-journal-main-zk-all)))

  (setq ub/org-agenda-files-gtd-all-journal-main-zk-all-doom-emacs-all (append ub/gtd-all-files-list
                                                                               ub/roam-dailies-main-files-list
                                                                               ub/roam-all-files-list
                                                                               ub/doom-emacs-files-list
                                                                               ))
  (setq ub/org-agenda-files-gtd-all-journal-main-zk-all-doom-emacs-all-code-block
        `((org-agenda-files ',ub/org-agenda-files-gtd-all-journal-main-zk-all-doom-emacs-all)))


  ;; org-id
  (setq org-id-locations-file (expand-file-name ".orgids" org-directory))
  (require 'org-id)
  (setq org-id-files ub/org-agenda-files-gtd-all-journal-main-zk-all)
  ;; BUG throws (wrong-type-argument listp 126) error
  ;;(setq org-generic-id-locations-file (concat org-directory ".org-generic-id-locations"))

  )



;; org-super-agenda package
(use-package! org-super-agenda
  :after org
  :config
  (org-super-agenda-mode t)
  (require 'org-habit))

(after! org-super-agenda
  ;; tag hierarchy and group don't work
  ;; a fix from https://github.com/alphapapa/org-super-agenda/issues/136
  ;; still doesn't work for tag grouping with regular expressions such as Project : {P@.+}
  (org-super-agenda--defgroup tag
                              "Group items that match any of the given tags. Argument may be a string or list of strings."
                              :let* ((target-tags (-flatten (cons args (mapcar (lambda (arg) (funcall #'org-tags-expand arg t)) args)))))
                              :section-name (concat "Tags: " (s-join " OR " args))
                              :test (seq-intersection (org-super-agenda--get-tags item) target-tags 'cl-equalp)))


(use-package! org
  :config
  (setq org-agenda-files ub/org-agenda-files-gtd-main-journal-main-zk-captures) ; default agenda file
  (setq org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
                                      (todo todo-state-down priority-down timestamp-up)
                                      (tags todo-state-down priority-down timestamp-up)
                                      (search todo-state-down priority-down timestamp-up)))

  (setq org-agenda-skip-deadline-prewarning-if-scheduled nil)
  (setq org-agenda-start-on-weekday 1)
  (setq calendar-week-start-day 1)
  (setq org-agenda-start-day "+0d")
  (setq org-agenda-sticky t)
  (setq org-agenda-window-setup (quote current-window))
  (setq org-deadline-warning-days 365)
  (setq org-extend-today-until 4)
  (setq org-use-effective-time t)
  ;;(setq org-habit-show-habits-only-for-today t)
  ;;(setq org-agenda-show-future-repeats 'next)

  (setq org-agenda-prefix-format (quote ((agenda . " %i %-15:c%-12t% s")
                                         (todo . " %i %-15:c%-12t% s")
                                         (tags . "%i %-15:c%-12t% s")
                                         (search . " %i %-15:c%-12t% s"))))

  (setq org-agenda-format-date "\n :calendar: %Y-%m-%d %A")
  (setq org-agenda-archives-mode nil) ;; NOTE setting t break org-agenda :/
  (setq org-super-agenda-date-format "%d %b %Y - %A")
  (setq org-agenda-time-grid '((daily today require-timed remove-match) (1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300) "......" "----------------"))
  (setq org-agenda-current-time-string "now <- - - - - - - - - - - - - - -")
  (setq org-agenda-show-current-time-in-grid t)
  (setq org-agenda-log-mode-add-notes nil)
  (setq org-agenda-clockreport-parameter-plist '(:stepskip0 t :link t :maxlevel 3 :fileskip0 t))

  (defun ub/days-to-next-sunday()
    (let ((dayspan 0)
          (today (string-to-number (format-time-string "%u"))))
      (cond
       ((> today 0) ; from today till sunday, today included
        (setq dayspan (- 8 today)))
       ((= today 0) ; sunday to sunday
        (setq dayspan 8)))))

  (defun ub/days-to-past-monday ()
    (- 7 (ub/days-to-next-sunday)))


  (defun ub/days-to-next-sunday-string()
    (format "+%sd" (ub/days-to-next-sunday)))

  (defun ub/days-to-past-monday-string()
    (format "-%sd" (ub/days-to-past-monday)))

  (defun ub/is-it-still-today ()
    (let ((agenda-starting-day 0)
          (current-hour (string-to-number (format-time-string "%H"))))
      (cond
       ((and (>= current-hour 0) (<= current-hour 4)) ; after midnight
        (setq agenda-starting-day "-1d"))
       (t ; during day
        (setq agenda-starting-day "+0d")))))

  (defun ub/is-it-still-tomorrow ()
    (let ((agenda-starting-day 0)
          (current-hour (string-to-number (format-time-string "%H"))))
      (cond
       ((and (>= current-hour 0) (<= current-hour 4)) ; after midnight
        (setq agenda-starting-day "+0d"))
       (t ; during day
        (setq agenda-starting-day "+1d")))))

  (defun ub/today-date ()
    (let ((current-hour (string-to-number (format-time-string "%H"))))
      (cond
       ((and (>= current-hour 0) (<= current-hour 4)) ; after midnight
        (setq ub/today-date (ts-format "%Y-%m-%d" (ts-adjust 'day -1 (ts-now)))))
       (t ; during day
        (setq ub/today-date (ts-format "%Y-%m-%d" (ts-now)))))))

  (defun ub/tomorrow-date ()
    (let ((current-hour (string-to-number (format-time-string "%H"))))
      (cond
       ((and (>= current-hour 0) (<= current-hour 4)) ; after midnight
        (setq ub/tomorrow-date (ts-format "%Y-%m-%d" (ts-now))))
       (t ; during day
        (setq ub/tomorrow-date (ts-format "%Y-%m-%d" (ts-adjust 'day 1 (ts-now))))
        ))))

  (setq ub/org-agenda-settings-clocked-report
        `(
          ;;(org-agenda-format-date "%Y-%m-%d %A")
          (org-agenda-show-all-dates t)
          (org-agenda-start-with-log-mode t)
          (org-agenda-show-log t)
          (org-agenda-time-grid nil)
          (org-agenda-log-mode-items '(clock))
          ;;(org-agenda-start-with-clockreport-mode t)
          (org-agenda-archives-mode nil)
          ;; I don't care if an entry was archived
          (org-agenda-hide-tags-regexp
           (concat org-agenda-hide-tags-regexp
                   "\\|ARCHIVE"))
          (org-super-agenda-groups
           '(
             (:discard (:file-path ".*_archive"))
             (:discard (:log nil))
             (:name "" :anything t)))))

  (setq ub/org-agenda-settings-habit-all
        `(
          (org-agenda-span 'day)
          ;;(org-agenda-block-separator nil)
          (org-agenda-format-date "")
          (org-agenda-time-grid nil)
          (org-habit-show-habits t)
          (org-habit-show-all-today t)
          ;; (org-habit-show-all-today t)
          ;;(org-deadline-warning-days 365)
          ;; (org-scheduled-past-days 0)
          (org-agenda-sorting-strategy '((agenda category-keep))) ; NOTE Doesn't work for sorting habits :/
          (org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|pinned\\|h@.*\\|habit"))
          (org-agenda-overriding-header "\n  :repeat: Habits") ;; 2 spaces = level 1 heading
          (org-agenda-prefix-format (quote ((agenda . "      %i %-12:c")))) ;; 6 spaces = level 2 content
          (org-super-agenda-groups
           '(
             (:discard (:not (:tag "habit")))
             (:discard (:tag "no_agenda"))
             (:discard (:tag "archive"))
             (:name "    Pinned\n" :and (:todo "TODO" :tag "habit" :tag "pinned"))
             (:name "    Others\n" :anything t)
             ;;(:name "Daily" :order 4 :and (:todo "T@NEXT" :tag "h@daily"))
             ;;(:name "More" :order 3 :and (:todo "T@NEXT" :tag "habit" :scheduled today))
             ;;(:discard (:anything t))
             ))))

  (setq ub/org-agenda-settings-todays-schedule
        `(
          (org-agenda-format-date "")
          (org-agenda-span 2)
          (org-agenda-time-grid '((daily today) () "......" "----------------"))
          ;;(org-agenda-time-grid '((daily today remove-match) (1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300) "......" "----------------"))
          (org-agenda-current-time-string "now <- - - - - - - - - - - - - - -")
          (org-agenda-show-current-time-in-grid t)
          (org-habit-show-habits t)
          (org-habit-show-habits-only-for-today t)
          ;;(org-habit-show-all-today t)
          (org-agenda-sorting-strategy '((agenda time-up habit-down priority-down category-keep)))
          (org-super-agenda-groups
           '(
             ;;(:discard (:tag "habit"))
             ;; NOTE pinned habits doesn't require scheduling since it has its own section
             (:discard (:and (:tag "habit" :tag "pinned")))
             (:discard (:tag "no_agenda"))
             (:discard (:tag "archive"))
             ;;(:discard (:tag "reminder"))
             ;;(:discard (:tag "j@1todaygreat"))
             ;;(:discard (:tag "j@tomorrowself"))
             ;;(:discard (:todo "T@HOLD"))
             ;; NOTE filter all todos that doesn't have today's date - except non todo items which are fixed scheduled events like Lunch, Meditation etc. If you are going to do a todo item then schedule it today. Use reminder section to what can be scheduled today from todos
             ;;(:discard (:and (:todo t :not (:date today)))) ;; NOTE discards item that has DO-DATE today but not scheduled for today so killed
             ;; NOTE time-grid t doesn't work when filter with j@schedule so now -> isn't showed, only works with alone
             ;; (:discard (:and (:todo nil :not (:tag "fixed_schedule"))))
             (:name "" :order 1 :time-grid t)
             (:name "" :order 2 :date today)
             (:discard (:anything t))))))

  (setq ub/org-agenda-settings-tasks-important-urgent-wo-date
        `(
          (org-agenda-overriding-header "")
          (org-deadline-warning-days 365)
          (org-agenda-sorting-strategy '((todo todo-state-down priority-down timestamp-up)))
          (org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|important\\|urgent"))
          (org-super-agenda-groups
           '(
             ;; (:discard (:habit t))
             ;; (:discard (:tag "recurring"))
             ;; (:discard (:tag "pj_title"))
             ;; (:discard (:date t))
             ;; (:discard (:scheduled t))
             ;; (:discard (:deadline t))
             (:name "    Important & Urgent w/o Date\n"
              ;;:discard (:date t)
              :discard (:scheduled t)
              :discard (:deadline t)
              :and (:tag "important" :tag "urgent")
              :order 1)
             (:name "    Important w/o Date\n"
              ;;:discard (:date t)
              :discard (:scheduled t)
              :discard (:deadline t)
              :and (:tag "important")
              :order 3)
             (:name "    Urgent w/o Date\n"
              ;;:discard (:date t)
              :discard (:scheduled t)
              :discard (:deadline t)
              :and (:tag "urgent")
              :order 2)
             (:discard (:anything t))))))

  (setq ub/org-agenda-settings-plan
        `(
          (org-agenda-time-grid nil)
          (org-deadline-warning-days 0)
          (org-habit-show-habits-only-for-today nil) ;; NOTE will have same effect of org-agenda-show-future-repeats
          (org-habit-show-habits t)
          ;; (org-habit-show-all-today nil)
          ;; (org-scheduled-past-days 0)
          ;; (org-agenda-skip-deadline-if-done t)
          ;; (org-agenda-skip-scheduled-(if t)-done t)
          (org-agenda-show-future-repeats t)
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
          (org-agenda-todo-ignore-timestamp 'past)
          (org-agenda-sorting-strategy '((agenda habit-down timestamp-up category-keep)))
          (org-super-agenda-groups
           '(
             (:discard (:tag "no_agenda"))
             (:discard (:tag "archive"))
             (:discard (:tag "j@tomorrowself"))
             (:discard (:tag "j@1todaygreat"))
             (:discard (:tag "r@daily"))
             (:discard (:tag "r@batch"))
             (:discard (:tag "h@daily"))
             (:discard (:tag "h@batch"))
             ;;(:discard (:todo "T@HOLD"))
             ;; NOTE if you discard habits aand recurrings, it will also discard future repeats so exclude them
             (:discard (:and (:deadline past :not (:tag "habit" :tag "recurring"))))
             ;;(:discard (:and (:scheduled past :not (:tag "habit" :tag "recurring"))))
             ;; TODO: how to filter ancestors with J@KILL/J@DONE
             ;; IDEA: automatically tag as no_agenda when set project as done?
             ;;(:discard (:and (:tag "project" :todo "J@DONE" :children t))) ;; children take the project heading, not the children
             ;;(:discard (:and (:tag "project" :todo "J@KILL" :children t)))
             ;; (:discard (:deadline past))
             ;; (:discard (:scheduled past))
             ;;(:discard (:todo "KILL"))
             (:name ""
              :anything t)))))

  (setq ub/org-agenda-settings-tasks-deadline-type
        `(
          (org-agenda-overriding-header "\n  :ghost: Attention Beggars") ;; level 1 heading
          (org-agenda-format-date "")
          (org-agenda-span 'day)
          (org-agenda-time-grid nil)
          (org-deadline-warning-days 365)
          (org-agenda-skip-scheduled-if-deadline-is-shown t)
          (org-agenda-sorting-strategy '((agenda deadline-up scheduled-down)))
          (org-super-agenda-groups
           '(
             (:discard (:tag "no_agenda"))
             (:discard (:tag "archive"))
             (:discard (:and (:tag "habit" :tag "h@daily")))
             (:name "    :radioactive_sign: :angry: Past (Non-Daily) Pinned Habits\n"
              :and (:scheduled past :tag "habit" :tag "pinned")
              :order 9)
             (:discard (:tag "habit"))

             (:name "    :radioactive_sign: :shit::shit::shit: Past Hard Deadlines \n"
              :and (:deadline past :tag "dead@hard")
              :order 1)

             (:name "    :radioactive_sign: :shit: Past Soft Deadlines \n"
              :deadline past
              :order 2)

             (:name "    :radioactive_sign: :kissing_heart: Today Soft Deadlines \n"
              :and (:deadline today)
              :order 4)

             (:name "    :radioactive_sign: :kissing_heart::kissing_heart: Today Hard Deadlines \n"
              :and (:deadline today :tag "dead@hard")
              :order 3)

             (:name "    :radioactive_sign: :alarm-clock: Upcoming Appointments \n"
              :category ("gcal_app" "gcal_jule_app")
              :order 5)

             (:name "    :building_construction: :construction_worker_tone5: Active Future Deadlines - Should Be Working On!\n"
              :and (:scheduled today :deadline future)
              :and (:scheduled past :deadline future)
              :order 6)
             (:name "    :radioactive_sign: :confused: Ambiguous Future Deadlines - Haven't Scheduled Yet!\n"
              :and (:scheduled nil :deadline future)
              :order 7)
             (:name "    :palm-tree: Chill Future Scheduled Deadlines - Still Have Time\n"
              :and (:scheduled future :deadline future)
              :order 8)

             (:name "    :radioactive_sign: :facepalm_tone4: Important &/ Urgent Tasks w/o Deadline \n"
              :and (:tag "important" :deadline nil)
              :and (:tag "urgent" :deadline nil)
              :order 10)


             (:name "    :radioactive_sign: :rolling_eyes: Past Recurring Tasks\n"
              :and (:scheduled past :tag "recurring")
              :order 11)


             (:name "    :interrobang: Waiting w/o Deadline - Waiting for Forever?\n"
              :and (:todo "T@WAIT" :deadline nil)
              :order 12)
             (:name "    :interrobang: In-Progress w/o Deadline - Working on Forever?\n"
              :and (:todo "T@IN" :deadline nil)
              :order 13)

             (:name "    :interrobang: Hold w/o Deadline - Working on Forever?\n"
              :and (:todo "T@HOLD" :deadline nil)
              :order 14)

             (:name "    :interrobang: Knock Knock - Tasks w/o deadline in last 3 weeks?\n" ;scheduled it already but haven't take actions yet because you haven't set deadline !
              :and (;;:scheduled past
                    :deadline nil
                    :scheduled (after ,(ts-format "%Y-%m-%d" (ts-adjust 'day -21 (ts-now))))
                    :scheduled (before ,(ts-format "%Y-%m-%d" (ts-adjust 'day 0 (ts-now))))
                    :not (:todo "I@LINK")
                    :not (:todo "T@IN")
                    :not (:todo "T@HOLD")
                    :not (:todo "J@IN")
                    :not (:todo "J@HOLD"))
              :order 15)

             (:name "    :interrobang: Self-deceptiving :S - Tasks w/o deadline and passed more than 3 weeks?\n" ;scheduled it already but haven't take actions yet because you haven't set deadline !
              :and (;;:scheduled past
                    :deadline nil
                    :scheduled (before ,(ts-format "%Y-%m-%d" (ts-adjust 'day -21 (ts-now))))
                    :not (:todo "I@LINK")
                    :not (:todo "T@IN")
                    :not (:todo "T@HOLD")
                    :not (:todo "J@IN")
                    :not (:todo "J@HOLD"))
              :order 16)

             ;; (:name "    :interrobang: Procrastinating Knowingly :P - Tasks w/ Deadline\n" ;scheduled it already but haven't take actions yet although you set deadline!
             ;;  :and (:scheduled past :deadline future :todo "TODO")
             ;;  ;;:and (:scheduled past :deadline future :todo "T@NEXT")
             ;;  :and (:scheduled past :deadline future :todo "PROJ")
             ;;  ;;:and (:scheduled past :deadline future :todo "j@NEXT")
             ;;  :order 13)

             (:discard (:anything t))
             ))))

  (setq ub/org-agenda-settings-tasks-forgot-to-schedule-or-deadline
        `(
          (org-super-agenda-groups
           '(
             (:discard (:tag "habit")) ; warning it doesn't include h@.+ :/
             (:discard (:and (:deadline t)))
             (:discard (:and (:scheduled t)))
             (:discard (:tag "pj_title"))
             (:discard (:tag "recurring"))
             (:name "    Started but Forgot to Schedule/Deadline :/ - Commit Properly!"
              :and (:todo "T@IN" :tag "important" :tag "urgent")
              :and (:todo "T@IN" :tag "important")
              :and (:todo "T@IN" :tag "urgent")
              :and (:todo "T@WAIT" :tag "important" :tag "urgent")
              :and (:todo "T@WAIT" :tag "important")
              :and (:todo "T@WAIT" :tag "urgent")
              :and (:todo "T@HOLD" :tag "important" :tag "urgent")
              :and (:todo "T@HOLD" :tag "important")
              :and (:todo "T@HOLD" :tag "urgent")
              :todo "T@IN"
              :todo "T@WAIT"
              :todo "T@HOLD"
              :order 10)
             (:discard (:anything t))))))

  (setq ub/org-agenda-settings-project-roadmap
        `(
          (org-agenda-format-date "")
          (org-agenda-span 'day)
          (org-agenda-time-grid nil)
          (org-deadline-warning-days 365)
          (org-agenda-skip-scheduled-if-deadline-is-shown t)
          (org-super-agenda-groups
           '((:auto-planning t)))))

  (setq ub/org-agenda-settings-project-tasks-outline-path
        `(
          (org-super-agenda-groups
           '(
             ;;(:discard (:todo "LOG"))
             ;;(:discard (:and (:tag "project" :not (:tag "pj_title") :todo "J@HOLD")))
             ;;(:discard (:and (:tag "project" :todo "J@HOLD")))
             ;;(:auto-category t)
             (:auto-outline-path t)))))

  ;; empty custom agenda list
  (setq org-agenda-custom-commands
        `())

  (add-to-list
   'org-agenda-custom-commands
   `("o" "Today"
     (

      (agenda "" ,(append ub/org-agenda-files-gtd-main-journal-main-code-block ;; ub/org-agenda-settings-todays-schedule
                          `(
                            ;; NOTE don't show scheduled/deadline string since there is coloring
                            ;;(org-agenda-prefix-format (quote ((agenda . "      %i %-15:c%-12t% s")))) ;; 6 spaces = level 2 content
                            (org-agenda-prefix-format (quote ((agenda . "    %i %-15:c%-12t")))) ;; 4 spaces = level 1 content
                            ;;(org-agenda-block-separator nil)
                            ;;(org-agenda-overriding-header "\n  🤙🏿 Today's Agenda\n")
                            (org-agenda-overriding-header "")
                            ;;(org-agenda-format-date "")
                            ;;(org-agenda-format-date "\n    :calendar: %Y-%m-%d %A\n") ;; 4 spaces = level 2 heading
                            (org-agenda-format-date "\n  :calendar: Today's Agenda - %Y-%m-%d %A\n") ;; 2 spaces = level 1 heading
                            (org-agenda-span 1)
                            (org-agenda-start-day (ub/is-it-still-today))
                            ;;(org-agenda-time-grid nil)
                            ;;(org-agenda-time-grid '((daily today remove-match) (1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300) "......" "----------------"))
                            (org-agenda-time-grid '((daily today require-timed) () "......" "----------------")) ;; no outline for time-grid, just show tasks
                            (org-agenda-current-time-string "now <- - - - - - - - - - - - - - -")
                            (org-agenda-show-current-time-in-grid t)
                            (org-habit-show-habits t)
                            (org-habit-show-habits-only-for-today t)
                            (org-agenda-block-separator nil)
                            ;;(org-habit-show-all-today t)
                            (org-agenda-sorting-strategy '((agenda time-up habit-down priority-down category-keep)))
                            (org-super-agenda-groups
                             '(
                               (:discard (:tag "no_agenda"))
                               (:discard (:tag "archive"))
                               ;;(:discard (:and (:tag "habit" :tag "h@daily")))

                               ;;(:discard (:tag "reminder"))
                               ;;(:discard (:tag "j@1todaygreat"))
                               ;;(:discard (:tag "j@tomorrowself"))
                               ;;(:discard (:todo "T@HOLD"))
                               ;; NOTE filter all todos that doesn't have today's date
                               ;; - except non todo items which are fixed scheduled events
                               ;; like Lunch, Meditation etc. If you are going to do a todo item then schedule it today.
                               ;; Use reminder section to what can be scheduled today from todos
                               ;;(:discard (:and (:todo t :not (:date today))))
                               ;; NOTE discards item that has DO-DATE today but not scheduled for today so killed
                               ;; NOTE time-grid t doesn't work when filter with j@schedule so now
                               ;; -> isn't showed, only works with alone
                               ;; (:discard (:and (:todo nil :not (:tag "fixed_schedule"))))

                               (:name " :repeat: Recurring\n" :order 7 :and (:time-grid t :tag "recurring")) ;; NOTE you cannot catch recurring tasks with :date today
                               (:name "" :order 1 :time-grid t)
                               (:name " :frog: of the Day\n" :order 2 :and (:tag "frog" :date today))
                               (:name " :repeat: Daily Habits\n" :order 6 :and (:tag "habit" :tag "h@daily"))
                               (:name " :biohazard_sign: Hard Deadlines\n" :order 4 :and (:deadline today :tag "dead@hard"))
                               (:name " :ghost: Soft Deadlines\n" :order 5 :deadline today)
                               (:name " :inbox-tray: Tasks\n" :order 3 :date today)
                               (:discard (:anything t)))))))


      ;; focus w/ dates
      (agenda ""
              ,(append ub/org-agenda-files-gtd-main-journal-main-code-block
                       `(
                         ;; from attention beggars
                         (org-agenda-format-date "")
                         (org-agenda-span 'day)
                         (org-agenda-time-grid nil)
                         (org-deadline-warning-days 365)
                         (org-agenda-skip-scheduled-if-deadline-is-shown t)

                         (org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|focus"))
                         ;;(org-agenda-prefix-format (quote ((tags . "    %i %-15:c")))) ;; 4 spaces = level 1 content
                         (org-agenda-prefix-format (quote ((agenda . "    %i %-15:c%-t% s")))) ;; 4 spaces = level 1 content
                         ;;(org-agenda-overriding-header "\n  :dart: Focus w/ dates") ;; 2 spaces = level 1 heading
                         (org-agenda-overriding-header "\n  :dart: Focus") ;; 2 spaces = level 1 heading
                         (org-agenda-block-separator nil)
                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                         (org-super-agenda-groups
                          '(
                            (:discard (:tag "no_agenda"))
                            ;; gather focus tag
                            (:name "" :tag "focus")
                            (:discard (:anything t))
                            )
                          )
                         )))

      ;; focus w/o dates
      (tags "focus"
            ,(append ub/org-agenda-files-gtd-main-journal-main-code-block
                     `((org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|focus"))
                       (org-agenda-prefix-format (quote ((tags . "    %i %-15:c")))) ;; 4 spaces = level 1 content
                       ;;(org-agenda-overriding-header "\n  :dart: Focus w/o dates\n") ;; 2 spaces = level 1 heading
                       (org-agenda-overriding-header "") ;; 2 spaces = level 1 heading
                       (org-agenda-block-separator nil)
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                       (org-super-agenda-groups
                        '(
                          (:discard (:tag "no_agenda"))
                          ;; discard if it has date
                          (:discard (:date t))
                          (:name "" :anything t)
                          )
                        )
                       )))


      (agenda "" ,(append ub/org-agenda-files-gtd-main-journal-main-code-block ub/org-agenda-settings-clocked-report
                          `(
                            (org-agenda-sorting-strategy '((time-up)))
                            (org-agenda-overriding-header "\n  :timer_clock: Today's Clocked\n") ;; 4 spaces = level 2 heading
                            (org-agenda-span 'day)
                            (org-agenda-block-separator nil)
                            (org-agenda-prefix-format (quote ((agenda . "      %i %-15:c%-12t% s")))) ;; 6 spaces = level 2 content
                            (org-agenda-format-date ""))))

      ))

   t)

  (add-to-list
   'org-agenda-custom-commands
   `("w" "Tomorrow"
     (
      (agenda "" ,(append ub/org-agenda-files-gtd-main-journal-main-code-block ;; ub/org-agenda-settings-todays-schedule
                          `(
                            ;; NOTE don't show scheduled/deadline string since there is coloring
                            (org-agenda-prefix-format (quote ((agenda . "      %i %-15:c%-12t")))) ;; 6 spaces = level 2 content
                            ;;(org-agenda-overriding-header "\n  🤙🏿 Tomorrow's Agenda\n")
                            (org-agenda-overriding-header "")
                            ;;(org-agenda-format-date "\n    :calendar: %Y-%m-%d %A\n") ;; 4 spaces = level 2 heading
                            (org-agenda-format-date "\n  :calendar: Tomorrow's Agenda - %Y-%m-%d %A\n") ;; 2 spaces = level 1 heading
                            (org-agenda-span 1)
                            (org-agenda-start-day (ub/is-it-still-tomorrow))
                            (org-agenda-time-grid nil)
                            ;;(org-agenda-time-grid '((daily weekly today remove-match) (1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300) "......" "----------------"))
                            ;;(org-agenda-current-time-string "now <- - - - - - - - - - - - - - -")
                            ;;(org-agenda-show-current-time-in-grid t)
                            (org-habit-show-habits t)
                            (org-habit-show-habits-only-for-today nil)
                            (org-agenda-block-separator nil)
                            ;;(org-habit-show-all-today t)
                            (org-agenda-sorting-strategy '((agenda time-up habit-down priority-down category-keep)))
                            (org-super-agenda-groups
                             '(
                               (:discard (:tag "no_agenda"))
                               (:discard (:tag "archive"))
                               (:name "" :anything t)
                               )))))
      ))

   t)

  ;; (add-to-list
  ;;  'org-agenda-custom-commands
  ;;  `("d" "Dashboard"
  ;;    ((tags "focus"
  ;;           ,(append ub/org-agenda-files-gtd-main-journal-main-code-block
  ;;                    `((org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|focus"))
  ;;                      (org-agenda-prefix-format (quote ((tags . "    %i %-15:c")))) ;; 4 spaces = level 1 content
  ;;                      (org-agenda-overriding-header "\n  :dart: Focus\n") ;; 2 spaces = level 1 heading
  ;;                      (org-agenda-block-separator nil)
  ;;                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done)))))

  ;;     (tags "pj_title"
  ;;           ,(append ub/org-agenda-files-gtd-main-code-block
  ;;                    `(;;(org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|focus"))
  ;;                      (org-agenda-prefix-format (quote ((tags . "    %i %-15:c")))) ;; 4 spaces = level 1 content
  ;;                      (org-agenda-overriding-header "\n  :radioactive_sign: Active Projects\n") ;; 2 spaces = level 1 heading
  ;;                      (org-agenda-block-separator nil)
  ;;                      ;;(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
  ;;                      ;;
  ;;                      (org-super-agenda-groups
  ;;                       '((:discard (:tag "focus")) ;; avoiding duplicates since I show it in the focus view
  ;;                         (:name "" :todo "J@IN")
  ;;                         (:discard (:anything t))))
  ;;                      )))


  ;;     (tags "lesson_learned|j@lesson_learned"
  ;;           ,(append ub/org-agenda-files-gtd-main-journal-main-zk-captures-code-block
  ;;                    `(;;(org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|focus"))
  ;;                      (org-agenda-prefix-format (quote ((tags . "    %i %-15:c")))) ;; 4 spaces = level 1 content
  ;;                      (org-agenda-overriding-header "\n  :facepalm_tone2: Lesson Learned\n") ;; 2 spaces = level 1 heading
  ;;                      (org-agenda-block-separator nil)
  ;;                      ;;(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
  ;;                      ;;
  ;;                      (org-super-agenda-groups
  ;;                       '(
  ;;                         (:name "" :anything t)))
  ;;                      )))


  ;;     (tags "+pinned+reminder"
  ;;           ((org-agenda-files '(,ub/gtd-horizon-file))
  ;;            (org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|reminder\\|pinned"))
  ;;            (org-agenda-prefix-format (quote ((tags . "      %i %-15:c")))) ;; 6 spaces = level 2 content
  ;;            (org-agenda-span 1)
  ;;            (org-agenda-format-date "")
  ;;            (org-agenda-show-future-repeats t)
  ;;            (org-agenda-block-separator nil)
  ;;            (org-agenda-overriding-header "\n  :martial_arts_uniform: Things to Keep in Mind\n\n    :warning: Tame the :monkey: !\n") ;; 2 spaces = level 1 heading + newline + 4 spaces = level 2 heading
  ;;            (org-super-agenda-groups
  ;;             '((:name "" :anything t)))))
  ;;     ;;(:discard (:anything t))


  ;;     (tags "j@1todaygreat"
  ;;           ((org-agenda-files '(,ub/daily-today-file))
  ;;            (org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|j@1todaygreat"))
  ;;            (org-agenda-prefix-format (quote ((tags . "      %i %-15:c")))) ;; 6 spaces = level 2 content
  ;;            (org-agenda-overriding-header "\n    :rocket: 1 Thing That Would Make Today Great\n") ;; 4 spaces = level 2 heading
  ;;            (org-agenda-block-separator nil)
  ;;            (org-super-agenda-groups
  ;;             '((:name "" :date today)
  ;;               (:discard (:anything t))))))

  ;;     (tags "frog"
  ;;           ,(append ub/org-agenda-files-gtd-main-journal-main-code-block
  ;;                    `((org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|frog"))
  ;;                      (org-agenda-prefix-format (quote ((tags . "      %i %-15:c")))) ;; 6 spaces = level 2 content
  ;;                      (org-agenda-overriding-header "\n    :frog: of the Day\n") ;; 4 spaces = level 2 heading
  ;;                      (org-agenda-block-separator nil)
  ;;                      (org-super-agenda-groups
  ;;                       '((:name "" :scheduled today)
  ;;                         (:discard (:anything t))))
  ;;                      )))

  ;;     (tags "j@tomorrowself"
  ;;           ((org-agenda-files '(,ub/daily-today-file))
  ;;            (org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|j@tomorrowself"))
  ;;            (org-agenda-prefix-format (quote ((tags . "     %i %-15:c")))) ;; 6 spaces = level 2 content
  ;;            (org-agenda-overriding-header "\n    :older_man_tone5: 1 Thing That Would Advice Tomorrow-Self") ;; 4 spaces = level 2 heading
  ;;            (org-agenda-block-separator nil)
  ;;            (org-super-agenda-groups
  ;;             '((:name "" :date today)
  ;;               (:discard (:anything t))))))


  ;;     (tags "frog"
  ;;           ,(append ub/org-agenda-files-gtd-main-journal-main-code-block
  ;;                    `((org-agenda-hide-tags-regexp (concat ub/tags-main-hide "\\|frog"))
  ;;                      (org-agenda-prefix-format (quote ((tags . "      %i %-15:c")))) ;; 6 spaces = level 2 content
  ;;                      (org-agenda-overriding-header "\n    :frog: of the Tomorrow\n") ;; 4 spaces = level 2 heading
  ;;                      (org-agenda-block-separator nil)
  ;;                      (org-super-agenda-groups
  ;;                       '(
  ;;                         (:discard (:scheduled (before ,(ts-format "%Y-%m-%d" (ts-adjust 'day 1 (ts-now))))))
  ;;                         (:discard (:scheduled (after ,(ts-format "%Y-%m-%d" (ts-adjust 'day 1 (ts-now))))))
  ;;                         ;; NOTE cannot pass exact date w/ot before/after? ex:
  ;;                         ;;(:name "" :scheduled (ts-format "%Y-%m-%d" (ts-adjust 'day 1 (ts-now))))
  ;;                         ;;(:discard (:anything t))
  ;;                         (:name "" :anything t))))))

  ;;     ))

  ;;  t)

  (add-to-list
   'org-agenda-custom-commands
   `("C" "Clocked This Week"
     agenda ""
     ;; agenda settings
     ,(append
       ub/org-agenda-files-gtd-main-journal-main-code-block ub/org-agenda-settings-clocked-report
       '((org-agenda-span 'week)
         (org-agenda-overriding-header ":timer_clock: Clocked Review"))))
   t)

  (add-to-list
   'org-agenda-custom-commands
   `("H" "Habits"
     agenda "" ,(append ub/org-agenda-files-gtd-main-journal-main-code-block ub/org-agenda-settings-habit-all
                        `(;;(org-agenda-prefix-format (quote ((agenda . "      %i %-12:c")))) ;; 4 spaces = level 1 content
                          (org-agenda-block-separator nil))))
   t)

  (add-to-list
   'org-agenda-custom-commands
   `("W" "Agenda This Week"
     agenda "" ,(append ub/org-agenda-files-gtd-main-journal-main-code-block ;; ub/org-agenda-settings-todays-schedule
                        ub/org-agenda-settings-clocked-report
                        `((org-agenda-prefix-format (quote ((agenda . "    %i %-15:c%-12t% s")))) ;; 6 spaces = level 2 content
                          ;;(org-agenda-block-separator nil)
                          (org-agenda-overriding-header "\n  🤙🏿 Agenda - This Week\n")
                          (org-agenda-format-date "\n    :calendar: %Y-%m-%d %A\n") ;; 4 spaces = level 2 heading
                          (org-agenda-span 'week)
                          (org-scheduled-past-days 0) ; w/o it, past scheduled showed on today, which makes it viewed twice
                          (org-deadline-warning-days 0) ; w/o it, prewarning deadline showed on today, which makes it viewed twice
                          (org-agenda-sorting-strategy '((agenda time-up priority-down category-keep)))
                          (org-super-agenda-groups
                           '(
                             (:discard (:tag "no_agenda"))
                             (:discard (:tag "archive"))
                             (:discard (:and (:tag "habit" :tag "h@daily")))
                             (:discard (:and (:tag "recurring" :tag "r@daily")))
                             (:discard (:deadline (before
                                                   ,(ts-format
                                                     "%Y-%m-%d"
                                                     (ts-adjust 'day (- (ub/days-to-past-monday)) (ts-now))))))
                             (:name "" :anything t)
                             )))))

   t)

  (add-to-list
   'org-agenda-custom-commands
   `("A" "Attention Beggars"
     ((agenda "" ,(append ub/org-agenda-files-gtd-main-journal-main-code-block ub/org-agenda-settings-tasks-deadline-type
                          `((org-agenda-prefix-format (quote ((agenda . "      %i %-15:c%-t% s"))))))) ;; 6 spaces = level 2 content

      ))

   t)

  )



;; org-ql views
(use-package! org-ql
  :defer t
  :config

  (defun ub/org-agenda-today ()
    (interactive)
    (org-agenda nil "O"))

  (defun ub/org-agenda-attention-beggars ()
    (interactive)
    (org-agenda nil "A"))

  (setq org-ql-views
        (list
         (cons "Deadline                #Agenda" #'ub/org-ql-deadlines-all)
         (cons "Today                   #Agenda" #'ub/org-agenda-today)
         (cons "Attention Beggars       #Agenda" #'ub/org-agenda-attention-beggars)

         (cons "Inbox Past 14d          #Inbox" #'ub/org-ql-captures-14d)
         (cons "Inbox All               #Inbox" #'ub/org-ql-captures-all)

         ;;(cons "Inbox-  Past 14d - Group by Tag" #'ub/org-ql-captures-14d-tags)

         (cons "Follow                  #Bucket"
               (list :buffers-files `(,ub/gtd-inbox-file
                                      ,ub/gtd-inbox-laptop-file
                                      ,ub/gtd-bookmark-file
                                      ,ub/gtd-me-file
                                      ,ub/gtd-work-file
                                      )
                     :query '(and (tags "follow")
                                  (not (children)) ;; filter movie heading
                                  ;;(not (done))
                                  )
                     :title "Follow"
                     :super-groups '((:auto-ts t))
                     :sort '(todo priority)))


         (cons "Research Papers         #Bucket"
               (list :buffers-files `(,ub/gtd-inbox-file
                                      ,ub/gtd-inbox-laptop-file
                                      ,ub/gtd-bookmark-file
                                      )
                     :query '(and (tags "paper" "bib")
                                  (not (children)) ; filter movie heading
                                  )
                     :title "Research Paper"
                     :super-groups '((:auto-ts t))
                     :sort '(todo priority)))

         (cons "Research Talks          #Bucket"
               (list :buffers-files `(,ub/gtd-inbox-file
                                      ,ub/gtd-inbox-laptop-file
                                      ,ub/gtd-bookmark-file
                                      )
                     :query '(and
                              (tags "research")
                              (tags "talk" "watch")
                              (not (children)) ;; filter movie heading
                              ;;(not (done))
                              )
                     :title "Research Talk"
                     :super-groups '((:auto-ts t))
                     :sort '(todo priority)))


         (cons "Movie & Series          #Bucket"
               (list :buffers-files `(,ub/gtd-inbox-file
                                      ,ub/gtd-inbox-laptop-file
                                      ,ub/gtd-bookmark-file
                                      )
                     :query '(and (tags "movie" "series")
                                  (not (children)) ; filter movie heading
                                  )
                     :title "Movie & Series"
                     :super-groups '((:auto-ts t))
                     :sort '(todo priority)))

         (cons "Listen & Watch          #Bucket"
               (list :buffers-files `(,ub/gtd-inbox-file
                                      ,ub/gtd-inbox-laptop-file
                                      ,ub/gtd-bookmark-file
                                      )
                     :query '(and (tags "listen" "watch" "talk")
                                  (not (children)) ; filter movie heading
                                  )
                     :title "Listen & Watch"
                     :super-groups '((:auto-ts t))
                     :sort '(todo priority)))

         (cons "Music                   #Bucket"
               (list :buffers-files `(,ub/gtd-inbox-file
                                      ,ub/gtd-inbox-laptop-file
                                      ,ub/gtd-bookmark-file
                                      )
                     :query '(and (tags "track" "set" "music")
                                  (not (children)) ; filter movie heading
                                  )
                     :title "Music"
                     :super-groups '((:auto-ts t))
                     :sort '(todo priority)))

         (cons "Read                    #Bucket"
               (list :buffers-files `(,ub/gtd-inbox-file
                                      ,ub/gtd-inbox-laptop-file
                                      ,ub/gtd-bookmark-file
                                      )
                     :query '(and (tags "read")
                                  (not (tags "book"))
                                  (not (children)) ;; filter movie heading
                                  ;;(not (done))
                                  )
                     :title "Read"
                     :super-groups '((:auto-ts t))
                     :sort '(todo priority)))

         (cons "Book                    #Bucket"
               (list :buffers-files `(,ub/gtd-inbox-file
                                      ,ub/gtd-inbox-laptop-file
                                      ,ub/gtd-bookmark-file
                                      )
                     :query '(and (tags "book")
                                  (not (children)) ;; filter movie heading
                                  ;;(not (done))
                                  )
                     :title "Book"
                     :super-groups '((:auto-ts t))
                     :sort '(todo priority)))


         (cons "Check out               #Bucket"
               (list :buffers-files `(,ub/gtd-inbox-file
                                      ,ub/gtd-inbox-laptop-file
                                      ,ub/gtd-bookmark-file
                                      )
                     :query '(and (tags "check" "check_later")
                                  (not (children)) ; filter movie heading
                                  )
                     :title "Check out"
                     :super-groups '((:auto-ts t))
                     :sort '(todo priority)))

         (cons "Twits                   #Bucket"
               (list :buffers-files `(,ub/gtd-inbox-file
                                      ,ub/gtd-inbox-laptop-file
                                      )
                     :query '(regexp "t.co" "twitter.com")
                     :title "Twits"
                     :super-groups '((:auto-ts t))
                     :sort '(todo priority)))

         ))
  )


;; fold org-agenda items that obeys the space-level hierarchy with origami
(use-package! origami
  :hook
  (org-agenda-mode . origami-mode)
  ;;(python-mode . origami-mode)  ;; NOTE backtab/S-TAB is binded to de-indent
  :bind (("<backtab>" . origami-recursively-toggle-node)))

(use-package! org-timeblock)
