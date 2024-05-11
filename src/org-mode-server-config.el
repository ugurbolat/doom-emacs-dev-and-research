;;; src/org-mode-server-config.el -*- lexical-binding: t; -*-

(use-package! org-pomodoro
  :config
  ;; NOTE Fix org-pomodoro can't play mp3
  (setq org-pomodoro-start-sound "~/main/org/docs/sound_effects/gong_small_gamelan.au")
  (setq org-pomodoro-finished-sound "~/main/org/docs/sound_effects/gong_bell_large_strike.au")
  (setq org-pomodoro-overtime-sound "~/main/org/docs/sound_effects/gong_distant_dark.au")
  (setq org-pomodoro-short-break-sound "~/main/org/docs/sound_effects/gong_metal_hit.au")
  (setq org-pomodoro-long-break-sound "~/main/org/docs/sound_effects/gong_metal_hit.au")

  (setq org-pomodoro-length 30)
  (setq org-pomodoro-short-break-length 5)
  (setq org-pomodoro-long-break-frequency 4)
  (setq org-pomodoro-start-sound-p t)
  (setq org-pomodoro-finished-sound-p t)
  (setq org-pomodoro-overtime-sound-p t)
  (setq org-pomodoro-short-break-sound-p t)
  (setq org-pomodoro-long-break-sound-p t)
  (setq org-pomodoro-manual-break t)
  (setq org-pomodoro-format "P~%s")
  (setq org-pomodoro-long-break-format "L~%s")
  (setq org-pomodoro-short-break-format "S~%s"))


(use-package! org-clock-budget
  ;;:defer t
  :after org
  :config
  (setq org-clock-budget-daily-budgetable-hours 7))



(use-package! elgantt
  ;;:defer t
  :after org
  :config
  (setq ub/elgantt-files `(,ub/gtd-me-file ,ub/gtd-work-file))

  (setq elgantt-agenda-files ub/elgantt-files)
  ;;(setq elgantt-start-date "2021-05-01")
  (setq elgantt-header-type 'outline)
  (setq elgantt-insert-blank-line-between-top-level-header t)
  (setq elgantt-startup-folded nil)
  (setq elgantt-show-header-depth t)
  (setq elgantt-draw-overarching-headers t)
  )


;; previous version
;; REF: https://www.reddit.com/r/orgmode/comments/8keyke/tip_org_clock_on_desktop_gnome_topbar/
;; new version with new gnome version e.g., 45, etc.
;; GNOME extension: https://extensions.gnome.org/extension/5018/simple-message/
(when (eq system-type 'gnu/linux)
  (interactive)
  (if (file-directory-p
       (expand-file-name "~/.local/share/gnome-shell/extensions/simple-message@freddez"))
      (progn
        (defun current-task-to-status ()
          (interactive)
          (if (fboundp 'org-clocking-p)
              (if (org-clocking-p)
                  (call-process "dconf" nil nil nil "write"
                                "/org/gnome/shell/extensions/simple-message/message"
                                (concat "'" (org-clock-get-clock-string) "'"))
                (call-process "dconf" nil nil nil "write"
                              "/org/gnome/shell/extensions/simple-message/message"
                              "'No active clock'"))))
        (run-with-timer 0 60 'current-task-to-status)
        (add-hook 'org-clock-in-hook 'current-task-to-status)
        (add-hook 'org-clock-out-hook 'current-task-to-status)
        (add-hook 'org-clock-cancel-hook 'current-task-to-status)
        (add-hook 'org-clock-goto-hook 'current-task-to-status))
    (message "Simple Message GNOME extension is not installed.
Install it from https://extensions.gnome.org/extension/5018/simple-message/")))


;; TODO calendar integration, gcal, outlook, etc. w/ org-caldav?
;; (use-package! org-caldav
;;   :config
;;   (setq org-caldav-url "https://gurlat.xyz/nextcloud/remote.php/dav/calendars/bolatu")
;;   ;;(setq org-caldav-calendar-id "appointments")
;;   ;;(setq org-caldav-inbox "~/main/org/tmp/org-caldav-deneme-inbox.org")
;;   ;;(setq org-caldav-files '("~/main/org/tmp/inbox.org"))
;;   (setq org-icalendar-timezone "Europe/Berlin")
;;   (setq org-icalendar-include-todo 'all
;;         org-caldav-sync-todo t)
;;   ;; ;; example of syncing multiple calendars
;;   (setq org-caldav-calendars
;;         '((:calendar-id "appointments" :files ("~/main/org/tmp/org-caldav/appointments.org")
;;                         :inbox "~/main/org/tmp/org-caldav/appointments_inbox.org")
;;           (:calendar-id "tasks"
;;                         :files ("~/main/org/tmp/org-caldav/todo.org") ;"~/org/play.org")
;;                         ;;:skip-conditions (regexp "soccer")
;;                         :inbox "~/main/org/tmp/org-caldav/todo_inbox.org")
;;           ))
;;   )
