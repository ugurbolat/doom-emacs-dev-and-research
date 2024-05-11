;;; src/org-roam-autoload.el -*- lexical-binding: t; -*-



;;;###autoload
(defun ub/org-roam-pick-dir ()
  "Select roam directories from a list"
  (interactive)
  (let* ((selected-dir (read-directory-name "Select directory: " nil nil t))
         (roam-db-loc (concat (file-name-as-directory selected-dir) ".roam.db")))
    (setq org-roam-directory selected-dir)
    (setq org-roam-db-location roam-db-loc)
    ;;(bibtex-completion-clear-cache)
    (org-roam-db-sync)))


;;;###autoload
(defun ub/org-roam-dailies-capture-today-respect-my-midnight ()
  "Create an org-roam daily note considering the day ends at 7 am."
  (interactive)
  (let ((current-hour (string-to-number (format-time-string "%H"))))
    (if (< current-hour 7)
        ;; if current hour is between 0 and 7, open 'yesterday' note
        (org-roam-dailies-capture-yesterday 1)
      ;; else, open 'today' note
      (org-roam-dailies-capture-today))))
