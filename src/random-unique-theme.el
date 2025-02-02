(defvar ub/favorite-themes '(
                             ef-dream ef-winter
                             ;;ef-rosa ;; this is our default server theme :)
                             ef-autumn ef-elea-dark ef-symbiosis ef-trio-dark ef-maris-dark
                             )
  "List of themes to randomly choose from.")

(defvar ub/theme-history-file "~/.emacs-theme-history"
  "File to store currently active themes across sessions.")

(defun ub/read-active-themes ()
  "Read currently active themes from history file."
  (when (file-exists-p ub/theme-history-file)
    (with-temp-buffer
      (insert-file-contents ub/theme-history-file)
      (read (buffer-string)))))

(defun ub/write-active-theme (theme)
  "Write THEME to history file."
  (let* ((active-themes (ub/read-active-themes))
         (updated-themes (cons theme active-themes)))
    (with-temp-file ub/theme-history-file
      (print updated-themes (current-buffer)))))

(defun ub/remove-active-theme ()
  "Remove current theme from history file when session ends."
  (let* ((active-themes (ub/read-active-themes))
         (current-theme doom-theme)
         (updated-themes (delete current-theme active-themes)))
    (with-temp-file ub/theme-history-file
      (print updated-themes (current-buffer)))))

(defun ub/get-available-themes ()
  "Get list of themes that aren't currently active in other sessions."
  (let ((active-themes (ub/read-active-themes)))
    (cl-remove-if (lambda (theme) 
                    (member theme active-themes))
                  ub/favorite-themes)))

(defun ub/set-random-unique-theme ()
  "Set a random theme that isn't used in other sessions."
  (let* ((available-themes (ub/get-available-themes))
         (theme (if available-themes
                    (nth (random (length available-themes)) available-themes)
                  (car ub/favorite-themes))))
    (setq doom-theme theme)
    (load-theme theme t)
    (ub/write-active-theme theme)))

;; Clean up theme history when Emacs exits
(add-hook 'kill-emacs-hook #'ub/remove-active-theme)

;; Use this instead of your current ub/run-if-else-server-active
(ub/set-random-unique-theme)
