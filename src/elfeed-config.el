;;; src/elfeed.el -*- lexical-binding: t; -*-

;; REF app/rss/config.el taken from doom but we would like to defer it...

(message "elfeed-config.el load start")


;; This is an opinionated workflow that turns Emacs into an RSS reader, inspired
;; by apps Reeder and Readkit. It can be invoked via `=rss'. Otherwise, if you
;; don't care for the UI you can invoke elfeed directly with `elfeed'.

(defvar +rss-split-direction 'below
  "What direction to pop up the entry buffer in elfeed.")

(defvar +rss-enable-sliced-images t
  "Automatically slice images shown in elfeed-show-mode buffers, making them
easier to scroll through.")

(defvar +rss-workspace-name "*rss*"
  "Name of the workspace that contains the elfeed buffer.")


;;
;;; Packages

(use-package! elfeed
  ;;:defer t
  :commands elfeed
  :init
  (setq elfeed-db-directory (concat doom-local-dir "elfeed/db/")
        elfeed-enclosure-default-dir (concat doom-local-dir "elfeed/enclosures/"))
  :config
  (setq
   ;;elfeed-search-filter "@2-week-ago "
   elfeed-search-filter "@3-days-ago +unread +research"
   elfeed-show-entry-switch #'pop-to-buffer
   elfeed-show-entry-delete #'+rss/delete-pane
   shr-max-image-proportion 0.8)

  (set-popup-rule! "^\\*elfeed-entry"
    :size 0.75 :actions '(display-buffer-below-selected)
    :select t :quit nil :ttl t)

  (make-directory elfeed-db-directory t)

  ;; Ensure elfeed buffers are treated as real
  (add-hook! 'doom-real-buffer-functions
    (defun +rss-buffer-p (buf)
      (string-match-p "^\\*elfeed" (buffer-name buf))))

  ;; Enhance readability of a post
  (add-hook 'elfeed-show-mode-hook #'+rss-elfeed-wrap-h)
  (add-hook! 'elfeed-search-mode-hook
    (add-hook 'kill-buffer-hook #'+rss-cleanup-h nil 'local))

  ;; Large images are annoying to scroll through, because scrolling follows the
  ;; cursor, so we force shr to insert images in slices.
  (when +rss-enable-sliced-images
    (setq-hook! 'elfeed-show-mode-hook
      shr-put-image-function #'+rss-put-sliced-image-fn
      shr-external-rendering-functions '((img . +rss-render-image-tag-without-underline-fn))))

  ;; Keybindings
  (after! elfeed-show
    (define-key! elfeed-show-mode-map
      [remap next-buffer]     #'+rss/next
      [remap previous-buffer] #'+rss/previous))


  )



(use-package! elfeed-org
  ;;:defer t
  ;;:when (modulep! +org)
  :after elfeed
  :preface
  ;;(setq rmh-elfeed-org-files (list "elfeed.org"))
  (setq rmh-elfeed-org-files (list ub/elfeed-file))
  :config
  (elfeed-org)
  (defadvice! +rss-skip-missing-org-files-a (&rest _)
    :before '(elfeed rmh-elfeed-org-mark-feed-ignore elfeed-org-export-opml)
    (unless (file-name-absolute-p (car rmh-elfeed-org-files))
      (let* ((default-directory org-directory)
             (files (mapcar #'expand-file-name rmh-elfeed-org-files)))
        (dolist (file (cl-remove-if #'file-exists-p files))
          (message "elfeed-org: ignoring %S because it can't be read" file))
        (setq rmh-elfeed-org-files (cl-remove-if-not #'file-exists-p files))))))


;; tries to make things pretty but we should be careful with this due to emacs hanging while updating feeds
;; (use-package! elfeed-goodies
;;   ;;:defer t
;;   :after elfeed
;;   :config
;;   (elfeed-goodies/setup))


;; (after! elfeed
;;   (setq elfeed-search-filter "@3-days-ago +unread +research")
;;   (setq rmh-elfeed-org-files
;;         (list
;;          ub/elfeed-file
;;          ))
;;   )

(use-package! elfeed-score
  :defer t
  :after elfeed
  :hook (elfeed-search-mode . elfeed-score-enable)
  :config
  ;; NOTE doesn't work here
  ;;(elfeed-score-enable)
  (define-key elfeed-search-mode-map "=" elfeed-score-map)
  (setq elfeed-score-serde-score-file "~/main/org/feed/elfeed.score")


  ;; modify the elfeed-search-print-entry-function to include the authors
  (defun concatenate-authors (authors-list)
    "Given AUTHORS-LIST, list of plists; return string of all authors concatenated."
    (if (> (length authors-list) 1)
        (format "%s et al." (plist-get (nth 0 authors-list) :name))
      (plist-get (nth 0 authors-list) :name)))

  ;; TODO elfeed on start doesn't load this print function but the default one
  ;; TODO face color can be improved
  (defun my-search-print-fn (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (title (or (elfeed-meta entry :title)
                      (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (entry-authors (concatenate-authors
                           (elfeed-meta entry :authors)))
           (title-width (- (window-width) 10
                           elfeed-search-trailing-width))
           (title-column (elfeed-format-column
                          title 120
                          :left))
           (entry-score (elfeed-format-column (number-to-string (elfeed-score-scoring-get-score-from-entry entry)) 10 :left))
           (authors-column (elfeed-format-column entry-authors 40 :left)))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")

      (insert (propertize title-column
                          'face title-faces 'kbd-help title) " ")
      (insert (propertize authors-column
                          'kbd-help entry-authors) " ")
      (insert entry-score " ")))

  (setq elfeed-search-print-entry-function #'my-search-print-fn)
  (setq elfeed-search-date-format '("%y-%m-%d" 10 :left))
  (setq elfeed-search-title-max-width 130)

  )


(use-package! elfeed-tube
  ;;:ensure t ;; or :straight t
  :defer t
  :after elfeed
  ;;:demand t
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)

  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

(use-package! mpv
  :config
  (setq mpv-executable "~/.local/share/flatpak/exports/bin/io.mpv.Mpv"))

;; NOTE
;; emacs linux process seperates command and arguments
;; flatpak run io.mpv.Mpv doesn't work due to flatpak is the main command and run is the argument
;; so it fails to run the command
;;(setq mpv-executable "flatpak run io.mpv.Mpv")

;; (setq mpv-executable "~/.local/share/flatpak/exports/bin/io.mpv.Mpv")
;; (setq elfeed-tube-mpv--available-p t)

(use-package! elfeed-tube-mpv
  ;;:ensure t ;; or :straight t
  :defer t
  ;;:demand t
  :after elfeed
  :config
  ;; HACK mpv executable somewhere else rather than /usr/bin/mpv so we override it
  (setq elfeed-tube-mpv--available-p t)
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-w" . elfeed-tube-mpv-where)))



;; (use-package elfeed-dashboard
;;   ;;:ensure t
;;   ;;:defer t
;;   :after elfeed
;;   :config
;;   (setq elfeed-dashboard-file "~/main/org/feed/elfeed-dashboard.org")
;;   ;; update feed counts on elfeed-quit
;;   (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links))

(use-package elfeed-summary
  :defer t
  :after elfeed
  :config
  (setq elfeed-summary-settings
        '(
          ;; (group (:title . "GitHub")
          ;;        (:elements
          ;;         (query . (url . "SqrtMinusOne.private.atom"))
          ;;         (group . ((:title . "Guix packages")
          ;;                   (:elements
          ;;                    (query . (and github guix_packages)))
          ;;                   (:hide t)))))
          (group (:title . "Arxiv")
                 (:elements
                  (query . arxiv)))
          (group (:title . "Research")
                 (:elements
                  (query . (and research (not arxiv))))
                 (:hide t))

          ;;;;;;; group by topic

          ;; quantum computing group which is tagged with qc and hide it by default
          (group (:title . "Quantum Computing")
                 (:elements
                  (query . qc))
                 (:hide t))

          ;; reinforcement learning group which is tagged with rl and hide it by default
          (group (:title . "Reinforcement Learning")
                 (:elements
                  (query . rl))
                 (:hide t))

          ;; machine learning group which is tagged with ml and hide it by default
          (group (:title . "Machine Learning")
                 (:elements
                  (query . ml))
                 (:hide t))


          ;; computer science group which is tagged with cs and hide it by default
          (group (:title . "Computer Science")
                 (:elements
                  (query . cs))
                 (:hide t))

          ;; math group which is tagged with math and hide it by default
          (group (:title . "Math")
                 (:elements
                  (query . math))
                 (:hide t))

          ;; emacs group which is tagged with emacs and hide it by default
          (group (:title . "Emacs")
                 (:elements
                  (query . emacs))
                 (:hide t))

          ;; comedy group which is tagged with dev and hide it by default
          (group (:title . "Comedy")
                 (:elements
                  (query . comedy))
                 (:hide t))

          ;;finance and economics group which is tagged with finance and economics and hide it by default
          (group (:title . "Finance & Economics")
                 (:elements
                  (query . (or finance economics)))
                 (:hide t))

          ;; deutsch group which is tagged with de and hide it by default
          (group (:title . "Deutsch")
                 (:elements
                  (query . de))
                 (:hide t))

          ;; turkish group which is tagged with tr and hide it by default
          (group (:title . "Turkish")
                 (:elements
                  (query . tr))
                 (:hide t))

          ;; fitness and health group which is tagged with fitness and health and hide it by default
          (group (:title . "Fitness & Health")
                 (:elements
                  (query . (or fitness health)))
                 (:hide t))

          ;; startup group which is tagged with startup and hide it by default
          (group (:title . "Startup")
                 (:elements
                  (query . startup))
                 (:hide t))


          ;;;;;;; group by content media type

          ;; youtube group which is tagged with yt and hide it by default
          (group (:title . "YouTube")
                 (:elements
                  (query . yt))
                 (:hide t))

          ;; podcast group which is tagged with podcast and hide it by default
          (group (:title . "Podcasts")
                 (:elements
                  (query . podcast))
                 (:hide t))

          ;; blog group which is tagged with blog and hide it by default
          (group (:title . "Blogs")
                 (:elements
                  (query . blog))
                 (:hide t))

          ;; news group which is tagged with news and hide it by default
          (group (:title . "News")
                 (:elements
                  (query . news))
                 (:hide t))


          ;; job group which is tagged with job and hide it by default
          (group (:title . "Jobs")
                 (:elements
                  (query . job))
                 (:hide t))

          ;; dev group which is tagged with dev and hide it by default
          (group (:title . "Dev")
                 (:elements
                  (query . dev))
                 (:hide t))

          ;; lecture youtube (playlist) group which is tagged with lecture and hide it by default
          (group (:title . "Lectures")
                 (:elements
                  (query . lecture))
                 (:hide t))

          ;; (group (:title . "Blogs [People]")
          ;;        (:elements
          ;;         (query . (and blogs people (not emacs)))
          ;;         (group (:title . "Emacs")
          ;;                (:elements
          ;;                 (query . (and blogs people emacs))))))
          ;; (group (:title . "Podcasts")
          ;;        (:elements
          ;;         (query . podcasts)))
          ;; (group (:title . "Videos")
          ;;        (:elements
          ;;         (group
          ;;          (:title . "Music")
          ;;          (:elements
          ;;           (query . (and videos music))))
          ;;         (group
          ;;          (:title . "Tech")
          ;;          (:elements
          ;;           (query . (and videos tech))))
          ;;         (group
          ;;          (:title . "History")
          ;;          (:elements
          ;;           (query . (and videos history))))
          ;;         ;; ...
          ;;         ))
          ;; ;; ...
          ;; (group (:title . "Miscellaneous")
          ;;        (:elements
          ;;         (group
          ;;          (:title . "Searches")
          ;;          (:elements
          ;;           (search
          ;;            (:filter . "@6-months-ago sqrtminusone")
          ;;            (:title . "About me"))
          ;;           (search
          ;;            (:filter . "+later")
          ;;            (:title . "Check later"))))
          ;;         (group
          ;;          (:title . "Ungrouped")
          ;;          (:elements :misc))))
          ))
  )


(message "elfeed-config.el load end")
