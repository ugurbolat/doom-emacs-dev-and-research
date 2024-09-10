(message "org-mode-config.el load start")

;; org defaults
(use-package! org
  :config
  (setq
   org-startup-with-inline-images t
   ;;(setq org-startup-with-latex-preview t)=
   org-startup-folded nil
   org-checkbox-hierarchical-statistics nil
   org-tags-column 0
   org-enforce-todo-dependencies nil
   org-log-state-notes-into-drawer nil
   +org-startup-with-animated-gifs 't
   org-startup-folded nil
   org-edit-src-content-indentation 0
   org-startup-indented t
   org-src-tab-acts-natively t
   org-src-fontify-natively t
   org-fontify-quote-and-verse-blocks t
   org-confirm-babel-evaluate nil
   org-support-shift-select nil
   shift-select-mode nil
   org-list-demote-modify-bullet
   '(("+" . "*") ("*" . "-") ("-" . "+"))
   org-hide-emphasis-markers t
   org-image-actual-width '(800)
   org-id-method 'ts
   org-use-speed-commands t
   org-log-into-drawer t
   org-log-reschedule t
   org-log-redeadline t
   org-todo-repeat-to-state "TODO" ; for habits.
   )

  (setq org-bookmark-names-plist
        '(
          :last-refile "org-refile-last-stored"
          ))
  (setq org-capture-bookmark nil)

  (setq org-todo-keywords
        '((sequence "TODO(t!)" "STARTED(s!)" "BLOCKED(b!)" "DELEG(e!)" "|" "DONE(d!)" "KILL(k!)")
          (sequence "PAPER(a!)" "STUDY(s!)" "HYPOTHESIS(h!)" "CODE(c!)" "WRITE(w!)" "REVIEW(r!)" "|" "DONE(d!)" "KILL(k!)")
          (sequence "PROJ(p!)" "J@IN(i!)" "J@HOLD(h!)" "|" "J@DONE(d!)" "J@KILL(k!)")
          (sequence "APPOINT(A!)" "IDEA(i!)" "GOAL(g!)" "DONT(x!)" "SOMEDAY(o!)" "MAYBE(m!)" "|" "DONE(d!)" "KILL(k!)")
          ))
  (setq org-priority-faces '((?A . (:foreground "red" :weight 'bold))
                             (?B . (:foreground "yellow"))
                             (?C . (:foreground "green"))))

  (defun ub/org-jump-to-heading-beginning ()
    "Jump to the beginning of the line of the closest Org heading."
    (interactive)
    (org-back-to-heading)
    (beginning-of-line))
  (define-key org-mode-map (kbd "C-c *") 'ub/org-jump-to-heading-beginning)

  (defun ub/set-created-property ()
    (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %H:%M]" (current-time))))
  (add-hook! 'org-insert-heading-hook #'ub/set-created-property)

  :bind
  ("C-c l" . org-store-link)
  ("C-c C-l" . org-insert-link)
  
  )


;;set /home/bolatu/emacs-configs/doom-done-right/.local/straight/repos/doom-snippets/org-mode
(setq +snippets-dir "/home/bolatu/emacs-configs/doom-done-right/.local/straight/repos/doom-snippets/")

(use-package! org-ai
  :defer t
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  ;;(setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
  (setq org-ai-default-chat-model "gpt-4-turbo")
  (setq org-ai-image-model "dall-e-3")
  (setq org-ai-image-default-size "1024x1024") ; vs. 1024x1792 or 1792x1024
  (setq org-ai-image-default-count 1)
  (setq org-ai-image-default-style 'natural) ; vs. 'vivid
  (setq org-ai-image-default-quality 'standard) ; vs. 'hd
  ;;(setq org-ai-image-directory (+org/org-ai-image-gen-download-method))
  (setq org-ai-image-directory (expand-file-name "tmp/img-gen/" ub/org-root-dir))
  (org-ai-install-yasnippets) ; if you are using yasnippet and want `ai` snippets

  (defcustom org-ai-explain-math-prompt
    (concat "You are an expert in math."
            "The following shows a math description in latex."
            "Explain each element in the equation step-by-step."
            "List the necessary background knowledge needed to understand the given math expression such as theorems."
            "when prompting mathematical equations, you will use latex where the inline math mode equation has prefix "
	    "and suffix as such $...$ and display math mode equations such as"
            "\\begin{equation}"
            "..."
            "\\end{equation}"
            "When providing answers, avoid warnings/disclaimers/extra recommendations!!!"
            )
    "The template to use for `org-ai-explain-math'."
    :type 'string
    :group 'org-ai)

  (defun org-ai-explain-math (start end)
    "Ask ChatGPT explain a code snippet.
`START' is the buffer position of the start of the code snippet.
`END' is the buffer position of the end of the code snippet."
    (interactive "r")
    (org-ai-on-region start end org-ai-explain-math-prompt))

  )


(use-package! org-sidebar
  :defer t
  )


(use-package! org-download
  :config
  ;; TODO in recent org updates (maybe in 9.7), something changed related drag-and-drop for files
  ;; current "work around" is just to link files but with their original path instead of copying.
  (setq org-yank-dnd-method 'file-link)

  ;; take an image that is already on the clipboard
  (setq org-download-screenshot-method "export filename=\"%s\"; import png:\"$filename\" ;xclip -selection clipboard -target image/png -filter < \"$filename\" &>/dev/null")

  (setq org-download-timestamp "%y-%m-%d_%H-%M-%S_")
  ;; org-download-method code snippet taken from
  ;; https://github.com/jethrokuan/dots/blob/master/.doom.d/config.el
  (defun +org/org-download-method (link)
    (let* ((filename
            (file-name-nondirectory
             (car (url-path-and-query
                   (url-generic-parse-url link)))))
           ;; Create folder name with current buffer name, and place in root dir
           (dirname (concat
                     (replace-regexp-in-string " " "_" (downcase (file-name-base buffer-file-name)))
                     ".assets/"))
           (filename-with-timestamp (format "%s%s.%s"
                                            (format-time-string org-download-timestamp)
                                            (file-name-sans-extension filename)
                                            (file-name-extension filename))))
      (make-directory dirname t)
      (expand-file-name filename-with-timestamp dirname)))
  (setq org-download-method '+org/org-download-method)
  ;;(setq org-image-actual-width nil) ;; think necessary for 500 to take effect
  ;;(setq org-download-image-org-width 600)
  (setq org-download-annotate-function 'ignore)
  (setq org-download-annotate-function (lambda (_link) ""))

  ;; doesn't work REF: https://github.com/abo-abo/org-download/issues/215
  ;; (add-hook 'org-mode-hook
  ;;           (lambda ()
  ;;             (kill-local-variable 'dnd-protocol-alist)))
  )


;; babel python
(use-package! org
  :config
  ;; doom already loads shell
  ;;(add-to-list 'org-babel-load-languages '(shell . t))
  ;; python
  (setq org-babel-default-header-args:python
        '((:results . "output")
          (:session . "python-default")
          (:python . "python3")
          (:exports . "both")
          (:cache .   "no")
          (:noweb . "no")
          (:hlines . "no")
          (:tangle . "no")
          (:async . "yes")
          (:eval . "never-export")))
  (add-to-list 'org-babel-load-languages '(python . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  )


;; FIXME
;; REF https://github.com/awth13/org-appear/issues/50
;; (use-package! org-appear
;;   ;;:ensure (:fetcher github :repo "awth13/org-appear")
;;   :defer 0.1
;;   :hook (org-mode . org-appear-mode)
;;   :config
;;   (setq org-appear-autolinks t)
;;   (setq org-appear-autosubmarkers t)
;;   (setq org-appear-autoentities t)
;;   (setq org-appear-autokeywords t)
;;   (setq org-appear-autosubmarkers t))


(use-package! org
  :commands (doct)
  :config
  ;; utilities
  (defun ub/capture-id-create (&optional arg)
    (save-excursion)
    (org-id-get-create))

  ;; templates
  (setq doct-cap-temp-list `())
  (setq cap-task-work `("TODO - work" :keys "w" :type entry :prepend t
                        :unnarrowed nil
                        :empty-lines-before 1
                        :file ,ub/gtd-work-file
                        :headline "Inbox"
                        :before-finalize ub/capture-id-create
                        ;;:prepare-finalize ub/capture-newlines-at-end
                        :template ("* TODO %i%?  "
                                   ":PROPERTIES:"
                                   ":CREATED: %U"
                                   ":REF-LINK: %a"
                                   ":END:")))
  (add-to-list 'doct-cap-temp-list cap-task-work t)
  (setq cap-task-me `("TODO - me" :keys "m" :type entry :prepend t
                      :unnarrowed nil
                      :empty-lines-before 1
                      :file ,ub/gtd-me-file
                      :headline "Inbox"
                      :before-finalize ub/capture-id-create
                      :template ("* TODO %i%?  "
                                 ":PROPERTIES:"
                                 ":CREATED: %U"
                                 ":REF-LINK: %a"
                                 ":END:")))
  (add-to-list 'doct-cap-temp-list cap-task-me t)
  (setq cap-task-me `("Inbox" :keys "i" :type entry :prepend t
                      :unnarrowed nil
                      :empty-lines-before 1
                      :file ,ub/gtd-inbox-laptop-file
                      ;;:headline "Inbox"
                      ;;:before-finalize ub/capture-id-create
                      :template ("* %i%?  "
                                 ":PROPERTIES:"
                                 ":CREATED: %U"
                                 ":REF-LINK: %a"
                                 ":END:")))
  (add-to-list 'doct-cap-temp-list cap-task-me t)
  (setq cap-note `("NOTE - daily" :keys "n" :type entry
                   :unnarrowed nil
                   :empty-lines-before 1
                   :file ,ub/daily-today-file
                   :template ("* %<%H:%M> %a%? "
                              "%i"
                              )))
  (add-to-list 'doct-cap-temp-list cap-note t)
  ;; (setq cap-bib `("Bib" :keys "BB" :type entry :prepend t
  ;;                 :unnarrowed nil
  ;;                 :empty-lines-before 1
  ;;                 :file ,ub/gtd-paper-file
  ;;                 :template-file ,(expand-file-name "temp.org" ub/bib-rebiber-dir)
  ;;                 ))
  ;; (add-to-list 'doct-cap-temp-list cap-bib t)
  (setq cap-bucket `("Web/Bucket" :keys "W"
                     :file ,ub/gtd-bookmark-file
                     :immediate-finish t
                     :prepend t
                     :type entry
                     :template ("* %a %?"
                                ":PROPERTIES:"
                                ":CREATED: %U"
                                ":REF-LINK: %a"
                                ":END:"
                                "%i")
                     :children (
                                ("Movie" :keys "m"
                                 :headline "Movies")
                                ("Series" :keys "s"
                                 :headline "Series")
                                ("Podcast/Talk" :keys "t"
                                 :headline "Podcasts/Talks")
                                ("Book" :keys "b"
                                 :headline "Books")
                                ("Track" :keys "T"
                                 :headline "Tracks")
                                ("Set" :keys "S"
                                 :headline "Sets")
                                ("Label" :keys "L"
                                 :headline "Labels"))))
  (add-to-list 'doct-cap-temp-list cap-bucket t)
  (setq org-capture-templates (doct doct-cap-temp-list))
  )





;; modify the #+LAST_MODIFIED: field each time you save an org file
(defun update-org-last-modified-date ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp "^#\\+LAST_MODIFIED:" nil t)
        (delete-line)
        (insert (format-time-string "#+LAST_MODIFIED: %Y-%m-%d %H:%M\n"))))))

(add-hook 'before-save-hook 'update-org-last-modified-date)


;; org-noter/org-noter
(use-package! org-noter
  ;;:ensure (:fetcher github :repo "org-noter/org-noter")
  :defer t
  :custom
  (org-noter-always-create-frame nil)
  (org-noter-notes-window-behavior '(start))
  (org-noter-notes-window-location 'other-frame)
  (org-noter-notes-search-path `(,ub/roam-bib-dir))
  (org-noter-auto-save-last-location t)
  (org-noter-default-notes-file-names '("000_org_noter_dump.org"))
  )



;; org-latex-preview
(use-package! org
  :custom
  (org-preview-latex-default-process 'dvisvgm)
  :config
  (plist-put org-format-latex-options :scale 1.7) ;; default is quite small
  (plist-put org-format-latex-options :foreground nil)
  (plist-put org-format-latex-options :background nil)
  ;; usepackage latex
  (add-to-list 'org-latex-packages-alist '("" "braket" t))
  (add-to-list 'org-latex-packages-alist '("" "bm" t))
  ;; for tables
  ;; REF https://pandas.pydata.org/docs/reference/api/pandas.io.formats.style.Styler.to_latex.html
  (add-to-list 'org-latex-packages-alist '("" "multirow" t))
  (add-to-list 'org-latex-packages-alist '("" "booktabs" t))
  (add-to-list 'org-latex-packages-alist '("table" "xcolor" t))
  (add-to-list 'org-latex-packages-alist '("" "siunitx" t))
  (add-to-list 'org-latex-packages-alist '("" "etoolbox" t))
  (add-to-list 'org-latex-packages-alist '("" "longtable" t))
  (add-to-list 'org-latex-packages-alist '("" "hyperref" t))

  ;; ;; src blocks
  ;; (setq org-latex-src-block-backend 'minted)
  ;; (add-to-list 'org-latex-packages-alist '("" "algorithm" t))
  ;; (add-to-list 'org-latex-packages-alist '("" "algorithmic" t))
  ;; (add-to-list 'org-latex-packages-alist '("outputdir=/tmp" "minted" t)) ;; src blocks

  (defun my/resize-org-latex-overlays ()
    (cl-loop for o in (car (overlay-lists))
             if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
             do (plist-put (cdr (overlay-get o 'display))
                           :scale (expt text-scale-mode-step
                                        text-scale-mode-amount))))

  :hook
  (org-mode . (lambda () (add-hook 'text-scale-mode-hook #'my/resize-org-latex-overlays nil t)))
  )


;; io12/org-fragtog
(use-package! org-fragtog
  ;;:ensure (:fetcher github :repo "io12/org-fragtog")
  ;;:defer 0.1
  :defer t
  :hook (org-mode . org-fragtog-mode))


;;org-export latex
(use-package! org
  :config
  (setq org-latex-logfiles-extensions '("blg" "fdb_latexmk" "fls" "figlist" "idx" "log" "nav" "out" "ptc" "run.xml" "snm" "toc" "vrb" "xdv")) ;; keep aux and bcf to generate .bib
  (setq org-latex-pdf-process (list "latexmk -shell-escape -f -pdf %f"
                                    ;; below migh be slowing down the export...
                                    ;;"biber --output_align --output_indent=2 --output_fieldcase=lower --output_format=bibtex --output_resolve %b.bcf"
                                    ;;"sed -i -e '/^\s*file\s*=/d' -e '/^\s*abstract\s*=/d' %b_biber.bib" ;; remove 'file' and 'abstract' field
                                    "rm %b.bcf"
                                    "rm %b.aux"
                                    ;; TODO replace \addbibresource{...} w/ local %b_biber.bib in generated .tex
                                    ))

  ;; ;; below migh be slowing down the export...
  ;; minted and algorithm packages don't work out-of-box
  ;; (setq org-latex-src-block-backend 'minted)
  ;; (add-to-list 'org-latex-packages-alist '("" "newfloat" t)) ;; beamer
  ;; (add-to-list 'org-latex-packages-alist '("outputdir=/tmp" "minted" t)) ;; src blocks
  ;; (add-to-list 'org-latex-packages-alist '("" "braket" t))
  ;; (add-to-list 'org-latex-packages-alist '("" "algorithm" t))
  ;; (add-to-list 'org-latex-packages-alist '("" "algorithmic" t))
  ;; (add-to-list 'org-latex-packages-alist '("" "algpseudocode" t))
  ;; (add-to-list 'org-latex-packages-alist '("" "cancel" t))
  ;; (add-to-list 'org-latex-packages-alist '("" "bm" t))
  ;; (add-to-list 'org-latex-packages-alist '("" "tikz" t))

  (eval-after-load "preview"
    '(add-to-list 'preview-default-preamble
      "\\PreviewEnvironment{tikzpicture}" t))
  (setq org-export-use-babel nil)
  )


(use-package! org-pandoc-import
  :defer t
  :after org)

(use-package! org-media-note
  :defer t
  ;;:init (setq org-media-note-use-org-ref t)
  :hook (org-mode .  org-media-note-mode)
  :bind (
         ("C-c u m" . org-media-note-hydra/body))  ;; Main entrance
  :config
  (setq org-media-note-screenshot-image-dir ub/org-media-tmp-image-dir)  ;; Folder to save screenshot
  (setq org-media-note-use-refcite-first t)  ;; use videocite link instead of video link if possible
  (setq org-media-note-screenshot-with-sub nil))
;;(setq org-media-note-ref-key-field "BIB-KEY") ;; CUSTOM_ID property works with org-ref for a reason.



;; org tags
(use-package! org
  :config
  (setq org-complete-tags-always-offer-all-agenda-tags t)
  (setq org-tags-exclude-from-inheritance
        (quote
         ("pj_title" "horizon_title"
          "frog"
          ;;"me_project" "work_project"
          "goal" "habit" "recurring" "focus" "reminder" "pinned"
          "important" "urgent"
          "r@batch" "r@daily" "r@weekly" "r@biweekly" "r@monthly" "r@quarterly" "r@emesterly" "r@yearly" "r@custom"
          "h@batch" "h@daily" "h@weekly" "h@biweekly" "h@monthly" "h@quarterly" "h@semesterly" "h@yearly" "h@custom"
          )))
  (setq ub/tags-main-hide ".*_file\\|.*_agenda\\|pj_*\\|project\\|recurring\\|habit\\|gtd")
  (setq org-agenda-hide-tags-regexp ub/tags-main-hide)

  (add-to-list 'org-tag-faces '("important" . (:foreground "yellow")))
  (add-to-list 'org-tag-faces '("urgent" . (:foreground "orange")))
  (add-to-list 'org-tag-faces '("dead@hard" . (:foreground "red")))


  ;; generate all tags for orgzly
  (defun get-all-tags (file)
    "Get a list of all tags in FILE."
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode) ;; FIXME might be slowing down
      (let (tags)
        (org-map-entries
         (lambda ()
           (let ((current-tags (org-get-tags nil t)))
             (if current-tags
                 (setq tags (append tags current-tags))
               (setq tags (append tags '()))))))
        tags)))

  (defun get-tags-from-directory (directory delete-duplicates)
    "Get a list of all tags in all Org mode files in DIRECTORY and its subdirectories."
    (let (tags)
      (mapc
       (lambda (file)
         (when (and (string-equal "org" (file-name-extension file))
                    (not (string-match-p "zz_tags" file)))
           (setq tags (union tags (get-all-tags file)))))
       (directory-files-recursively directory "\\.org$" 'file-regular-p))
      (if delete-duplicates
          (delete-dups tags)
        tags)))

  (defun write-all-tags (tags fname)
    "Prepend `TEXT' to file `FNAME'"
    (with-temp-buffer
      ;; first: insert `text' into temp buffer
      (org-mode) ;; FIXME might be slowing down
      (org-insert-heading)
      (insert "all tags for orgzly")
      (org-set-tags tags)
      ;; finally: write whole buffer to `fname'
      (write-region (point-min) (point-max) fname)))

  (defun ub/write-all-tags ()
    (interactive)
    (write-all-tags (get-tags-from-directory "~/main/org" t) "~/main/org/gtd/zz_tags_head.org"))

  )


;; org-refile
(use-package! org
  :config
  (setq ub/roam-index-files-list (ub/directory-files-recursively ub/roam-index-dir "\.org$" 0 ".sync-conflict"))
  (setq org-reverse-note-order t)
  ;; TODO define maxlevels for each refile file instead of all
  (setq org-refile-targets `(
                             ;;(,ub/refile-target-all-files-list :maxlevel . 2)

                             (,ub/gtd-me-file :maxlevel . 2)
                             (,ub/gtd-work-file :maxlevel . 2)

                             ;; (,ub/gtd-me-projects-main-files-list :maxlevel . 2)
                             ;; (,ub/gtd-work-projects-main-files-list :maxlevel . 2)

                             ;;(,ub/doom-emacs-files-list :maxlevel . 1)

                             (,ub/elfeed-file :maxlevel . 2)
                             ;;(,ub/doom-elisp-snippets-file :maxlevel . 1)
                             (,ub/gtd-bookmark-file :maxlevel . 1)
                             (,ub/roam-index-files-list :maxlevel . 1)
                             ;;(,ub/gtd-horizon-file :maxlevel . 1)

                             ;;(,ub/journal-main-files-list :maxlevel . 1)
                             ))

  ;; useful for refile by structure of the files
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes 'confirm) ; allow to create new headline
  )


;;org-clock
(use-package! org-mru-clock
  :defer t
  :bind* (("C-c C-x i" . org-mru-clock-in)
          ("C-c C-x C-j" . org-mru-clock-select-recent-task))
  :config
  (setq org-mru-clock-how-many 100
        org-mru-clock-completing-read #'ivy-completing-read)
  (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook))
(use-package! org
  :config
  (setq org-clock-mode-line-total 'today)
  (setq org-duration-format (quote h:mm)))


(use-package! org-anki
  :defer t
  :config
  (setq org-anki-default-deck "org-notes")
  )


;; dummy org-roam should be replaced by org-roam when server is up
(use-package! org-roam
  ;;:ensure (:fetcher github :repo "org-roam/org-roam")
  ;;:defer 0.1
  ;;:defer t  ; doom already defers it
  :custom
  (org-roam-directory (expand-file-name "dummy" ub/roam-root-dir)))

(message "org-mode-config.el load end")
