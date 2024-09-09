;;; src/main.el -*- lexical-binding: t; -*-
(message "main.el load start")

(load-file (expand-file-name "src/paths.el" doom-user-dir))
(load-file (expand-file-name "src/autoload.el" doom-user-dir))

;; emacs global defaults
;; to suppress org-element-cache warning
;; TODO add copilot ?
;; err
;; ⛔ Warning (copilot): copilot--infer-indentation-offset found no mode-specific indentation offset.
(setq warning-suppress-types '((org-element-cache defvaralias))
      warning-minimum-level :error)

(use-package! emacs
  ;;:ensure nil ;; since it's a built in feature
  :custom
  ;; already doom's defaults
  ;;(inhibit-startup-screen t "Open scratch buffer at startup")
  ;;(indent-tabs-mode nil "Stop using tabs to indent")
  ;;(make-backup-files nil "Stop creating ~ backup files")
  ;;(enable-local-variables 't "local variables are customizations in either file or directory")
  ;;(global-hl-line-mode t "Horizontal current line highlight")
  ;;(create-lockfiles nil "Stop creating .# files")
  ;;(delete-selection-mode t "Currently selected text gets deleted when typing new text")
  ;;(revert-without-query '(".") "Revert buffer without asking")

  ;; override doom's defaults
  (text-scale-mode-step 1.1 "Scaling text less aggressive")
  (display-line-numbers-type nil "No line numbers at startup")

  :bind
  ("<f5>" . revert-buffer)
  ;;:hook
  ;;((text-mode . visual-line-mode))
  )

(require 'server)
(unless (server-running-p)
  (server-start))


(use-package modus-themes
  :init
  (setq modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia))
  ;; ... setting all variable that need to
  ;; be present before loading the theme ...
  :config
  (load-theme (car modus-themes-to-toggle) t t)
  :bind ("<f5>" . modus-themes-toggle))



(ub/run-if-else-server-active
 #'(lambda () (load-theme 'doom-vibrant t))
 ;;#'(lambda () (load-theme 'modus-vivendi t)))
 #'(lambda () (load-theme 'modus-vivendi-tinted t)))

;;(setq doom-theme 'modus-vivendi)
;;(setq doom-theme 'doom-vibrant)
;;(load-theme 'doom-vibrant t)
;;(load-theme 'modus-vivendi t)
(setq doom-font (font-spec
                 ;;:family "Ubuntu Mono"
                 :size 20 :weight 'normal))

;; window management
;; setting for resizing windows
(use-package! window
  ;;:ensure nil ;; built in feature, don't need to ensure
  :bind
  (("S-C-<left>" . shrink-window-horizontally)
   ("S-C-<right>" . enlarge-window-horizontally)
   ("S-C-<down>" . shrink-window)
   ("S-C-<up>" . enlarge-window)
   ("C-=" . text-scale-increase)
   ("C--" . text-scale-decrease)))

(use-package! rotate
  :defer t
  ;;:ensure (:fetcher github :repo "daichirata/emacs-rotate")
  ;; :bind
  ;; ("C-c r" . rotate-layout)
  )

(use-package! minions
  :defer 0.1
  :after doom-modeline
  :config
  (setq doom-modeline-minor-modes t)
  (minions-mode 1))

(use-package! hl-todo
  ;;:ensure (:fetcher github :repo "tarsius/hl-todo")
  ;;:defer 0.1
  :config
  (setq hl-todo-keyword-faces
        '(("TODO" . "#ff4500")
          ("SOMEDAY" . "#ffff00")
          ("KILL" . "#696969")
          ("DONE" . "#44bc44")
          ("IDEA" . "#ff00ff")
          ("PAPER" . "#ff00ff")
          ("STUDY" . "#ff00ff")
          ("HYPOTHESIS" . "#ff4500")
          ("CODE" . "#ff4500")
          ("WRITE" . "#ffa500")
          ("REVIEW" . "#6ae4b9")

          ("DONT" . "#70b900")
          ("BUG" . "#C70039")
          ("NOTE" . "#d3b55f")
          ("HOLD" . "#c0c530")
          ("HACK" . "#d0bc00")
          ("FAIL" . "#ff8059")
          ("WORKAROUND" . "#ffcccc")
          ("FIXME" . "#ff9077")
          ("DEPRECATED" . "#bfd9ff")
          ("REF" . "#660066")))
  (global-hl-todo-mode 1))


(use-package! tramp
  ;;:ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))



(use-package! recentf
  ;;  :ensure nil ; built-in package
  ;;:defer 0.1
  :custom
  (recentf-auto-cleanup 30)
  (recentf-max-saved-items 200)
  :config
  (recentf-mode)
  (run-with-idle-timer 30 t 'recentf-save-list))


;; smooth scrolling
(use-package! mwheel
  ;;:ensure nil
  :custom
  (mouse-wheel-scroll-amount '(1
                               ((shift) . 5)
                               ((control))))
  (mouse-wheel-progressive-speed nil))
(use-package! pixel-scroll
  ;;:ensure nil
  :config
  (pixel-scroll-mode))


(use-package! dired
  ;;:ensure nil
  :config
  ;;(global-set-key (kbd "C-c o -") 'dired-jump)
  (setq delete-by-moving-to-trash 't)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(use-package! dired-hacks
  ;;:ensure (:fetcher github :repo "Fuco1/dired-hacks")
  :defer t
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("<C-tab>" . dired-subtree-cycle)
        ("<backtab>" . dired-subtree-remove)
        ("e" . ub/ediff-files)
        ("<M-right>" . dired-find-file)
        ("<M-left>" . dired-up-directory))
  :config
  ;; dired extra
  (require 'dired-x)
  ;; TODO write the
  ;; subtree
  (require 'dired-subtree)
  (setq dired-subtree-use-backgrounds nil)

  ;; TODO move this to autoload?
  (defun ub/ediff-files ()
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2 (if (cdr files)
                           (cadr files)
                         (read-file-name
                          "file: "
                          (dired-dwim-target-directory)))))
            (if (file-newer-than-file-p file1 file2)
                (ediff-files file2 file1)
              (ediff-files file1 file2))
            (add-hook 'ediff-after-quit-hook-internal
                      (lambda ()
                        (setq ediff-after-quit-hook-internal nil)
                        (set-window-configuration wnd))))
        (error "no more than 2 files should be marked"))))
  (when (eq system-type 'gnu/linux)
    (setq dired-guess-shell-alist-user '(("\\.mp3\\'"  "mpv")
                                         ("\\.mp4\\'"  "mpv")
                                         ("\\.m4a\\'"  "mpv")
                                         ("\\.webm\\'" "mpv")
                                         ("\\.mkv\\'"  "mpv")
                                         ("\\.avi\\'"  "mpv")
                                         ("\\.pdf\\'" "evince")
                                         ("\\.pd\\'"  "evince")
                                         ("\\.dvi\\'" "evince")
                                         ("\\.epub\\'" "ebook-viewer")
                                         ("\\.doc\\'" "libreoffice")
                                         ("\\.docx\\'" "libreoffice")
                                         ("\\.ppt\\'" "libreoffice")
                                         ("\\.pptx\\'" "libreoffice")
                                         ("\\.xls\\'" "libreoffice")
                                         ("\\.xlsx\\'" "libreoffice")
                                         ("\\.odt\\'" "libreoffice")
                                         ("\\.ods\\'" "libreoffice")
                                         ("\\.odg\\'" "libreoffice")
                                         ("\\.odp\\'" "libreoffice")
                                         ("\\.jpg\\'" "eog")
                                         ("\\.jpeg\\'" "eog")
                                         ("\\.png\\'" "eog")
                                         ("\\.gif\\'" "eog")
                                         ("\\.svg\\'" "eog")
                                         ))))

(use-package! dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  ;;:ensure t
  :defer t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  ;; following command won't effect dired-sidebar
  (push 'rotate-layout dired-sidebar-toggle-hidden-commands)
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  ;;(setq dired-sidebar-subtree-line-prefix "_")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  ;;(setq dired-sidebar-use-custom-font t)
  )

;; (use-package dired-hide-dotfiles
;;   ;;:ensure t
;;   :bind
;;   (:map dired-mode-map
;;         ("." . dired-hide-dotfiles-mode))
;;   :hook
;;   (dired-mode . dired-hide-dotfiles-mode))

(use-package! diredfl
  ;;:ensure t
  :hook
  (dired-mode . diredfl-mode))

(use-package! async
  ;;:ensure t
  :defer t
  :custom
  (dired-async-mode 1))

(use-package! dired-rsync
  ;;:ensure t
  :defer t
  :bind
  (:map dired-mode-map
        ("r" . dired-rsync)))

;; (use-package dired-launch
;;   ;;:ensure t
;;   :defer t
;;   :hook
;;   (dired-mode . dired-launch-mode))

;; (use-package dired-git-info
;;   ;;:ensure t
;;   :defer t
;;   :bind
;;   (:map dired-mode-map
;;         (")" . dired-git-info-mode)))


(use-package! real-auto-save
  ;;:ensure (:fetcher github :repo "ChillarAnand/real-auto-save")
  :defer 0.1
  :config
  (add-hook 'prog-mode-hook 'real-auto-save-mode)
  (add-hook 'org-mode-hook 'real-auto-save-mode)
  (setq real-auto-save-interval 5))



(use-package! vterm
  ;;:ensure (:fetcher github :repo "akermu/emacs-libvterm")
  ;;:defer 0.1
  :config
  (setq vterm-max-scrollback 10000)
  ;; doom already has this in some form
  ;;(add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  )

(use-package! git-timemachine
  ;;:ensure t
  :defer t)


(use-package! emacs
  ;;:ensure nil
  :custom
  ;;(shell-file-name "/usr/bin/zsh")
  (shell-file-name "/usr/bin/bash")
  (shell-command-switch "-c"))



(use-package! aggressive-indent
  ;;:ensure (:fetcher github :repo "Malabarba/aggressive-indent-mode")
  :defer t
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))


(use-package! pdf-tools
  ;;:ensure (:fetcher github :repo "vedang/pdf-tools")
  ;;:defer 0.1
  ;;:config
  ;;(pdf-loader-install)
  ;;:hook
  ;;(pdf-view-mode . pdf-view-midnight-minor-mode)
  )


(use-package! realgud
  ;;:ensure (:fetcher github :repo "realgud/realgud")
  ;;:defer 0.1
  ;; when in python-mode set key C-c d d to realgud:pdb
  :bind
  (:map python-mode-map
        ("C-c d d" . realgud:pdb)
        ;;("C-c d f" . ub/realgud:pdb)
        )
  :custom
  (realgud-safe-mode nil))


(use-package activity-watch-mode
  ;;:ensure (:fetcher github :repo "pauldub/activity-watch-mode")
  :defer 0.1
  :after magit
  :config
  (require 'magit-process)
  (global-activity-watch-mode))



;; TODO better integration with doom theme
;; atm, it doesn't pick up the inactive buffer that has a darker background
;; forgot the name of the package which doom use to differentiate active and inactive buffer
(use-package! spacious-padding
  ;;:ensure (:fetcher github :repo "protesilaos/spacious-padding")
  :defer 0.1
  :custom
  ;; new
  (spacious-padding-widths
   '( :internal-border-width 15
      :header-line-width 4
      :mode-line-width 4
      :tab-width 4
      :right-divider-width 30
      :scroll-bar-width 8
      :fringe-width 8))
  :config
  (spacious-padding-mode 1))


(use-package! csv-mode
  ;;:ensure t
  :defer t
  :mode
  (("\\.[Cc][Ss][Vv]\\'" . csv-mode)))

(use-package! indent-bars
  ;;:ensure (:fetcher github :repo "jdtsmith/indent-bars")
  :config
  (setq indent-bars-width-frac 0.2)
  :after treesit
  :defer 0.1
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
                               list list_comprehension
                               dictionary dictionary_comprehension
                               parenthesized_expression subscript)))
  :hook ((python-base-mode yaml-mode) . indent-bars-mode))

(use-package! doc-toc
  ;;:ensure (:fetcher github :repo "dalanicolai/doc-tools-toc")
  :defer t)


;; ;; Not needed atm since I use local-dir and local env. vars.
;; ;;old
;; ;; "Ever find that a command works in your shell, but not in Emacs?" - Oh yeah!
;; (elpaca
;;  (exec-path-from-shell
;;   :repo "purcell/exec-path-from-shell"
;;   :fetcher github))
;; (elpaca-wait)
;; (when (memq window-system '(mac ns x)) ;; if you're in the GUI
;;   ;; You might have already installed exec-path-from-shell
;;   (require 'exec-path-from-shell)
;;   ;; Append any paths you would like to import here:
;;   (dolist (var '(
;;                  ;;"LD_LIBRARY_PATH" "PYTHONPATH"
;;                  ;;"SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
;;                  ))
;;     (add-to-list 'exec-path-from-shell-variables var))
;;   (exec-path-from-shell-initialize))
;; (when (daemonp) ;; if you're in the emacs-client
;;   (exec-path-from-shell-initialize))

;; ;;new
;; (use-package exec-path-from-shell
;;   ;;:ensure (:fetcher github :repo "purcell/exec-path-from-shell")
;;   ;;:defer 0.1
;;   :config
;;   ;;(when (memq window-system '(mac ns x)))
;;   (when (memq window-system '(mac ns x)) ;; if you're in the GUI
;;     ;; You might have already installed exec-path-from-shell
;;     (require 'exec-path-from-shell)
;;     ;; Append any paths you would like to import here:
;;     (dolist (var '(
;;                    ;;"LD_LIBRARY_PATH" "PYTHONPATH"
;;                    ;;"SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
;;                    ))
;;       (add-to-list 'exec-path-from-shell-variables var))
;;     (exec-path-from-shell-initialize))
;;   (when (daemonp) ;; if you're in the emacs-client
;;     (exec-path-from-shell-initialize))
;;   )

;;(setenv "BASH_ENV" "~/.bash_aliases")
;;(setenv "BASH_ENV" (expand-file-name "~/.bash_aliases"))


;; NOTE format region w/ black/etc. for python doesn't work
(use-package! format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  ;; :config
  ;; (setq-default format-all-formatters
  ;;               '(("C"     (astyle "--mode=c"))
  ;;                 ("Shell" (shfmt "-i" "4" "-ci"))))
  )
(use-package! python-black
  :after python
  )

;; disassembly to bytecode as well as assembly.
(use-package! rmsbolt)

(load-file (expand-file-name "src/credentials.el" doom-user-dir))

(load-file (expand-file-name "src/completions.el" doom-user-dir))

(load-file (expand-file-name "src/python.el" doom-user-dir))

(load-file (expand-file-name "src/org-mode-autoload.el" doom-user-dir))
(load-file (expand-file-name "src/org-mode-common-config.el" doom-user-dir))

(load-file (expand-file-name "src/jupyter.el" doom-user-dir))

(load-file (expand-file-name "src/bib.el" doom-user-dir))
;;(load-file (expand-file-name "src/rebiber.el" doom-user-dir)) ;; not written all well, reqiure fixed files like temp.org to be in certain paths


(ub/run-if-server-active
 #'(lambda ()
     (progn
       (load-file (expand-file-name "src/org-mode-server-config.el" doom-user-dir))

       (load-file (expand-file-name "src/org-roam-autoload.el" doom-user-dir))
       (load-file (expand-file-name "src/org-roam-config.el" doom-user-dir))
       ;; NOTE should be loaded after org-roam-config.el
       ;;(load-file (expand-file-name "src/yequake.el" doom-user-dir))
       (load-file (expand-file-name "src/org-protocol-capture-windows.el" doom-user-dir))
       (load-file (expand-file-name "src/org-agenda-config.el" doom-user-dir))
       )))


(ub/run-if-server-active
 #'(lambda ()
     (progn
       (load-file (expand-file-name "src/elfeed-autoload.el" doom-user-dir))
       (load-file (expand-file-name "src/elfeed-config.el" doom-user-dir))
       )))



(message "main.el load end")
