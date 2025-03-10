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


;; (ub/run-if-else-server-active
;;  ;;#'(lambda () (setq doom-theme 'doom-dracula))
;;  #'(lambda () (setq doom-theme 'ef-dream))
;;  #'(lambda () (setq doom-theme 'ef-rosa)))

(ub/run-if-else-server-active
 ;;#'(lambda () (setq doom-theme 'doom-dracula))
 #'(lambda () (setq doom-theme 'ef-rosa))
 #'(lambda () (load-file (expand-file-name "src/random-unique-theme.el" doom-user-dir))))

;; removing unused menu items:
(assoc-delete-all "Open project" +doom-dashboard-menu-sections)
(assoc-delete-all "Open documentation" +doom-dashboard-menu-sections)

;; add new items
;; ;; REF: https://discourse.doomemacs.org/t/how-to-change-your-splash-screen/57
;; ;; e.g.
;; (add-to-list '+doom-dashboard-menu-sections
;;              '("Add journal entry"
;;                :icon (all-the-icons-octicon "calendar" :face 'doom-dashboard-menu-title)
;;                :when (featurep! :lang org +journal)
;;                :face (:inherit (doom-dashboard-menu-title bold))
;;                :action org-journal-new-entry))
(defun open-me-org-file ()
  (interactive)
  (find-file "~/main/org/gtd/me.org"))
(add-to-list '+doom-dashboard-menu-sections
             '("Open work.org" :icon (nerd-icons-mdicon "nf-md-robot_angry" :face 'doom-dashboard-menu-title)
               :action open-work-org-file))
(defun open-work-org-file ()
  (interactive)
  (find-file "~/main/org/gtd/work.org"))
(add-to-list '+doom-dashboard-menu-sections
             '("Open me.org" :icon (nerd-icons-mdicon "nf-md-cat" :face 'doom-dashboard-menu-title)
               :action open-me-org-file))

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

(setq mouse-wheel-tilt-scroll 't)

;; (setq scroll-margin 1
;;       scroll-conservatively 0
;;       scroll-up-aggressively 0.01
;;       scroll-down-aggressively 0.01)
;; Recenter when returning to original position
(setq scroll-restore-recenter t)


(setq grep-highlight-matches nil)
;; get proper Python syntax highlighting in grep results
(add-hook 'grep-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '((".*\\.py:[0-9]+:" . 'font-lock-constant-face)
               ("\\(def\\|class\\) \\([a-zA-Z0-9_]+\\)"
                (1 'font-lock-keyword-face)
                (2 'font-lock-function-name-face))))))

(set-popup-rules!
  '(("^\\*grep"
     :side left
     :width 80
     :select t
     :quit nil)))


;; too shorten filenames in grep
(use-package scf-mode
  :config
  (add-hook 'grep-mode-hook (lambda () (scf-mode 1))))

(require 'scroll-restore)
(scroll-restore-mode 1)

(use-package nerd-icons-dired
  :config
  :hook (dired-mode . (lambda ()
                        (interactive)
                        (unless (file-remote-p default-directory)
                          (nerd-icons-dired-mode)))))


(use-package dired-subtree
  :config
  (advice-add 'dired-subtree-toggle :after (lambda ()
                                             (interactive)
                                             (when nerd-icons-dired-mode
                                               (revert-buffer)))))

(use-package! dired
  ;;:ensure nil
  :config
  ;;(global-set-key (kbd "C-c o -") 'dired-jump)
  (setq delete-by-moving-to-trash 't)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(use-package! dired-du
  :config
  ;;(add-hook 'dired-mode-hook #'dired-du-mode)
  )

(use-package! dired-hacks
  ;;:ensure (:fetcher github :repo "Fuco1/dired-hacks")
  :defer t
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ;;("<C-tab>" . dired-subtree-cycle) ;; will use it for tab switch
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

  (setq dired-sidebar-theme 'nerd-icons)
  ;;(setq dired-sidebar-subtree-line-prefix "_")
  ;;(setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  )

(setq ibuffer-expert t) ; stop yes no prompt on delete

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org" (mode . org-mode))
               ;;("magit" (name . "^magit"))
               ("latex" (or (mode . latex-mode)
                            (filename . "LaTeXMode")))
               ("planner" (or
                           (name . "^\\*Calendar\\*$")
                           (name . "^\\*Org Agenda\\*")))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))))))

(add-hook 'ibuffer-hook
          (lambda ()
            (let ((groups (ibuffer-vc-generate-filter-groups-by-vc-root)))
              (setq ibuffer-filter-groups
                    (append groups (cdr (assoc "default" ibuffer-saved-filter-groups))))
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))


(use-package! ibuffer-sidebar
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq ibuffer-sidebar-use-custom-font t)
  (setq ibuffer-sidebar-face `(:family "Helvetica" :height 140)))

(defun sidebar-toggle-both-ibuffer-and-dired ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (ibuffer-sidebar-toggle-sidebar)
  (dired-sidebar-toggle-sidebar))

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

;; (use-package! async
;;   ;;:ensure t
;;   ;;:defer t
;;   :custom
;;   (dired-async-mode 1))


(use-package! dired-rsync
  :after dired
  :bind
  (:map dired-mode-map
        ("r" . dired-rsync))
  :config
  ;;(setq dired-rsync-command "rsync")
  (setq dired-rsync-options "-azh --progress")
  (setq dired-rsync-unmark-on-completion t)
  ;;(add-to-list 'mode-line-misc-info '(:eval dired-rsync-modeline-status 'append)
  )

(use-package! dired-rsync-transient
  :bind (:map dired-mode-map
              ("C-c C-x" . dired-rsync-transient)))


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

(use-package! gptel
  :config
  (setq gptel-default-mode 'org-mode)

  ;;(setq gptel-model "meta-llama/llama-3.3-70b-instruct")
  ;;(setq gptel-model "anthropic/claude-3.7-sonnet")
  ;;(setq gptel-model "gpt-4o")
  ;;(setq gptel-model "sonar")

  ;;(setq gptel-api-key #'gptel-api-key-from-auth-source)
  (setq gptel-api-key #'ub/load-key-openai-token)

  ;; (setq
  ;;  ;;gptel-model "claude-3-5-sonnet-20240620"
  ;;  gptel-backend (gptel-make-anthropic "Claude"
  ;;                  :stream t :key (ub/load-key-anthropic-token)))

  (gptel-make-anthropic "Claude"
    :stream t :key #'ub/load-key-anthropic-token)

  (gptel-make-perplexity "Perplexity" ;Any name you want
    :key #'ub/load-key-perplexity-token ;can be a function that returns the key
    :stream t) ;If you want responses to be streamed

  ;; OpenRouter offers an OpenAI compatible API
  (setq
   ;;gptel-model 'meta-llama/llama-3.3-70b-instruct
   gptel-model 'anthropic/claude-3.7-sonnet
   gptel-backend
   (gptel-make-openai "OpenRouter"               ;Any name you want
     :host "openrouter.ai"
     :endpoint "/api/v1/chat/completions"
     :stream t
     :key #'ub/load-key-openrouter-token
     :models '(anthropic/claude-3.7-sonnet
               anthropic/claude-3.7-sonnet:beta ;; self-moderated?
               anthropic/claude-3.7-sonnet:thinking
               anthropic/claude-3.5-sonnet
               anthropic/claude-3.5-sonnet:beta ;; self-moderated?
               google/gemini-flash-1.5
               qwen/qwen-2.5-72b-instruct

               deepseek/deepseek-chat
               deepseek/deepseek-r1
               deepseek/deepseek-r1-distill-llama-8b
               deepseek/deepseek-r1-distill-llama-70b

               meta-llama/llama-3.3-70b-instruct
               qwen/qwen-2.5-72b-instruct

               nousresearch/hermes-2-pro-llama-3-8b

               google/gemini-2.0-flash-001

               ;;
               ;; mistralai/mixtral-8x7b-instruct
               ;; meta-llama/codellama-34b-instruct
               ;; codellama/codellama-70b-instruct
               ;; google/palm-2-codechat-bison-32k
               ;; google/gemini-pro
               )))


  (setq gptel-directives
        '((default . "Adapt your response style and depth based on the user's demonstrated knowledge level and the complexity of their queries. Focus on the request task without warnings, disclaimers, etc.")
          (code . "You are an expert programmer and computer scientist. Write only the request code and only code as output without any additional text, prompt or note unless it is specificially asked.")
          (code-emacs . "You are an expert in emacs and emacs-lisp programming language. Always give emacs-lisp which be run programatically instead of manual UI operations. Write only the request code and only code as output without any additional text, prompt or note unless it is specificially asked.")
          (code-explain . "You are an expert programmer and computer scientist. Explain step-by-step from first principles.")
          (code-debug . "You are a debugging expert. Analyze code snippets, identify potential issues, and suggest fixes. If the issue is not obvious, suggest print statements for collecting more info.")
          (code-data-analysis . "You are a data analysis expert. Interpret data, suggest visualization methods, and provide insights.")
          (code-git-commit . "You are expert in git. Given the changes, write a good commit title and message. The first line is treated as the subject of an email and the rest of the text as the body. Note that commit message title is limited to 72 characters so write a fitting short commit title. The trick to structuring an exceptional commit message is to find the proper balance between brevity and detail. Brief enough that it’s easy to read, but detailed enough that it’s easy to understand. In the title, you can use a fitting github-based emojis for example :bug: fix issue x. Write only the request commit title and message without any additional text, prompt or note. In the title, use only lower-case letters.")

          ;; REF: https://github.com/f/awesome-chatgpt-prompts
          (act-as-python-interpreter . "I want you to act like a Python interpreter. I will give you Python code, and you will execute it. Do not provide any explanations. Do not respond with anything except the output of the code. . When I need to tell you something in English, I will do so by putting text inside curly brackets {like this}.")
          (act-as-linux-cli . "I want you to act as a linux terminal. I will type commands and you will reply with what the terminal should show. I want you to only reply with the terminal output inside one unique code block, and nothing else. do not write explanations. do not type commands unless I instruct you to do so. When I need to tell you something in English, I will do so by putting text inside curly brackets {like this}.")

          (write . "You are an expert writing assistant. Respond concisely.")
          (chat . "You are a conversation partner. Respond concisely.")
          (math-explain . (concat "You are an expert in math."
                                  "Be mathematically rigorous and precise!"
                                  "When prompting mathematical expressions, you will use latex where the inline math expressions are enclosed with single dollar signs (i.e., $...$)"
                                  " and displayed math expressions enclosed with double dollar signs (i.e.,"
                                  ;;"\\begin{equation}"
                                  "$$"
                                  "..."
                                  "$$"
                                  ;;"\\end{equation}"
                                  ")"
                                  "Avoid warnings/disclaimers."
                                  ))
          (math-explain . (concat "You are an expert in math."
                                  "Be mathematically rigorous and precise!"
                                  "Explain step-by-step from first principles."
                                  "List the necessary background knowledge needed to understand the given math expression such as definitions, properties, and theorems."
                                  "When prompting mathematical expressions, you will use latex where the inline math expressions are enclosed with single dollar signs (i.e., $...$)"
                                  " and displayed math expressions enclosed with double dollar signs (i.e.,"
                                  ;;"\\begin{equation}"
                                  "$$"
                                  "..."
                                  "$$"
                                  ;;"\\end{equation}"
                                  ")"
                                  "Avoid warnings/disclaimers."
                                  ))
          (research . "You are a research assistant. Summarize key points, cite relevant sources, quote related statements and identify areas for further investigation.")
          (security . "You are a cybersecurity expert. Analyze potential vulnerabilities and suggest best practices for secure coding and system design.")

          ;; REF: https://github.com/f/awesome-chatgpt-prompts?tab=readme-ov-file#structured-iterative-reasoning-protocol-sirp
          (reasoning-sirp . "Begin by enclosing all thoughts within tags, exploring multiple angles and approaches. Break down the solution into clear steps within tags. Start with a 20-step budget, requesting more for complex problems if needed. Use tags after each step to show the remaining budget. Stop when reaching 0. Continuously adjust your reasoning based on intermediate results and reflections, adapting your strategy as you progress. Regularly evaluate progress using tags. Be critical and honest about your reasoning process. Assign a quality score between 0.0 and 1.0 using tags after each reflection. Use this to guide your approach: 0.8+: Continue current approach 0.5-0.7: Consider minor adjustments Below 0.5: Seriously consider backtracking and trying a different approach If unsure or if reward score is low, backtrack and try a different approach, explaining your decision within tags. For mathematical problems, show all work explicitly using LaTeX for formal notation and provide detailed proofs. Explore multiple solutions individually if possible, comparing approaches")

          ))

  )


(use-package! aider
  :config
  ;;(setq aider-args '("--model" "gpt-4o-mini"))
  (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022 --no-auto-commits"))
  )


;; REF: https://www.perplexity.ai/search/emacs-tab-bar-mode-organizatio-Ie0PFY1ZTp2baBnSMYMcEA
;; REF: [[https://www.youtube.com/watch?v=C7ZlNRbWdVI&t=166s][Organize Your Windows with the Tab Bar in Emacs 27 - YouTube]]
(tab-bar-mode 1)
(setq tab-bar-show 1)          ; Only show bar when multiple tabs exist
(setq tab-bar-close-button-show nil)  ; Hide close button
(setq tab-bar-tab-hints t)     ; Show tab numbers
(setq tab-bar-button-margin 1)
;; Removing Tab Gaps
(setq tab-bar-separator (propertize " " 'display '((space :width 0))))
(setq tab-bar-new-button nil) ;; who's gonna click that button anyway :p



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

       ;;(load-file (expand-file-name "src/org-roam-autoload.el" doom-user-dir))
       ;;(load-file (expand-file-name "src/org-roam-config.el" doom-user-dir))
       ;; NOTE should be loaded after org-roam-config.el
       ;;(load-file (expand-file-name "src/yequake.el" doom-user-dir))
       ;;(load-file (expand-file-name "src/org-protocol-capture-windows.el" doom-user-dir))
       (load-file (expand-file-name "src/org-agenda-config.el" doom-user-dir))
       )))


(ub/run-if-server-active
 #'(lambda ()
     (progn
       (load-file (expand-file-name "src/elfeed-autoload.el" doom-user-dir))
       (load-file (expand-file-name "src/elfeed-config.el" doom-user-dir))
       )))




(load-file (expand-file-name "src/remote-runner-args-python.el" doom-user-dir))
(when (featurep 'transient)
  (load-file (expand-file-name "src/remote-runner-args-python-transient.el" doom-user-dir)))
(load-file (expand-file-name "src/remote-runner.el" doom-user-dir))



(use-package! detached
  :init
  (detached-init)
  :custom
  (detached-show-output-on-attach t)
  (detached-terminal-data-command system-type)
  (detached-shell-program "/bin/zsh")
  :config
  (setq detached-notification-function #'detached-extra-alert-notification)

  (defun my/detached-vterm-send-input-fix (&optional detached)
    "Fix `detached-vterm-send-input' to exclude shell prompt."
    (interactive)
    (let* ((line (buffer-substring-no-properties (vterm-beginning-of-line) (vterm-end-of-line)))
           ;; Remove shell prompt (assuming format like "bolatu:~$ command")
           ;;(input (string-trim (replace-regexp-in-string "^[^ ]+[:~]+\\$ " "" line))) ;; for bash
           (input (string-trim (replace-regexp-in-string "$ " "" line))) ;; zsh
           (detached-session-origin 'vterm)
           (detached-session-action detached-vterm-session-action)
           (detached-session-mode (if detached 'detached 'attached))
           (detached-current-session (detached-create-session input))
           (command (detached-session-start-command detached-current-session :type 'string)))
      (vterm-send-C-a)
      (vterm-send-C-k)
      (process-send-string vterm--process command)
      (setq detached-buffer-session detached-current-session)
      (vterm-send-C-e)
      (vterm-send-return)))
  ;; Apply the advice to override `detached-vterm-send-input`
  (advice-add 'detached-vterm-send-input :override #'my/detached-vterm-send-input-fix)
  )


(message "main.el load end")
