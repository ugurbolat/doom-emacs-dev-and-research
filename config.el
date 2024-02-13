;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Ugur Bolat
;;
;; Author: Ugur Bolat <dev@bolat.xyz>
;; Maintainer: Ugur Bolat <dev@bolat.xyz>
;; Created:  February 13, 2024

;; Package-Requires: ((emacs "28.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an Doom Emacs configuration file that aims at development environment (python)
;; and research environment (org-mode, org-roam, citar, etc.)



;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Ugur Bolat"
      user-mail-address "dev@bolat.xyz")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)
;;(setq doom-theme 'doom-tomorrow-night)
;;(setq doom-theme 'doom-vibrant)
(if (version< emacs-version "29.0")
    (setq doom-theme 'doom-vibrant)
  (setq doom-theme 'modus-vivendi))


(setq org-directory "~/main/org/")

;; credentials
(load-file (expand-file-name "credentials.el" doom-user-dir))


;; check if ~/main/org/dots/remote.el exists, if so, load it
(let ((remote-el (expand-file-name "dots/remote.el" org-directory)))
  (if (file-exists-p remote-el)
      (load-file remote-el)))

;;(setq doom-font (font-spec :family "Ubuntu" :size 16 :height 1.0))
;;(setq doom-font (font-spec :size 22))
;;(setq doom-font (font-spec :family "Roboto Mono" :size 19 :weight 'medium))
;;(setq doom-font (font-spec :family "Fira Code" :size 19 :weight 'medium))
(setq doom-font (font-spec :family "Ubuntu Mono" :size 26 :weight 'normal))
;;
;; (setq
;;  doom-font (font-spec :family "Ubuntu" :size 16 :height 1.0)
;;  doom-big-font (font-spec :family "Ubuntu" :size 22 :height 1.0)
;;  ;;doom-font (font-spec :family "Fira Code" :size 16 :height 1.0)
;;  ;;doom-big-font (font-spec :family "Fira Code" :size 22 :height 1.0)
;;  )

;; traditional strip-down doom already does this
;; (menu-bar-mode -1)
;; (scroll-bar-mode -1)
;; (tool-bar-mode -1)
;; good defaults
(delete-selection-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
;; (setq inhibit-startup-screen t) ; open scratch buffer at startup
;; (setq initial-scratch-message "")
(setq indent-tabs-mode nil) ; stop using tabs to indent
(setq make-backup-files nil) ; stop creating ~ backup files
(require 'autorevert)
(global-auto-revert-mode 1) ; load recent changes done outside
(setq auto-revert-use-notify t) ; and notify
(add-hook 'dired-mode-hook 'auto-revert-mode)
(setq enable-local-variables 't) ; local variables are customizations in either file or directory
(setq scroll-step 1
      scroll-conservatively 10000) ;; better scroll: go one line up or down, not half the screen.
(global-hl-line-mode 1) ;; horizontal current line highlight
(setq cursor-type 'box)
(add-hook 'emacs-startup-hook #'(lambda () (size-indication-mode -1)) 'append)
(setq display-line-numbers-type nil)

(global-set-key (kbd "C-z") 'undo-only)
(global-set-key (kbd "C-S-z") 'undo-redo)


;; resizing windows,
;; better keybindings than default C-x {,} and C-x ^,= which are both hard to reach and multi-key combinations
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

;; margins
;; TODO
;; REF https://discourse.doomemacs.org/t/setting-window-margins-for-particular-buffers-org-in-particular/2868/8
;; (defvar +text-mode-left-margin-width 10
;;   "The `left-margin-width' to be used in `text-mode' buffers.")

;; (defun +setup-text-mode-left-margin ()
;;   (when (and (derived-mode-p 'text-mode)
;;              (eq (current-buffer) ; Check current buffer is active.
;;                  (window-buffer (frame-selected-window))))
;;     (setq left-margin-width (if display-line-numbers
;;                                 0 +text-mode-left-margin-width))
;;     (set-window-buffer (get-buffer-window (current-buffer))
;;                        (current-buffer))))

;; (add-hook 'window-configuration-change-hook #'+setup-text-mode-left-margin)
;; (add-hook 'display-line-numbers-mode-hook #'+setup-text-mode-left-margin)
;; (add-hook 'text-mode-hook #'+setup-text-mode-left-margin)

;; (defadvice! +doom/toggle-line-numbers--call-hook-a ()
;;   :after #'doom/toggle-line-numbers
;;   (run-hooks 'display-line-numbers-mode-hook))

;; (remove-hook 'text-mode-hook #'display-line-numbers-mode)

(after! hl-todo
  (setq hl-todo-keyword-faces
        '(("TODO" . "#ff4500")
          ("DONT" . "#70b900")
          ("NEXT" . "#b6a0ff")
          ("BUG" . "#C70039")
          ("DONE" . "#44bc44")
          ("NOTE" . "#d3b55f")
          ("WARN" . "#d3145f")
          ("HOLD" . "#c0c530")
          ("HACK" . "#d0bc00")
          ("FAIL" . "#ff8059")
          ("WORKAROUND" . "#ffcccc")
          ("FIXME" . "#ff9077")
          ("REVIEW" . "#6ae4b9")
          ("DEPRECATED" . "#bfd9ff")
          ("REF" . "#660066"))))

(use-package! minions
  ;;:defer 1
  :after doom-modeline
  :config
  (setq doom-modeline-minor-modes t)
  (minions-mode 1))

;; dired ediff marked files
;;;###autoload
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

(after! dired
  (setq delete-by-moving-to-trash 't)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(use-package! dired-x
  ;;  :defer 1
  :after dired
  :bind*
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("<C-tab>" . dired-subtree-cycle)
        ("e" . ub/ediff-files))
  ;; (global-set-key (kbd "C-c o .") 'dired-jump)
  :config
  (setq dired-subtree-use-backgrounds nil)
  (when (eq system-type 'gnu/linux)
    (setq dired-guess-shell-alist-user
          '(("\\.mp3\\'"  "mpv")
            ("\\.mp4\\'"  "mpv")
            ("\\.m4a\\'"  "mpv")
            ("\\.webm\\'" "mpv")
            ("\\.mkv\\'"  "mpv")
            ("\\.avi\\'"  "mpv")

            ("\\.pdf\\'" "okular")
            ("\\.pd\\'"  "okular")
            ("\\.dvi\\'" "okular")

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
            ("\\.svg\\'" "eog")))))

(use-package! real-auto-save
  :defer 10
  :config
  (message "setup real-auto-save")
  (setq real-auto-save-interval 60)
  :hook
  ((prog-mode-hook . real-auto-save-mode)
   (org-mode-hook . real-auto-save-mode)))

(use-package! ace-window
  :defer 1
  :config
  (global-set-key (kbd "C-x o") 'ace-window))


(use-package! aggressive-indent
  :defer 1
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))


;;;###autoload
(defun ub/insert-commit-prefix-w-emoji ()
  (interactive)
  (let* ((string-list
          '(":tada: init: "
            ":construction: wip: "
            ":christmas-tree: christmas tree bill (torba yasa)"
            ":bookmark: tag: "
            ":sparkles: feat: "
            ":bug: fix: "
            ":books: docs: "
            ":lipstick: style: "
            ":hammer: refactor: "
            ":rotating_light: test: "
            ":smiling-imp: customize:"
            ":wrench: chore:"
            ":ok_hand: review: "
            ":card_index: meta: "
            ;;":bulb: source: "
            ":racehorse: perf: "
            ":white_check_mark: addTest: "
            ":heavy_check_mark: passTest: "
            ":zap: update: "
            ":art: fmt: "
            ":fire: remove: "
            ":truck: move: "
            ":green_heart: ci: "
            ":lock: sec: "
            ":arrow_up: upDep: "
            ":arrow_down: downDep: "
            ":shirt: lint: "
            ;;":alien: i18n: "
            ;;":pencil: txt: "
            ":ambulance: hotfix: "
            ":rocket: deploy: "
            ":apple: fixMac: "
            ":penguin: fixLinux: "
            ":checkered_flag: fixWin: "
            ":construction_worker: ciBuild: "
            ":chart_with_upwards_trend: analytics: "
            ":heavy_minus_sign: removeDep: "
            ":heavy_plus_sign: addDep: "
            ":whale: docker: "
            ;;":wrench: config: "
            ;;":package: pkgJson: "
            ":twisted_rightwards_arrows: merge: "
            ":hankey: badCode: "
            ":rewind: revert: "
            ":boom: breaking: "
            ;;":wheelchair: a11y: "
            ))
         (selected-string (completing-read "Select a string: " string-list)))
    (insert selected-string)))


;;;;;;; search engines
;;(with-eval-after-load 'consult
(use-package! consult
  :bind*
  (("C-c f r" . consult-recent-file)
   ("C-x C-v" . consult-buffer)
   ("C-x C-'" . consult-grep)
   ("C-c f f" . consult-find)
   ("C-s" . consult-line)))
(setq consult-line-start-from-top 't)

;; TODO
(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(add-to-list 'tramp-remote-path "/home/dfki.uni-bremen.de/ubolat/miniconda3/envs/qinros/bin")
;;(add-to-list 'tramp-remote-path "/home/dfki.uni-bremen.de/ubolat/miniconda3/envs/tnrljax/bin")

;; python
(load-file (expand-file-name "python.el" doom-user-dir))


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

(use-package! org-ai
  ;; TODO how to defer/lazy-load?
  ;;:after org-mode
  :config
  ;;(require 'org-ai)
  (add-hook 'org-mode-hook #'org-ai-mode)
  (setq org-ai-openai-api-token (ub/load-key-openai-token))
  ;; if you are on the gpt-4 beta:
  (setq org-ai-default-chat-model "gpt-4")
  (setq chatgpt-temperature 0.1) ;; NOTE set 0.75, etc. if you want creativity/hallicunation
  ;;(setq org-ai-default-max-tokens 4096)
  ;; if you are using yasnippet and want `ai` snippets
  (org-ai-install-yasnippets))


;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (
         (prog-mode . copilot-mode)
         (org-mode . copilot-mode))
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :bind (:map prog-mode-map
              ;; C-S-; NOTE C-; good old unintelligent complete
              ("C-:" . 'copilot-complete)))

;; NOTE These doesn't work
;;(require 'highlight-indent-guides)
;; (set-face-foreground 'highlight-indent-guides-odd-face "red")
;; (set-face-background 'highlight-indent-guides-even-face "red")
;; (set-face-background 'highlight-indent-guides-character-face "red")
;; NOTE only character works but odd and even doesn't work
;; (doom-themes-set-faces 'user
;;   ;;'(default :background "red" :foreground "blue")
;;   ;;'(highlight-indent-guides-odd-face :background "red")
;;   ;;'(highlight-indent-guides-even-face :background "red")
;;   '(highlight-indent-guides-character-face :background "red")
;;   )

;; (doom-themes-set-faces 'user
;;   '(modus-themes-hl-line :background "red") ;; in editor
;; ;;  '(highlight :background "red")
;;   )

;; attempting to change the background color of highlight in minibuffer
;; not visible in modus-vivendi
;; (custom-set-faces!
;;   ;;'(cursor :background "red")
;;   '(highlight :background "red" :foreground "red")
;;   )
;; (doom-themes-set-faces 'user
;;   ;;'(modus-themes-hl-line :background "red") ;; in editor
;;   '(highlight :background "red" :foreground "red")
;;   ;;  '(highlight :background "red")
;;   )

;; ;; (custom-theme-set-faces 'doom-one
;; ;;                         '(highlight ((t . ((:foreground "blue")
;; ;;                                            (:background "black")))))
;; ;;                         )

;; (set-face-attribute 'highlight nil :foreground "#000000")


;; make the guides pop more
(after! highlight-indent-guides
  (setq highlight-indent-guides-auto-odd-face-perc 12)
  (setq highlight-indent-guides-auto-even-face-perc 12)
  (setq highlight-indent-guides-auto-character-face-perc 12))

(after! company
  (setq company-idle-delay nil)
  (setq company-minimum-prefix-length 1))

;; NOTE these doesn' work..
;; (use-package! spell-fu
;;   :defer t
;;   :config
;;   (remove-hook 'text-mode-hook 'spell-fu-mode))
;; (after! spell-fu
;;   (remove-hook 'text-mode-hook 'spell-fu-mode))

;; NOTE this works but not nice
;;(remove-hook 'text-mode-hook 'spell-fu-mode)
;;
(use-package! spell-fu
  :init
  (remove-hook 'text-mode-hook 'spell-fu-mode))

(use-package! org-roam
  :custom
  (org-roam-directory (file-truename "~/main/org/roamable/zk")))

(use-package! citar
  :config
  (setq citar-library-file-extensions
        '("pdf" "jpg" "png" "epub" "mp4" "mp3" "djvu" "txt"
          "doc" "docx" "ppt" "pptx" "xls" "xlsx" "odt" "odp" "ods")
        citar-file-additional-files-separator "_")
  :custom
  (citar-notes-paths '("~/main/org/roamable/zk/bib"))
  (citar-bibliography '("~/main/library/bib/bolatu/0_Reference_Materials.bib"
                        "~/main/library/bib/bolatu/1_Papers_Archive.bib"
                        "~/main/library/bib/bolatu/2_Hot_Projects.bib")))

(after! realgud
  (setq realgud-safe-mode nil))


;; activity-watch
(require 'magit-process)
(global-activity-watch-mode)
;; (defun disable-activity-watch-tramp ()
;;   (when (and (buffer-file-name) (file-remote-p (buffer-file-name)))
;;     (activity-watch-mode -1)
;;     (message (format "activity-watch-mode for %s is %s" buffer-file-name activity-watch-mode))))
;; (add-hook 'find-file-hook 'disable-activity-watch-tramp)


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
;;
(use-package! page-break-lines
  :defer t
  ;; enabled in prog-mode
  :hook (prog-mode . page-break-lines-mode)
  ;; :config
  ;; (global-page-break-lines-mode)
  )

;; TODO
;; REF https://github.com/doomemacs/doomemacs/issues/1454
;; not possible to disable w/ (package! anaconda-mode :disable t) as it has dependencies
;; might be slowing down tramp/remote editing
;; whenever I open a python file, it takes more time than other files...
;; disable anaconda-mode in python, only enable when manually called
(use-package! anaconda-mode
  :defer t
  :hook (python-mode . (lambda () (anaconda-mode -1)))
  ;; :config
  ;; (setq anaconda-mode-eldoc-as-single-line t)
  ;; (setq anaconda-mode-eldoc-as-single-line)
  )
(with-eval-after-load 'anaconda-mode
  (remove-hook 'python-mode-local-vars-hook '+python-init-anaconda-mode-maybe-h))

(after! anaconda-mode
  (package! anaconda-mode :disable t))
