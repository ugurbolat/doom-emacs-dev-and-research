
(message "completion.el load start")

;; Here is where the magic happens for our (Modern) Emacs.

;; MINIBUFFER
;; vertico, orderless, consult, marginalia, embark


;; functions that use `consult-find` w/ default dirs
;; signature (defun consult-find (&optional dir initial)
(defun ub/consult-find-main (&optional initial)
  "Search for files with `find' in ~/main."
  (interactive "P")
  (consult-find "~/main" initial))
(defun ub/consult-find-home (&optional initial)
  "Search for files with `find' in ~/"
  (interactive "P")
  (consult-find "~/" initial))
(defun ub/consult-find-org (&optional initial)
  "Search for files with `find' in ~/main/org."
  (interactive "P")
  (consult-find "~/main/org" initial))
(defun ub/consult-find-zotero-pdf (&optional initial)
  "Search for files with `find' in ~/main/org."
  (interactive "P")
  (consult-find "~/main/library/Zotero-Library" initial))

;; same for `consult-ripgrep`
(defun ub/consult-ripgrep-main (&optional initial)
  "Search for files with `grep' in ~/main."
  (interactive "P")
  (consult-ripgrep "~/main" initial))
(defun ub/consult-ripgrep-home (&optional initial)
  "Search for files with `grep' in ~/"
  (interactive "P")
  (consult-ripgrep "~/" initial))
(defun ub/consult-ripgrep-org (&optional initial)
  "Search for files with `grep' in ~/main/org."
  (interactive "P")
  (consult-ripgrep "~/main/org" initial))
(defun ub/consult-ripgrep-current-dir (&optional initial)
  "Search for files with `grep' in current directory."
  (interactive "P")
  (consult-ripgrep "./" initial))


;; `consult-find` that asks the user dir which can be found interactively in minibuffer
(defun ub/consult-find-interactive ()
  "Search for files with `find' in a directory."
  (interactive)
  (consult-find (read-directory-name "Directory: ")))
;; same for `consult-ripgrep`
(defun ub/consult-ripgrep-interactive ()
  "Search for files with `grep' in a directory."
  (interactive)
  (consult-ripgrep (read-directory-name "Directory: ")))


(use-package! consult
  ;;:ensure (:fetcher github :repo "minad/consult")
  ;;:defer 0.1
  ;;:after vertico
  :bind
  (("C-c f r" . consult-recent-file)
   ("C-x C-v" . consult-buffer)
   ("C-x C-'" . ub/consult-ripgrep-current-dir)
   ("C-S-s" . consult-line))
  :custom
  (consult-line-start-from-top nil))


;; IN BUFFER
;; corfu, orderless, cape, eglot, copilot


;; bind completion-at-point to C-;
(use-package! emacs
  ;;:ensure nil
  :bind
  ("C-;" . completion-at-point))

(use-package! corfu
  ;;:ensure (:fetcher github :repo "minad/corfu")
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  :config
  ;;(corfu-echo-mode t) ;; either show up echo or popup
  (corfu-history-mode t)
  (corfu-popupinfo-mode t)
  )
;; A few more useful configurations...
(use-package! emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  ;; TODO
  (setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))
;; corfu's eglot related configs
;; REF https://github.com/minad/corfu/wiki#configuring-corfu-for-eglot
;; Option 1: Specify explicitly to use Orderless for Eglot
(setq completion-category-overrides '((eglot (styles orderless))
                                      (eglot-capf (styles orderless))))
(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)


(use-package! nerd-icons-corfu
  ;;:ensure (:fetcher github :repo "LuigiPiucco/nerd-icons-corfu")
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


(use-package! cape
  ;;:ensure (:fetcher github :repo "minad/cape")
  :init
  (global-corfu-mode))
;; Add extensions
(use-package! cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :config
  ;; NOTE "This feature is EXPERIMENTAL and should only be used carefully in special scenarios."
  ;; Merge the dabbrev, dict and keyword capfs, display candidates together.
  (setq-local completion-at-point-functions
              (list (cape-capf-super #'cape-dabbrev #'cape-dict #'cape-keyword)))
  ;; Alternative: Define named Capf instead of using the anonymous Capf directly
  (defun cape-dabbrev-dict-keyword ()
    (cape-wrap-super #'cape-dabbrev #'cape-dict #'cape-keyword))
  (setq-local completion-at-point-functions (list #'cape-dabbrev-dict-keyword))
  )


;; note that eglot is built-in since emacs 29
;; eglot customizations
(use-package! emacs
  ;;:ensure nil
  :custom-face
  ;; NOTE setting font in early-init to avoid jitter/flashing
  ;;(default ((t (:height 160)))) ; set font size
  (eglot-highlight-symbol-face ((t (:foreground "#ffffff" :background "#10387c")))))

;; eglot's resized echo area display too annoying
;; REF https://joaotavora.github.io/eglot/#Eglot-Features
;; REF https://www.reddit.com/r/emacs/comments/16nnlwa/turn_of_eldoc_in_eglot_without_turning_of_symbol/
(use-package! eglot
  ;;:ensure nil
  :defer t
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-echo-area-prefer-doc-buffer t))


;;;###autoload
(defun copilot-shutup ()
  (interactive)
  (setq copilot-idle-delay nil))

;;;###autoload
(defun copilot-keep-talking ()
  (interactive)
  (setq copilot-idle-delay 0))

(use-package! copilot
  :defer t
  ;;:ensure (:fetcher github :repo "zerolfx/copilot.el")
  :hook (
         (prog-mode . copilot-mode)
         ;;(org-mode . copilot-mode)
         (copilot-mode . (lambda ()
                           (setq-local copilot--indent-warning-printed-p t))))
  :config
  ;; disable idle delay for displaying instant completions which can be annoying
  (setq copilot-idle-delay 0)

  (setq copilot-indent-offset-warning-disable 't)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :bind (:map prog-mode-map
              ;; C-S-; NOTE C-; good old unintelligent complete
              ("C-:" . 'copilot-complete)))
;; NOTE copilot--start-agent: Node 18+ is required but found 16.2
;; but works with other doom-emacs version, upgrading might break the other setups...


(message "completion.el load end")
