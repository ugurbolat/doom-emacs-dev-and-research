
(message "python.el load start")

;; TODO add feature to run multiple quickrun buffers...
(use-package! quickrun
  ;;:ensure (:fetcher github :repo "emacsorphanage/quickrun")
  ;;:defer t
  :after quickrun
  :custom
  (quickrun-focus-p 't)
  (quickrun-timeout-seconds -1)
  :bind
  ("C-c r r" . quickrun)
  :config
  (quickrun-add-command "python-color"
    '((:command . "python")
      (:exec . "%c -m colored_traceback %s")
      (:tempfile . nil)))

  ;;(remove-hook 'quickrun-after-run-hook '+eval-quickrun-shrink-window-h)
  (remove-hook 'quickrun-after-run-hook '+eval-quickrun-scroll-to-bof-h)

  ;; TODO put vertical maybe
  ;; TODO after end of the process it goes to the beginning of the buffer, fix it
  ;;(set-popup-rule! "^\\*quickrun" :size 0.3 :ttl 0 :slot

  )

;; TODO
;; REF https://github.com/doomemacs/doomemacs/issues/1454
;; not possible to disable w/ (package! anaconda-mode :disable t) as it has dependencies
;; might be slowing down tramp/remote editing
;; whenever I open a python file, it takes more time than other files...
;; disable anaconda-mode in python, only enable when manually called
(use-package! anaconda-mode
  ;;:defer t
  :hook (python-mode . (lambda () (anaconda-mode -1)))
  ;; :config
  ;; (setq anaconda-mode-eldoc-as-single-line t)
  ;; (setq anaconda-mode-eldoc-as-single-line)
  )
(with-eval-after-load 'anaconda-mode
  (remove-hook 'python-mode-local-vars-hook '+python-init-anaconda-mode-maybe-h))


(with-eval-after-load 'python
  (let ((map-var python-mode-map))
    ;;(define-key map-var (kbd "C-c C-s") #'quickrun-shell)
    (define-key map-var (kbd "<tab>") 'python-indent-shift-right)
    (define-key map-var (kbd "S-<tab>") 'python-indent-shift-left)
    (define-key map-var [S-iso-lefttab] 'python-indent-shift-left)
    (define-key map-var (kbd "C-c C-i") 'pyimport-insert-missing)
    (define-key map-var (kbd "C-c C-b") 'python-black-region)))

;; add environment variable
;; we modify environment variables for dev. mode
(with-eval-after-load 'python
  (setq python-shell-process-environment
	'(
	  ;; disabling breakpoints
	  ;; to ignore breakpoints not to go into pdb mode
	  ;; python's repl is used for running uninterrupted
	  ;; if you want debugging, call explicitly pdb
	  "PYTHONBREAKPOINT=\"0\""
	  ;; disabling jax's gpu memory preallocation which %90 of the mem.
	  "XLA_PYTHON_CLIENT_PREALLOCATE=\"false\""  
	  )
        ))


(use-package! pyvenv
  ;;:ensure (:fetcher github :repo "jorgenschaefer/pyvenv")
  ;;:defer t
  :config
  (setenv "WORKON_HOME" "/home/bolatu/miniconda3/envs")
  ;; TODO
  ;; (pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                ;; remove tramp prefix from pyvenv-virtual-env due to ssh or docker which starts with /ssh: or /docker: and ends with :/
                (setq pyvenv-virtual-env (replace-regexp-in-string "/.*:" "" pyvenv-virtual-env))
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3"))
                (setq realgud--ipdb-command-name (concat pyvenv-virtual-env "bin/python -m ipdb"))
                ;; (setq realgud:pdb-command-name "pyth on -m pdb")
                (setq realgud:pdb-command-name (concat pyvenv-virtual-env "bin/python -m pdb")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")
                (setq realgud:pdb-command-name "python -m pdb")))))


(use-package! pdb-capf
  ;;:ensure (:fetcher github :repo "ugurbolat/emacs-pdb-capf")
  :defer 0.1
  ;;:defer t
  :config
  (add-hook 'pdb-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        'pdb-capf nil t)))
  (add-hook 'pdb-track-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        'pdb-capf nil t))))



(use-package! ox-ipynb)
;; NOTE for the bug if still exists
;;; ;; BUG Refresh inline images after executing src blocks
;; (after! ox-ipynb
;;   (advice-remove 'org-display-inline-images #'font-lock-fontify-buffer))



;; REF: https://github.com/minad/consult?tab=readme-ov-file#grep-and-find
;; search pattern #regexps#filter-string
;; regexps: regular expression to search for
;; filter-string: string to filter the results withing the first search
;; TODO start wihout hashtag and search for files with consult-ripgrep
;; (defun ub/consult-ripgrep-wo-hashtag (&optional dir initial)
;;   (interactive)
;;   (let ((consult--async-split-initial "asd"))
;;     (consult-ripgrep dir initial)))
;; (defun my-consult-ripgrep-advice (orig-func &optional dir initial)
;;   "Advice to adjust initial input for `consult-ripgrep`.
;; Remove the automatic `#` prefix if present."
;;   (interactive "P")
;;   ;; Check if initial exists and modify it, or set it to empty as an example.
;;   (let ((new-initial (if (string-prefix-p "#" initial)
;;                          (substring initial 1)
;;                        (or initial ""))))
;;     (funcall orig-func dir new-initial)))
;; (advice-add 'consult-ripgrep :around #'my-consult-ripgrep-advice)



;; TODO considering searching with consult etc.
(defun ub/python-summary-on-current-buffer-w-occur ()
  (interactive)
  (occur "\\<def\\>\\|\\<class\\>"))

(defun ub/python-search-uncommented-breakpoint-on-current-buffer-w-occur ()
  (interactive)
  (occur "^[^#]*breakpoint()"))

(defun ub/python-commented-breakpoint-on-current-buffer-w-occur ()
  (interactive)
  (occur "^[^#]*#.*breakpoint("))

;; TODO customize grep output face/style with awk to align the output
;; (setq grep-command
;;       "find . -type f ! -path './' -exec grep --color=auto -nH --null -e YOUR_PATTERN_HERE {} + | awk '{printf \"%s:%-4s :%s\\n\", $1,$2,$3}'")

(defun ub/python-summary-on-current-directory-w-occur ()
  (interactive)
  (rgrep "\\<def\\>\\|\\<class\\>" "*.py" "./" nil))

;; (defun ub/python-search-uncommented-breakpoint ()
;;   (interactive)
;;   (ub/consult-ripgrep-current-dir "'^[^#]*breakpoint()'"))
(defun ub/python-search-uncommented-breakpoint-w-consult()
  (interactive)
  (ub/consult-ripgrep-current-dir "breakpoint()#^[^#]*breakpoint()"))

;; (defun ub/python-commented-breakpoint ()
;;   (interactive)
;;   (ub/consult-ripgrep-current-dir "'^[^#]*#.*breakpoint()'"))
(defun ub/python-commented-breakpoint-w-consult ()
  (interactive)
  (ub/consult-ripgrep-current-dir "breakpoint()##"))

(defun ub/python-summary-w-consult ()
  (interactive)
  (ub/consult-ripgrep-current-dir "'\\<def\\>\\|\\<class\\>'"))

;; TODO
;; (defun ub/python-search-uncommented-breakpoint-w-wgrep()
;;   (interactive)
;;   (ub/consult-ripgrep-current-dir "breakpoint()#^[^#]*breakpoint()")
;;   (embark-export)
;;   (wgrep-change-to-wgrep-mode)
;;   )


(message "python.el load end")
