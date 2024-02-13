;;; python.el -*- lexical-binding: t; -*-
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


(setenv "WORKON_HOME" "/home/bolatu/miniconda3/envs")
;; (use-package pyvenv
;;   :config
;;   (setenv "WORKON_HOME" "/home/bolatu/miniconda3/envs")
;;                                         ;(setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[" pyvenv-virtual-env-name "] ")))
;;   ;;(pyvenv-tracking-mode 1)
;;   )

(after! pyvenv
  (setenv "WORKON_HOME" "/home/bolatu/miniconda3/envs"))

(defun ub/enable-eglot ()
  "Enable eglot in the current Python buffer after pyvenv activation."
  (interactive)
  (when (derived-mode-p 'python-mode)
    (eglot-ensure)))

;; (after! pyvenv
;;   (add-hook 'pyvenv-post-activate-hooks #'ub/enable-eglot))
;; (if (featurep 'pyvenv)
;;     (add-hook 'pyvenv-post-activate-hooks #'ub/enable-eglot)
;;   (message "pyvenv is not installed so cannot hook eglot to pyvenv"))

(after! pyvenv
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                ;; remove tramp prefix from pyvenv-virtual-env due to ssh or docker which starts with /ssh: or /docker: and ends with :/
                (setq pyvenv-virtual-env (replace-regexp-in-string "/.*:" "" pyvenv-virtual-env))
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3"))
                ;; default (setq realgud:pdb-command-name "python -m pdb")
                (setq realgud:pdb-command-name (concat pyvenv-virtual-env "bin/python -m pdb"))
                )
              ))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")
                (setq realgud:pdb-command-name "python -m pdb")
                ))))

;; setting same face w/ eglot highlight
(custom-set-faces
 '(eglot-highlight-symbol-face ((t (:foreground "#ffffff" :background "#10387c")))))

(with-eval-after-load 'python
  (let ((map-var python-mode-map))
    (define-key map-var (kbd "C-c C-s") #'quickrun)
    (define-key map-var (kbd "<tab>") 'python-indent-shift-right)
    (define-key map-var (kbd "S-<tab>") 'python-indent-shift-left)
    (define-key map-var [S-iso-lefttab] 'python-indent-shift-left)
    (define-key map-var (kbd "C-c C-i") 'pyimport-insert-missing)
    ;;(define-key map-var (kbd "C-c C-b") 'python-black-region)
    (define-key map-var (kbd "C-c C-b") '+format/region)))

;; fixing auto-completion at realgub:pdb/python debbugging
(use-package pdb-capf
  :defer 1
  :config
  (add-hook 'pdb-track-mode-hook
	    (lambda ()
	      (add-hook 'completion-at-point-functions
                        'pdb-capf nil t))))


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

(setq org-babel-default-header-args:jupyter-python
      '((:results . "both")
        (:session . "jupyter-python-default")
        (:kernel . "python3")
        (:pandoc . "t")
        (:exports . "both")
        (:cache .   "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:async . "yes")
        (:eval . "never-export")))



;;;;;;;;; juypter+babel


;; add environment variable to ignore breakpoints not to go into pdb mode
;; python's repl is used for running full file
(with-eval-after-load 'python
  (setq python-shell-process-environment '("PYTHONBREAKPOINT=\"0\"")))
;; (with-eval-after-load 'python
;;   (setq python-shell-process-environment '("XLA_PYTHON_CLIENT_PREALLOCATE=\"false\"")))


(defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))
