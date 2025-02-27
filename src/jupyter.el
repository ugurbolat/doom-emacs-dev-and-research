;;; src/jupyter.el -*- lexical-binding: t; -*-

;; See the discussion: https://github.com/emacs-jupyter/jupyter/issues/464
;; The current setup uses specific commit to avoid the issue related zmq

;; TODO current setup disables zmq, which is faster and async?
;; so currently experience freezing sessions
(use-package! jupyter
  ;;:ensure (:fetcher github :repo "emacs-jupyter/jupyter")
  ;;:defer 0.1
  :config
  (require 'ob-jupyter)
  (require 'jupyter)
  (setq jupyter-use-zmq nil)

  ;; NOTE doesn't work...
  ;; ;; Set default environment variables for Jupyter kernels
  ;; (setq jupyter-kernel-process-environment
  ;;       '("PYTHONBREAKPOINT=0"
  ;;         "XLA_PYTHON_CLIENT_PREALLOCATE=false"))

  ;; REF: https://github.com/emacs-jupyter/jupyter/issues/500
  (defun my-jupyter-api-http-request--ignore-login-error-a
      (func url endpoint method &rest data)
    (cond
     ((member endpoint '("login"))
      (ignore-error (jupyter-api-http-error)
        (apply func url endpoint method data)))
     (:else
      (apply func url endpoint method data))))
  (advice-add
   #'jupyter-api-http-request
   :around #'my-jupyter-api-http-request--ignore-login-error-a)
  )

(use-package! org
  :config

  ;; jupyter-python
  (setq org-babel-default-header-args:jupyter
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

  (add-to-list 'org-babel-load-languages '(jupyter . t))
  ;; (add-to-list 'org-src-lang-modes '("jupyter" . python))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  )

;; REF https://github.com/emacs-jupyter/jupyter/issues/500
;; BUG latest Jupyter : REST API error: 404, "Not found" #500
;; if you have this issue, make sure to install/download the jupyter
;; install jupyter_server=1.23.4
