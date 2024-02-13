;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
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

(package! emacs-rotate
  :recipe (:host github
           :repo "daichirata/emacs-rotate"))

(package! minions
  :recipe (:host github :repo "tarsius/minions"))

(package! dired-hacks
  :recipe (:host github :repo "Fuco1/dired-hacks"))

(package! real-auto-save
  :recipe (:host github :repo "ChillarAnand/real-auto-save"))

(package! ace-window
 :recipe (:host github
          :repo "abo-abo/ace-window"))

(package! aggressive-indent
  :recipe (:host github
           :repo "Malabarba/aggressive-indent-mode"))



;; (package! realgud-ipdb
;;   :recipe (:host github
;;            :repo "realgud/realgud-ipdb"))

(package! emacs-pdb-capf
  :recipe (:host github
           :repo "ugurbolat/emacs-pdb-capf"))

(package! emacs-zmq
  :recipe (:host github
           :repo "nnicandro/emacs-zmq"
           ;;:build (:not compile)
           )
  :pin "af5299d80715b1083a18145e9c84ef9563020676"
  )

(package! jupyter
  :recipe (:host github
           :repo "emacs-jupyter/jupyter"
           ;;:branch "next"
           ;;:build (:not compile)
           )
  ;;:pin "455166712e606c9c6a8de763ea0a77548cadcef2"
  :pin "053a78da252b19cf59cefe6b83f9c4531a38d6b2"
  )

(package! org-ai
  :recipe (:host github
           :repo "rksm/org-ai"))

(package! copilot
  :recipe (:host github
           :repo "zerolfx/copilot.el"
           :files ("*.el" "dist")))

(if (version< emacs-version "29.0")
    (package! eglot
      :recipe (:host github
               :repo "joaotavora/eglot")
      :pin "8b5532dd32b25276c1857508030b207f765ef9b6")
  (package! eglot :built-in 'prefer))


(package! activity-watch-mode
  :recipe (:host github :repo "pauldub/activity-watch-mode"))

(package! dired-sidebar
  :recipe (:host github :repo "jojojames/dired-sidebar"))

(package! page-break-lines
  :recipe (:host github :repo "purcell/page-break-lines"))


;; (package! org
;;   :recipe (:host github
;;            :repo "emacs-straight/org"
;;            :files (:defaults "etc")
;;            ;; HACK Org requires a post-install compilation step to generate a
;;            ;;      org-version.el with org-release and org-git-version
;;            ;;      functions, using a 'git describe ...' call.  This won't work
;;            ;;      in a sparse clone and I value smaller network burdens on
;;            ;;      users over non-essential variables so we fake it:
;;            :build t
;;            :pre-build
;;            (with-temp-file "org-version.el"
;;              (insert "(defun org-release () \"9.5\")\n"
;;                      (format "(defun org-git-version (&rest _) \"9.5-??-%s\")\n" ;; but 9.6.1
;;                              (cdr (doom-call-process "git" "rev-parse" "--short" "HEAD")))
;;                      "(provide 'org-version)\n")))
;;   :pin "630f86dfc42472aafd9a4f305e1965cbe92b2891")

;; (package! org-contrib
;;   :recipe (:host github
;;            :repo "emacsmirror/org-contrib"
;;            :files (:defaults "etc"))
;;   :pin "fff6c888065588527b1c1d7dd7e41c29ef767e17")

;;(package! anaconda-mode :disable t)
