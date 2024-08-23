;;; src/org-roam-config.el -*- lexical-binding: t; -*-

(use-package! org-roam
  ;;:ensure (:fetcher github :repo "org-roam/org-roam")
  ;;:defer 0.1
  ;;:defer t  ; doom already defers it
  :custom
  (org-roam-directory ub/roam-root-dir)
  (org-roam-db-location (expand-file-name ".roam.db" ub/roam-root-dir))
  ;; TODO migrate roam capture
  ;;(org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      ;;"* %<%H:%M> %?\n:PROPERTIES:\n:CREATED: %U\n:END:"
      ;; cleaner
      "* %<%H:%M> %?"
      :target (file+head
               "%<%Y-%m-%d>.org"
               "#+TITLE: Dailies - %<%Y-%m-%d>
#+CREATED: %<%Y-%m-%d %H:%M>
#+LAST_MODIFIED: %<%Y-%m-%d %H:%M>
#+CATEGORY: roam
#+FILETAGS: :daily:daily_file:\n")
      :jump-to-captured t
      )))

  (org-roam-capture-templates
   `(("d" "default" plain
      "%?"
      :target
      (file+head
       ;;"%<%Y%m%d%H%M%S>-${slug}.org"
       ;;"%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/dump/${citar-citekey}.org"
       "%(concat ub/roam-root-dir (format-time-string \"/dump/%y%m%d%H%M%S-\" (current-time) t) (s-left 70 \"${slug}\") \".org\")"
       ,(concat
         ":PROPERTIES:\n"
         ":CREATED: %U\n"
         ":END:\n"
         "#+TITLE: ${title}\n"
         "#+CREATED: %<%Y-%m-%d %H:%M>\n"
         "#+LAST_MODIFIED: %<%Y-%m-%d %H:%M>\n"
         "#+CATEGORY: roam\n"
         "#+FILETAGS: :dump:dump_file:\n"))
      :unnarrowed t
      :jump-to-captured t)
     ("l" "literature note" plain
      "%?"
      :target
      (file+head
       ;;"%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/bib/${citar-citekey}.org"
       "%(concat ub/roam-root-dir \"/bib/${citar-citekey}.org\")"
       ;; TODO ${title} :: ${first-author}
       "#+TITLE: ${note-title}
#+CREATED: %<%Y-%m-%d %H:%M>
#+LAST_MODIFIED: %<%Y-%m-%d %H:%M>
#+CATEGORY: roam
#+FILETAGS: :bib:bib_file:
#+SETUPFILE: /home/bolatu/main/org/templates/latex-include-header/bib_bolatu_linux.org
#+SETUPFILE: /home/bolatu/main/org/templates/latex-include-header/org_latex_report.org


* References
#+LATEX: \\printbibliography[heading=none]
")
      :unnarrowed t
      :jump-to-captured t)

     ;; ("b" "beamer" plain
     ;;  "%?"
     ;;  :target
     ;;  ;; TODO cont.
     ;;  ;; REF see /home/bolatu/main/org/roamable/zk/230725205154-dfki_ric_rl_discussion_paper_presentation_quantum_tensor_networks_for_.org
     ;;  :unnarrowed t)

     ))

  ;;#+STARTUP: beamer
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ;;("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n j" . ub/org-roam-dailies-capture-today-respect-my-midnight))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))

  (require 'org-roam-protocol)

  ;; org-roam-protocol capture templates
  (setq roam-ref-cap-temp-list `())
  (add-to-list 'roam-ref-cap-temp-list
               `("i" "Dump" plain
                 ,(concat
                   ;;"\n\n- tags ::"
                   "\n\n")
                 :if-new
                 (file+head
                  "%(concat ub/roam-dump-dir (format-time-string \"/%y%m%d%H%M%S-\" (current-time) t) (s-left 70 \"${slug}\") \".org\")"
                  ;;"bib/${citekey}.org"
                  ,(concat
                    ":PROPERTIES:\n"
                    ":CREATED: %U\n"
                    ":ROAM_REFS: ${ref}\n"
                    ":END:\n"
                    "#+TITLE: ${title}\n"
                    "#+CREATED: %<%Y-%m-%d %H:%M>\n"
                    "#+LAST_MODIFIED: %<%Y-%m-%d %H:%M>\n"
                    "#+CATEGORY: roam\n"
                    "#+FILETAGS: :dump:web:dump_file:\n"))
                 :unnarrowed t)
               t)
  (add-to-list 'roam-ref-cap-temp-list
               `("YY" "Dump Video (bib)" plain
                 ,(concat
                   ;;"\n\n- tags ::"
                   "\n\n")
                 :if-new
                 (file+head
                  "%(concat ub/roam-dump-dir (format-time-string\"/%y%m%d%H%M%S-\" (current-time) t) (s-left 70 \"${slug}\") \".org\")"
                  ;;"bib/${citekey}.org"
                  ,(concat
                    ":PROPERTIES:\n"
                    ":CREATED: %U\n"
                    ":MPV_URL: %i\n"
                    ":ROAM_REFS: ${ref}\n"
                    ":END:\n"
                    "#+TITLE: ${title}\n"
                    "#+CREATED: %<%Y-%m-%d %H:%M>\n"
                    "#+LAST_MODIFIED: %<%Y-%m-%d %H:%M>\n"
                    "#+CATEGORY: roam\n"
                    "#+FILETAGS: :dump:web:b@video:dump_file\n"))
                 :unnarrowed t)
               t)
  (setq org-roam-capture-ref-templates roam-ref-cap-temp-list)



  ;; we don't want to dailies relative to the org-roam-directory
  ;; HACK
  (defcustom org-roam-dailies-directory-abs-path nil
    "Absolute path to daily-notes, if non-nil."
    :group 'org-roam
    :type '(choice
            (const :tag "None" nil)
            (directory :tag "Absolute path")))

  (defun org-roam--resolve-dailies-dir ()
    (or org-roam-dailies-directory-abs-path
        (expand-file-name org-roam-dailies-directory org-roam-directory)))

  (defun org-roam--override-dailies-dir (orig-fun &rest args)
    (let ((org-roam-dailies-directory (org-roam--resolve-dailies-dir)))
      (apply orig-fun args)))

  ;; below are the functions that need to be overriden since they use org-roam-directory
  (dolist (fn '(org-roam-dailies-find-directory
                org-roam-dailies--daily-note-p
                org-roam-dailies--list-files
                org-roam-dailies-calendar-mark-entries
                org-roam-dailies--capture))
    (advice-add fn :around #'org-roam--override-dailies-dir))

  (setq org-roam-dailies-directory-abs-path "/home/bolatu/main/org/roam/daily/")



  (org-roam-db-autosync-mode)
  )
