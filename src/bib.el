(message "bib.el load start")


;; REF:
;; https://gist.github.com/rka97/57779810d3664f41b0ed68a855fcab54
;; https://emacsconf.org/2021/talks/researc/h


;;(setq async-shell-command-buffer 'new-buffer)

(defun ub/async-shell-command-no-switch (orig-fun command &optional output-buffer error-buffer)
  "Run the async-shell-command without switching to the output buffer."
  (let ((display-buffer-alist
         (cons '("*Async Shell Command*" display-buffer-no-window)
               display-buffer-alist)))
    (funcall orig-fun command output-buffer error-buffer)))

;;(advice-add 'async-shell-command :around #'ub/async-shell-command-no-switch)


(defun ub/reformat-bib-library (&optional filename)
  "Formats the bibliography using biber & rebiber and updates the PDF -metadata."
  (interactive "P")
  (let ((cmnd (concat
               (format "cd %s && " ub/bib-rebiber-dir)
               ;;(format "rebiber -i %s &&" filename) ; Get converence versions of arXiv papers
               ;;(format "rebiber -i %s -o %s &&" filename filename-rebiber)
               ;;(format "rebiber -i ../bolatu/%s -o %s && " filename filename)
               ;;(format "rebiber -i /home/bolatu/main/library/rebibered/%s -o %s && " filename filename)
               (format "rebiber -i /home/bolatu/main/library/zotra/%s -o %s && " filename filename)
               ;;(format "biber --tool --output_align --output_indent=2 --output_fieldcase=lower --configfile=~/bib-lib/biber-myconf.conf --output_file=%s %s && " filename filename) ; Properly format the bibliography
               ;;(format "biber --tool --output_align --output_indent=2 --output_fieldcase=lower --output_file=%s %s" filename-biber-out filename-rebiber-out) ; Properly
               ;;(format "sed -i -e 's/arxiv/arXiv/gI' -e 's/journaltitle/journal     /' -e 's/date      /year      /' %s &&" filename) ; Some replacements
               ;;(format "git commit -m \"Updating bibliography..\" %s && git push" filename) ; Commit and push the updated bib
               "git diff > temp.diff && "
               "git commit -m \"Updating bibliography w rebiber yo..\" ."
               )))
    (async-shell-command cmnd)))


(defun ub/setup-rebiber-async-process ()
  "Setup the async process for rebibering the bib file on change."
  (setq async-shell-command-buffer 'new-buffer)
  (advice-add 'async-shell-command :around #'ub/async-shell-command-no-switch)

  ;; (add-hook 'bib-file-hot-change-hook #'(lambda () (ub/reformat-bib-library ub/bib-file-hot)))
  ;; (add-hook 'bib-file-archive-change-hook #'(lambda () (ub/reformat-bib-library ub/bib-file-archive)))
  ;; (add-hook 'bib-file-ref-change-hook #'(lambda () (ub/reformat-bib-library ub/bib-file-ref)))

  (require 'filenotify)

  (defvar my-file-change-debounce-timer nil
    "Timer for debouncing file change events.")
  (defvar my-file-change-debounce-timer-archive nil
    "Timer for debouncing file change events.")
  (defvar my-file-change-debounce-timer-hot nil
    "Timer for debouncing file change events.")


  (defvar bib-file-all-change-hook nil
    "Hook run after the specified file is changed.")

  (defun bib-file-all-change-callback (event)
    "Callback for file change events."
    (message "File Notify `all.bib` Change Event: %S" event)
    (when (eq (nth 1 event) 'changed)
      (unless my-file-change-debounce-timer-hot
        (setq my-file-change-debounce-timer-hot
              (run-with-timer 3.0 nil ; Set the debounce duration (in seconds) as needed
                              (lambda ()
                                (run-hooks 'bib-file-all-change-hook)
                                (setq my-file-change-debounce-timer-hot nil)))))))

  (defvar bib-file-all-monitor-descriptor nil
    "Descriptor for monitoring a specific file.")

  (let ((file-to-watch-hot (expand-file-name ub/bib-file-all ub/library-zotra-dir)))
    (setq bib-file-all-monitor-descriptor
          (file-notify-add-watch file-to-watch-hot
                                 '(change)
                                 'bib-file-all-change-callback)))

  (add-hook 'bib-file-all-change-hook #'(lambda () (ub/reformat-bib-library ub/bib-file-all)))



  ;; (defvar bib-file-hot-change-hook nil
  ;;   "Hook run after the specified file is changed.")

  ;; (defun bib-file-hot-change-callback (event)
  ;;   "Callback for file change events."
  ;;   (message "File Notify `hot-change` Event: %S" event)
  ;;   (when (eq (nth 1 event) 'changed)
  ;;     (unless my-file-change-debounce-timer-hot
  ;;       (setq my-file-change-debounce-timer-hot
  ;;             (run-with-timer 3.0 nil ; Set the debounce duration (in seconds) as needed
  ;;                             (lambda ()
  ;;                               (run-hooks 'bib-file-hot-change-hook)
  ;;                               (setq my-file-change-debounce-timer-hot nil)))))))

  ;; (defvar bib-file-hot-monitor-descriptor nil
  ;;   "Descriptor for monitoring a specific file.")

  ;; (let ((file-to-watch-hot (expand-file-name ub/bib-file-hot ub/bib-dir)))
  ;;   (setq bib-file-hot-monitor-descriptor
  ;;         (file-notify-add-watch file-to-watch-hot
  ;;                                '(change)
  ;;                                'bib-file-hot-change-callback)))

  ;; (add-hook 'bib-file-hot-change-hook #'(lambda () (ub/reformat-bib-library ub/bib-file-hot)))

  ;; ;; TODO ugly quick copy-paste of hooks - archive

  ;; (defvar bib-file-archive-change-hook nil
  ;;   "Hook run after the specified file is changed.")

  ;; (defun bib-file-archive-change-callback (event)
  ;;   "Callback for file change events."
  ;;   (message "File Notify `archive-change` Event: %S" event)
  ;;   (when (eq (nth 1 event) 'changed)
  ;;     (unless my-file-change-debounce-timer-archive
  ;;       (setq my-file-change-debounce-timer-archive
  ;;             (run-with-timer 3.0 nil ; Set the debounce duration (in seconds) as needed
  ;;                             (lambda ()
  ;;                               (run-hooks 'bib-file-archive-change-hook)
  ;;                               (setq my-file-change-debounce-timer-archive nil)))))))

  ;; (defvar bib-file-archive-monitor-descriptor nil
  ;;   "Descriptor for monitoring a specific file.")

  ;; (let ((file-to-watch-archive (expand-file-name ub/bib-file-archive ub/bib-dir)))
  ;;   (setq bib-file-archive-monitor-descriptor
  ;;         (file-notify-add-watch file-to-watch-archive
  ;;                                '(change)
  ;;                                'bib-file-archive-change-callback)))
  ;;                                       ;(lambda (event) (message "Deneme Event %S" event)))))

  ;; (add-hook 'bib-file-archive-change-hook #'(lambda () (ub/reformat-bib-library ub/bib-file-archive)))

  ;; ;; - ref

  ;; (defvar bib-file-ref-change-hook nil
  ;;   "Hook run after the specified file is changed.")

  ;; (defun bib-file-ref-change-callback (event)
  ;;   "Callback for file change events."
  ;;   (message "File Notify `ref-change` Event: %S" event)

  ;;   (when (eq (nth 1 event) 'changed)
  ;;     (unless my-file-change-debounce-timer
  ;;       (setq my-file-change-debounce-timer
  ;;             (run-with-timer 3.0 nil ; Set the debounce duration (in seconds) as needed
  ;;                             (lambda ()
  ;;                               (run-hooks 'bib-file-ref-change-hook)
  ;;                               (setq my-file-change-debounce-timer nil)))))))


  ;; (defvar bib-file-ref-monitor-descriptor nil
  ;;   "Descriptor for monitoring a specific file.")

  ;; (let ((file-to-watch-ref (expand-file-name ub/bib-file-ref ub/bib-dir)))
  ;;   (setq bib-file-ref-monitor-descriptor
  ;;         (file-notify-add-watch file-to-watch-ref
  ;;                                '(change)
  ;;                                'bib-file-ref-change-callback)))
  ;;                                       ;(lambda (event) (message "Deneme Event %S" event)))))

  ;; (add-hook 'bib-file-ref-change-hook #'(lambda () (ub/reformat-bib-library ub/bib-file-ref)))


  ;; ;; - paper dump bib file

  ;; (defvar bib-file-dump-change-hook nil
  ;;   "Hook run after the specified file is changed.")

  ;; (defun bib-file-dump-change-callback (event)
  ;;   "Callback for file change events."
  ;;   (message "File Notify `dump-change` Event: %S" event)

  ;;   (when (eq (nth 1 event) 'changed)
  ;;     (unless my-file-change-debounce-timer
  ;;       (setq my-file-change-debounce-timer
  ;;             (run-with-timer 3.0 nil ; Set the debounce duration (in seconds) as needed
  ;;                             (lambda ()
  ;;                               (run-hooks 'bib-file-dump-change-hook)
  ;;                               (setq my-file-change-debounce-timer nil)))))))


  ;; (defvar bib-file-dump-monitor-descriptor nil
  ;;   "Descriptor for monitoring a specific file.")

  ;; (let ((file-to-watch-dump (expand-file-name ub/bib-file-dump ub/bib-dir)))
  ;;   (setq bib-file-dump-monitor-descriptor
  ;;         (file-notify-add-watch file-to-watch-dump
  ;;                                '(change)
  ;;                                'bib-file-dump-change-callback)))
  ;;                                       ;(lambda (event) (message "Deneme Event %S" event)))))

  ;; (add-hook 'bib-file-dump-change-hook #'(lambda () (ub/reformat-bib-library ub/bib-file-dump)))


  )


(ub/run-if-server-active #'ub/setup-rebiber-async-process)


;; (use-package! parsebib
;;   ;;:ensure (:fetcher github :repo "joostkremers/parsebib")
;;   ;;:defer 0.1
;;   )
;; ;;(elpaca-wait)
;; ;;(add-to-list 'load-path (expand-file-name "builds/parsebib" elpaca-directory))

;; ;; FIXME doesn't load here..
;; (require 'parsebib)



(defun bibtex-to-org (bibfile orgfile)
  ;;(message "bibtex-to-org yoo start")
  (require 'parsebib)
  (let ((parsed-data nil)
        (bibtex-file (expand-file-name bibfile))
        (org-file (expand-file-name orgfile)))
    ;; Parsing the bibtex file
    (setq parsed-data (parsebib-parse bibtex-file :display t :fields '("title" "author" "file")))
    ;; Creating the org file
    (with-temp-file org-file
      (maphash
       (lambda (key value)
         (let* ((file (cdr (assoc-string "file" value)))
                (date (format-time-string "%Y-%m-%d %H:%M"))
                (raw-author (cdr (assoc-string "author" value)))
                (author-list (if raw-author (split-string raw-author " and ") nil))
                (author (if (and author-list (> (length author-list) 1))
                            (concat (car author-list) " et al.")
                          (car author-list)))
                (title (cdr (assoc-string "title" value))))
           (insert (format
                    "* %s :: %s
:PROPERTIES:
:CREATED: [%s]
:ROAM_REFS: @%s
:END:
[cite:@%s]
[[file:%s]]
\n\n\n"
                    title author date key key file))))
       parsed-data)))
  ;;(message "bibtex-to-org yoo end")
  )


;; (defun bibtex-to-org (bibfile orgfile)
;;   (let ((parsed-data nil)
;;         (bibtex-file (expand-file-name bibfile))
;;         (org-file (expand-file-name orgfile)))
;;     ;; Parsing the bibtex file
;;     (setq parsed-data (parsebib-parse bibtex-file :display t :fields '("title" "author" "file")))
;;     ;; Creating the org file
;;     (with-temp-file org-file
;;       (maphash
;;        (lambda (key value)
;;          (let* ((file (cdr (assoc-string "file" value)))
;;                 (date (format-time-string "%Y-%m-%d %H:%M"))
;;                 (raw-author (cdr (assoc-string "author" value)))
;;                 (author-list (if raw-author (split-string raw-author " and ") nil))
;;                 (author (if (and author-list (> (length author-list) 1))
;;                             (concat (car author-list) " et al.")
;;                           (car author-list)))
;;                 (title (cdr (assoc-string "title" value))))
;;            (insert (format "* %s :: %s
;; :PROPERTIES:
;; :CREATED: %s
;; :ROAM_REFS: @%s
;; :END:
;; [cite:@%s]
;; [[file:%s]]
;; \n\n\n"
;;                            title author key key file))))
;;        parsed-data))))


;; ;; Example usage
;; (bibtex-to-org
;;  "/home/bolatu/main/library/bib/bolatu_rebiber_test/2_Hot_Projects.bib"
;;  "/home/bolatu/main/library/bib/bolatu_rebiber_test/2_Hot_Projects.org")


(defun read-file-content
    (filepath)
  "Read the content of a file and return as a string."
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(defun extract-added-lines-from-diff
    (diff-content)
  "Parse given diff content and return lines that start with a plus sign as a list."
  (let
      ((lines
        (split-string diff-content "\n"))
       added-lines)
    (dolist
        (line lines added-lines)
      (when
          (string-prefix-p "+" line)
        (unless
            (string-prefix-p "+++" line)
          (push
           (substring line 1)
           added-lines))))
    (nreverse added-lines)))

;; (extract-added-lines-from-diff
;;  (read-file-content
;;   "/home/bolatu/main/library/bib/bolatu_rebiber_test/my.diff"))


(defun diff-to-org (difffile orgfile)
  "Extract bibtex from difffile and save the content as an orgfile."
  (let* ((added-lines (extract-added-lines-from-diff
                       (read-file-content difffile)))
         (bibtexstring "")
         (tempbibfile (expand-file-name "temp.bib" ub/bib-rebiber-dir))) ;;"/tmp/tempbib.bib"))
    ;;(message "diff-to-org yoo")
    ;; Concatenate all added-lines to create the bibtex content
    (dolist (line added-lines)
      (setq bibtexstring (concat bibtexstring line "\n")))
    ;; Write the bibtex content to a temporary .bib file
    (with-temp-file tempbibfile
      (insert bibtexstring))
    ;; Call the bibtex-to-org function to create the org file
    (bibtex-to-org tempbibfile orgfile)))


;; (diff-to-org "/home/bolatu/main/library/bib/bolatu_rebiber_test/my.diff" "/home/bolatu/main/library/bib/bolatu_rebiber_test/mydif.org")


(defun update-org-file-from-diff ()
  (let ((diff-file (expand-file-name "temp.diff" ub/bib-rebiber-dir))
        (org-file (expand-file-name "temp.org" ub/bib-rebiber-dir)))
    (diff-to-org diff-file org-file)
    ;;(ub/yequake-org-bib-async-capture nil nil)
    ))


(defun generate-org-file-from-bib-files-given-dir (dir)
  "Generate org files from the bibtex files in the given directory. Except temp.bib..."
  (let ((bib-files (directory-files dir t "\.bib$")))
    (dolist (bib-file bib-files)
      (unless (string= bib-file (expand-file-name "temp.bib" dir))
        (let ((org-file (concat (file-name-sans-extension bib-file) ".org")))
          (bibtex-to-org bib-file org-file))))))

(defun generate-org-file-from-bib-files-for-ub-bib-dir ()
  "Generate org files from the bibtex files in the library directory."
  (generate-org-file-from-bib-files-given-dir ub/bib-dir))

;;(generate-org-file-from-bib-files-for-ub-bib-dir)

(defun generate-org-file-from-bib-files-for-ub-bib-rebiber-dir ()
  "Generate org files from the bibtex files in the library directory."
  (generate-org-file-from-bib-files-given-dir ub/bib-rebiber-dir))

;;(generate-org-file-from-bib-files-for-ub-bib-rebiber-dir)

(defun setup-diff-file-monitor ()  
  ;; create a hook for temp.diff file change to produce temp_diff.org 
  (defvar my-file-change-debounce-timer-diff nil
    "Timer for debouncing temp.diff file change events.")

  (defvar diff-file-change-hook nil
    "Hook run after the temp.diff file is changed.")

  (defun diff-file-change-callback (event)
    "Callback for file change events."
    (message "File Notify `diff-change` Event: %S" event)
    (when (eq (nth 1 event) 'changed)
      (unless my-file-change-debounce-timer-diff
        (setq my-file-change-debounce-timer-diff
              (run-with-timer 3.0 nil ; Set the debounce duration (in seconds) as needed
                              (lambda ()
                                (run-hooks 'diff-file-change-hook)
                                (setq my-file-change-debounce-timer-diff nil)))))))

  (defvar diff-file-monitor-descriptor nil
    "Descriptor for monitoring the temp.diff file.")

  (let ((file-to-watch-diff
         (expand-file-name "temp.diff" ub/bib-rebiber-dir)))
    (setq diff-file-monitor-descriptor
          (file-notify-add-watch file-to-watch-diff
                                 '(change)
                                 'diff-file-change-callback)))

  (add-hook 'diff-file-change-hook 'update-org-file-from-diff)
  (add-hook 'diff-file-change-hook 'generate-org-file-from-bib-files-for-ub-bib-dir)
  (add-hook 'diff-file-change-hook 'generate-org-file-from-bib-files-for-ub-bib-rebiber-dir))


  (ub/run-if-server-active #'setup-diff-file-monitor)




(use-package! citar
  ;;:ensure (:fetcher github :repo "emacs-citar/citar")
  ;;:defer 0.1
  :after org
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-notes-paths '("~/main/org/roam/bib"))
  (citar-bibliography '(
                        "~/main/library/zotra/all.bib"
                        ;; "~/main/library/bib/bolatu/0_Reference_Materials.bib"
                        ;; "~/main/library/bib/bolatu/1_Papers_Archive.bib"
                        ;; "~/main/library/bib/bolatu/2_Hot_Projects.bib"
                        ;; "~/main/library/bib/bolatu/99_Paper_Dump.bib"
                        ))
  :config
  (setq citar-library-file-extensions
        '("pdf" "jpg" "png" "epub" "mp4" "mp3" "djvu" "txt"
          "doc" "docx" "ppt" "pptx" "xls" "xlsx" "odt" "odp" "ods")
        citar-file-additional-files-separator "_")
  ;; (defvar citar-indicator-files-icons
  ;;   (citar-indicator-create
  ;;    :symbol (nerd-icons-faicon
  ;;             "nf-fa-file_o"
  ;;             :face 'nerd-icons-green
  ;;             :v-adjust -0.1)
  ;;    :function #'citar-has-files
  ;;    :padding "  " ; need this because the default padding is too low for these icons
  ;;    :tag "has:files"))
  ;; (defvar citar-indicator-links-icons
  ;;   (citar-indicator-create
  ;;    :symbol (nerd-icons-faicon
  ;;             "nf-fa-link"
  ;;             :face 'nerd-icons-orange
  ;;             :v-adjust 0.01)
  ;;    :function #'citar-has-links
  ;;    :padding "  "
  ;;    :tag "has:links"))
  ;; (defvar citar-indicator-notes-icons
  ;;   (citar-indicator-create
  ;;    :symbol (nerd-icons-codicon
  ;;             "nf-cod-note"
  ;;             :face 'nerd-icons-blue
  ;;             :v-adjust -0.3)
  ;;    :function #'citar-has-notes
  ;;    :padding "    "
  ;;    :tag "has:notes"))
  ;; (defvar citar-indicator-cited-icons
  ;;   (citar-indicator-create
  ;;    :symbol (nerd-icons-faicon
  ;;             "nf-fa-circle_o"
  ;;             :face 'nerd-icon-green)
  ;;    :function #'citar-is-cited
  ;;    :padding "  "
  ;;    :tag "is:cited"))
  ;; (setq citar-indicators
  ;;       (list ;;citar-indicator-files ; plain text
  ;;        citar-indicator-files-icons
  ;;        citar-indicator-links-icons
  ;;        citar-indicator-notes-icons
  ;;        citar-indicator-cited-icons))
  (setq citar-org-roam-note-title-template "${title} :: ${author}")
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))


;; (use-package! citar-org-roam
;;   :after (citar org-roam)
;;   ;;:ensure (:fetcher github :repo "emacs-citar/citar-org-roam")
;;   )

;; NOTE below config was found heavy search as it seems to not load with :after in use-package
(with-eval-after-load 'citar
  (with-eval-after-load 'org-roam
    (require 'citar-org-roam)
    (setq citar-org-roam-capture-template-key "l")
    (citar-org-roam-mode 1)))


;;;###autoload
(defvar +zotra-capture-frame-parameters
  `((name . "zotra-capture")
    (width . 0.75)
    (height . 0.5)
    (alpha . 0.95)
    (top . 0.10)
    ;;(transient . t)
    ,@(when (featurep :system 'linux)
        `((window-system . ,(if (boundp 'pgtk-initialized) 'pgtk 'x))
          (display . ,(or (getenv "WAYLAND_DISPLAY")
                          (getenv "DISPLAY")
                          ":0"))))
    ,(if (featurep :system 'macos) '(menu-bar-lines . 1)))
  "TODO")

;;;###autoload
(defun +zotra-capture-frame-p ()
  "Checks if the current frame is a Zotra capture frame."
  (equal (frame-parameter nil 'name) "zotra-capture"))

;;;###autoload
(defun +zotra-capture-cleanup ()
  "Cleans up the Zotra capture frame after use."
  (when (+zotra-capture-frame-p)
    (delete-frame nil t)))


;;;###autoload
(defun generate-bibtex-key (entry)
  "Generate a BibTeX key for the given ENTRY.
The pattern used is `auth.lower + shorttitle + year`.
Inspired by Better BibTeX format.
see https://retorque.re/zotero-better-bibtex/citing/
ENTRY is assumed to be an alist of BibTeX fields."
  (let* (
         ;;(author-list (cdr (assoc "author" entry)))
         (raw-author (cdr (assoc "author" value)))
         (author-list (if raw-author (split-string raw-author " and ") nil))
         ;; only the first author is used for the file name
         (author (if (and author-list (> (length author-list) 1))
                     (car author-list)))
         (title (cdr (assoc "title" entry)))
         (year (cdr (assoc "year" entry)))
         (last-name (when author
                      (car (split-string author ","))))
         ;;(cdr (last (split-string author ",\\| and ")))))
         (short-title (when title
                        (let ((words (split-string (downcase title) "\\W+")))
                          (if (> (length words) 3)
                              (mapconcat 'capitalize (cl-subseq words 0 3) "")
                            (mapconcat 'capitalize words "")))))
         (key-new (format "%s%s%s"
                          (downcase (or last-name ""))
                          (or short-title "")
                          (or year ""))))
    (replace-regexp-in-string "\\s-+" "" key-new) ;; Remove any whitespace
    ;; TODO replace special characters with their ascii equivalent
    ;; for a now, we just remove them
    (replace-regexp-in-string "[^a-zA-Z0-9 ]" "" key-new)
    ))


;;;###autoload
(defun add-file-field-to-entries (bib-entries file-path)
  "Add a file field with FILE-PATH to each entry in BIB-ENTRIES hash table."
  (maphash (lambda (key entry)
             (let ((file-field (assoc "file" entry)))
               (if file-field
                   (setcdr file-field file-path)  ; Update the existing 'file' field
                 ;; If not exists, add 'file' to the entry.
                 (push (cons "file" file-path) entry)

                 ;; also add date related custom fields: 'created', 'modified'
                 (push (cons "cf-created" (format-time-string "%Y-%m-%d %H:%M:%S")) entry)
                 (push (cons "cf-modified" (format-time-string "%Y-%m-%d %H:%M:%S")) entry)

                 ;; In case the entry doesn't get altered in the hash, explicitly set it:
                 (puthash key entry bib-entries))))
           bib-entries)
  bib-entries)  ; Return the modified hash


;;;###autoload
(defun serialize-bibtex-entry-with-new-key (entry)
  "Serialize a single BibTeX entry from ENTRY alist into a string in reverse order of fields.
And, Generates new BibTeX key."
  (let* ((key (generate-bibtex-key entry))  ; Generate new key from entry fields
         (type (cdr (assoc "=type=" entry)))  ; Get the entry type
         (formatted-entry (format "@%s{%s,\n" type key)))  ; Start the BibTeX entry using new key
    ;; Debug new key
    (message "New key: %s" key)
    ;; Loop through each field of the entry in reverse
    (cl-loop for field in (reverse entry)
             unless (member (car field) '("=type=" "=key="))  ; Skip special fields
             do (setq formatted-entry
                      (concat formatted-entry
                              (format "  %s = {%s},\n" (car field) (cdr field)))))
    ;; Close the entry
    (concat formatted-entry "}\n")))  ; Return the completed entry string

;;;###autoload
(defun save-bibtex-entries-to-file (entries file-name)
  "Write all Bibtex entries from ENTRIES hash table to FILE-NAME."
  (with-temp-file file-name
    (maphash (lambda (key entry)
               ;;(insert (serialize-bibtex-entry key entry)))
               (insert (serialize-bibtex-entry-with-new-key entry)))
             entries)))


;;;###autoload
(defun +zotra-capture/open-frame (input)
  "Opens a Zotra capture in a dedicated frame. URL is the target to capture."
  (interactive "sURL: ")
  (require 'parsebib)
  ;;(message "Input: %s" input)
  ;; NOTE returned input is below
  ;; (:url https://arxiv.org/abs/1710.03599 :bibfile /home/bolatu/main/library/zotra/bib/dump.bib :format bibtex)
  (let* ((frame-title-format "")
         (frame (if (+zotra-capture-frame-p)
                    (selected-frame)
                  (make-frame +zotra-capture-frame-parameters))))
    (select-frame-set-input-focus frame)
    (with-selected-frame frame
      (let* ((download-dir "~/main/library/zotra/99_dump")
             (bibfile-all "~/main/library/zotra/all.bib")
             (url (plist-get input :url))
             (bibfile (plist-get input :bibfile))
             (format-zotra (plist-get input :format))

             ;; get the bibtex entry w/ zotra-get-entry
             (bibtex-entry-string (zotra-get-entry url format-zotra))

             ;; save the bibtex entry to a temp file w/ with-temp-file to temp.bib
             ;;(temp-bib-file (expand-file-name "temp.bib" ub/bib-rebiber-dir))
             (temp-bib-file (expand-file-name "~/main/library/tmp/zotra-temp.bib"))
             ;;(temp-bib-rebiber-file (expand-file-name "~/main/library/zotra/bib/temp_rebiber.bib"))

             )
        ;; check if the temp files exist, if so delete them
        (when (file-exists-p temp-bib-file)
          (delete-file temp-bib-file))
        ;; (when (file-exists-p temp-bib-rebiber-file)
        ;;   (delete-file temp-bib-rebiber-file))
        (with-temp-file temp-bib-file
          (insert bibtex-entry-string))

        ;; NOTE cancelling the rebiber part for now as it is async and it complicates the process
        ;; ;; rebiber the temp.bib file to get the updated bibtex entry and save to temp_rebiber.bib
        ;; (let ((cmnd (concat
        ;;              (format "cd %s && " "~/main/library/zotra/bib/") ;; TODO variable for directory
        ;;              (format "rebiber -i %s -o %s && " temp-bib-file temp-bib-rebiber-file)
        ;;              )))
        ;;   (async-shell-command cmnd))

        ;; parse the updated bibtex entry to get the author, year, and title with parsebib similar to bibtext-to-org
        (let* ((bib-data (parsebib-parse temp-bib-file :display t))
               ;;(bibtex-key (generate-bibtex-key bib-data))
               )
          ;;:fields '("title" "author" "year" "file"))))

          ;;(message "BibTeX key: %s" bibtex-key)

          (maphash (lambda (key value)
                     (let* (
                            (raw-author (cdr (assoc-string "author" value)))
                            (author-list (if raw-author (split-string raw-author " and ") nil))
                            ;; only the first author is used for the file name
                            (author (if (and author-list (> (length author-list) 1))
                                        (car author-list)))
                            (author-last-name (car (split-string author ",")))
                            (author-last-name-decaptialized (downcase author-last-name))
                            (title (cdr (assoc-string "title" value)))
                            (title-special-chars-removed-except-space (replace-regexp-in-string "[^a-zA-Z0-9 ]" "" title))
                            (title-with-underscores (replace-regexp-in-string " " "_" title-special-chars-removed-except-space))
                            (title-decaptialized (downcase title-with-underscores))
                            (year (cdr (assoc-string "year" value)))
                            (date (format-time-string "%y%m%d"))
                            ;; MAYBE add bibtex-key to pdf file name
                            (pdf-file-name (format "%s_%s_%s_%s.pdf" date author-last-name-decaptialized year title-decaptialized))
                            ;; TODO replace special characters with their ascii equivalent
                            ;; for a now, we just remove them
                            ;; replace special characters with "" except underscore
                            ;;(pdf-file-name (replace-regexp-in-string "[^a-zA-Z0-9_ ]" "" pdf-file-name))
                            (pdf-file-name-wo-special-chars (replace-regexp-in-string "[^a-zA-Z0-9_ .]" "" pdf-file-name))
                            ;;(pdf-file-path (expand-file-name pdf-file-name download-dir))
                            (pdf-file-path (expand-file-name pdf-file-name-wo-special-chars download-dir))
                            )

                       ;; save the pdf file to the download directory
                       (zotra-download-attachment url download-dir pdf-file-name-wo-special-chars nil)

                       (message "PDF file path: %s" pdf-file-path)

                       ;; save the file path to a variable
                       ;; add 'file' field to the bibtex entry with the pdf file path in temp.bib and temp_rebiber.bib
                       ;;(bib-data (add-file-field-to-entries bib-data pdf-file-path))
                       (let ((updated-bib-data (add-file-field-to-entries bib-data pdf-file-path)))

                         ;; debug updated-bib-data
                         ;; (maphash (lambda (key entry)
                         ;;            (message "Entry %s: %s" key entry))
                         ;;          updated-bib-data)

                         ;; delete previous temp.bib file which doesn't have the 'file' field
                         (delete-file temp-bib-file)

                         ;; save the updated bibtex entry to a temp file w/ with-temp-file to temp.bib
                         (save-bibtex-entries-to-file updated-bib-data temp-bib-file)
                         )

                       )
                     )
                   bib-data)
          )

        ;; prepend the updated string/text temp.bib to the original bib file and all.bib file
        ;; with emacs lisp
        (let ((temp-bib-string (with-temp-buffer
                                 (insert-file-contents temp-bib-file)
                                 (buffer-string)))
              (original-bib-string (with-temp-buffer
                                     (insert-file-contents bibfile)
                                     (buffer-string)))
              (original-bib-string-all (with-temp-buffer
                                         (insert-file-contents bibfile-all)
                                         (buffer-string))))
          (with-temp-file bibfile
            (insert temp-bib-string)
            (insert "\n")
            (insert original-bib-string))
          (with-temp-file bibfile-all
            (insert temp-bib-string)
            (insert "\n")
            (insert original-bib-string-all)))

        ;; open temp.bib file in a new buffer
        (find-file temp-bib-file)

        ))))


(use-package! zotra
  ;;:ensure t
  :config
  ;; Setup your backend
  (setq zotra-backend 'zotra-server)
  (setq zotra-local-server-directory "/home/bolatu/main/apps/zotra-server")
  (setq zotra-download-attachment-default-directory (expand-file-name "~/main/library/zotra/99_dump"))
  (setq zotra-default-bibliography (expand-file-name "~/main/library/zotra/99_dump/99_dump.bib"))


  ;; (defun my-zotra-capture (info)
  ;;   (let ((url (plist-get info :url)))

  ;;     (zotra-add-entry url)
  ;;     (zotra-download-attachment url)
  ;;     ))

  )

;; ;; remove the default org-protocol handler from org-protocol-protocol-alist
;; ;; name as zotra-protocol
;; ;; e.g., current org-protocol-protocol-alist is
;; ;; (("zotra-protocol" :protocol "zotra" :function zotra-protocol)
;; ;;  ("org-roam-node" :protocol "roam-node" :function org-roam-protocol-open-node)
;; ;;  ("org-roam-ref" :protocol "roam-ref" :function org-roam-protocol-open-ref))
;; (setq org-protocol-protocol-alist
;;       (cl-remove-if (lambda (x) (equal (plist-get x :protocol) "zotra"))
;;                     org-protocol-protocol-alist))

;; (add-to-list 'org-protocol-protocol-alist
;;              '("zotra-capture-frame"
;;                :protocol "zotra-capture-frame"
;;                :function +zotra-capture/open-frame))
(add-to-list 'org-protocol-protocol-alist
             '("zotra-protocol"
               :protocol "zotra"
               :function +zotra-capture/open-frame))

(message "bib.el load end")
