;;; src/rebiber.el -*- lexical-binding: t; -*-

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
