(message "bib.el load start")




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
