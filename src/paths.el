(message "paths.el load start")

;; ref: http://turingmachine.org/bl/2013-05-29-recursively-listing-directories-in-elisp.html
(defun ub/directory-files-recursively (directory match maxdepth ignore)
  "List files in DIRECTORY and in its sub-directories.
   Return files that match the regular expression MATCH but ignore
   files and directories that match IGNORE (IGNORE is tested before MATCH. Recurse only
   to depth MAXDEPTH. If zero or negative, then do not recurse"
  (let* ((files-list '())
         (current-directory-list
          (directory-files directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (let ((f (car current-directory-list)))
        (cond
         ((and
           ;; make sure it is not nil
           ;; string match only with file not full path
           ignore (string-match ignore (file-name-nondirectory f))) nil)
         ((and
           (file-regular-p f)
           (file-readable-p f)
           (string-match match (file-name-nondirectory f)))
          (setq files-list (cons f files-list)))
         ((and
           (file-directory-p f)
           (file-readable-p f)
           (not (string-equal ".." (substring f -2)))
           (not (string-equal "." (substring f -1)))
           (> maxdepth 0))
          ;; recurse only if necessary
          (setq files-list (append files-list (ub/directory-files-recursively f match (- maxdepth -1) ignore))))
         (t)))
      (setq current-directory-list (cdr current-directory-list)))
    files-list))

(defun define-dir-path-const (name path)
  (unless (file-directory-p path)
    (message "Warning: Directory path does not exist: %s" path))
  (eval `(defconst ,name ,path "Defines a path constant.")))

(defun define-file-path-const (name path)
  (unless (file-exists-p path)
    (message "Warning: File path does not exist: %s" path))
  (eval `(defconst ,name ,path "Defines a file path constant.")))


(define-dir-path-const 'ub/org-root-dir (file-truename "~/main/org"))
(define-dir-path-const 'ub/gtd-root-dir (file-truename "~/main/org/gtd"))
(define-dir-path-const 'ub/roam-root-dir (file-truename "~/main/org/roam/"))



;; TODO rename doom-done-right
(define-dir-path-const 'ub/doom-emacs-root-dir (file-truename "~/doom-configs/doom-minimal-eglot/src"))


;; (setq ub/docs-dir (file-truename "~/main/org/docs"))
;; (setq ub/docs-files-list (ub/directory-files-recursively ub/docs-dir "\.org$" 0 ".sync-conflict"))


(define-dir-path-const 'ub/roam-bib-dir (file-truename "~/main/org/roam/bib"))
(define-dir-path-const 'ub/roam-blogs-dir (file-truename "~/main/org/roam/blog"))
(define-dir-path-const 'ub/roam-dailies-dir (file-truename "~/main/org/roam/daily"))

(define-dir-path-const 'ub/roam-dailies-archive-dir (file-truename "~/main/org/roam/daily/0_archive"))

(define-dir-path-const 'ub/roam-drafts-dir (file-truename "~/main/org/roam/draft"))
(define-dir-path-const 'ub/roam-dump-dir (file-truename "~/main/org/roam/dump"))
(define-dir-path-const 'ub/roam-index-dir (file-truename "~/main/org/roam/index"))

(define-dir-path-const 'ub/roam-index-research-dir (file-truename "~/main/org/roam/index_research"))
(define-dir-path-const 'ub/roam-study-dir (file-truename "~/main/org/roam/study"))
(define-dir-path-const 'ub/roam-tags-dir (file-truename "~/main/org/roam/tags"))
(define-dir-path-const 'ub/roam-tdb-dir (file-truename "~/main/org/roam/tdb"))
(define-dir-path-const 'ub/roam-vdb-dir (file-truename "~/main/org/roam/vdb"))





;; TODO here is we save new papers added to zotero through automation pipeline
(define-file-path-const 'ub/roam-paper-inbox-file (file-truename "~/main/org/roam/index/idx_paper_inbox.org"))



;; gtd files for org-agenda
(define-file-path-const 'ub/gtd-work-file (file-truename "~/main/org/gtd/work.org"))
(define-file-path-const 'ub/gtd-me-file (file-truename "~/main/org/gtd/me.org"))
(define-file-path-const 'ub/gtd-inbox-file (file-truename "~/main/org/gtd/inbox.org"))
(define-file-path-const 'ub/gtd-inbox-laptop-file (file-truename "~/main/org/gtd/inbox-laptop.org"))
(define-file-path-const 'ub/gtd-bookmark-file (file-truename "~/main/org/gtd/bookmark.org"))
(define-file-path-const 'ub/gtd-horizon-file (file-truename "~/main/org/gtd/horizon.org"))

(define-file-path-const 'ub/gtd-paper-file (file-truename "~/main/org/gtd/paper.org"))


(defconst ub/daily-today-file 
  (format-time-string "~/main/org/roam/daily/%Y-%m-%d.org")
  "Today's file path")

(defconst ub/daily-tomorrow-file 
  (let* ((curr-time (current-time))
         (decoded-time (decode-time curr-time))
         (tomorrow-time (encode-time (nth 0 decoded-time)
                                     (nth 1 decoded-time)
                                     (nth 2 decoded-time)
                                     (1+ (nth 3 decoded-time))
                                     (nth 4 decoded-time)
                                     (nth 5 decoded-time))))
    (format-time-string "~/main/org/roam/daily/%Y-%m-%d.org" tomorrow-time))
  "Tomorrow's file path")

(defconst ub/daily-yesterday-file 
  (let* ((curr-time (current-time))
         (decoded-time (decode-time curr-time))
         (yesterday-time (encode-time (nth 0 decoded-time)
                                      (nth 1 decoded-time)
                                      (nth 2 decoded-time)
                                      (1- (nth 3 decoded-time))
                                      (nth 4 decoded-time)
                                      (nth 5 decoded-time))))
    (format-time-string "~/main/org/roam/daily/%Y-%m-%d.org" yesterday-time))
  "Yesterday's file path")


;; "Set org-roam daily note considering the day ends at 7 am."
(let ((current-hour (string-to-number (format-time-string "%H"))))
  (when (< current-hour 7)
    (setq ub/daily-today-file ub/daily-yesterday-file)))


(define-dir-path-const 'ub/library-zotra-dir (file-truename "~/main/library/zotra"))
(setq ub/bib-file-all "all.bib")
(define-dir-path-const 'ub/bib-rebiber-dir (expand-file-name "~/main/library/rebiber"))


;; (define-dir-path-const 'ub/bib-dir (file-truename "~/main/library/bib/bolatu"))
(define-dir-path-const 'ub/bib-dir (file-truename "~/main/library/zotra"))

;; (define-dir-path-const 'ub/bib-rebiber-dir (expand-file-name "~/main/library/bib/bolatu_rebiber"))
;; (setq ub/bib-files-list  (ub/directory-files-recursively ub/bib-dir "\.bib$" 0 nil))
;; (setq ub/bib-file-ref "0_Reference_Materials.bib")
;; (setq ub/bib-file-archive "1_Papers_Archive.bib")
;; (setq ub/bib-file-hot "2_Hot_Projects.bib")
;; (setq ub/bib-file-dump "99_Paper_Dump.bib")

;; (setq ub/bib-rebiber-files-list  (ub/directory-files-recursively ub/bib-rebiber-dir "\.bib$" 0 nil))



;; TODO add
;;(setq ub/bib-file-downloads "downloads.bib")

(define-dir-path-const 'ub/org-media-tmp-image-dir (file-truename "~/main/org/tmp/org-media-img"))


(define-file-path-const 'ub/elfeed-file (file-truename "~/main/org/gtd/elfeed.org"))











(message "paths.el load end")
