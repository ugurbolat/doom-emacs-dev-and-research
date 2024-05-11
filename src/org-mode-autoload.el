;;; src/org-mode-autoload.el -*- lexical-binding: t; -*-

(message "org-mode-autoload.el load start")

;;;###autoload
(defun ub/org-open-at-point-other-frame ()
  "Jump to bookmark in another frame. See `bookmark-jump' for more."
  (interactive)
  (let ((org-link-frame-setup (acons 'file 'find-file-other-frame org-link-frame-setup)))
    (org-open-at-point)))

;;;###autoload
(defun ub/latex-dollar-to-equation-block (start end)
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let ((replace-limit (or end (point-max))))
      (while (search-forward-regexp "\\$\\$\\([^$]+\\)\\$\\$" replace-limit t)
        (replace-match "\\\\begin{equation}
\\1
\\\\end{equation}" nil nil)))))

;; TODO doesn't work all the time, need to fix
;;;###autoload
(defun ub/latex-square-bracket-to-equation-block (start end)
  "Replace LaTeX square bracket \[ equation\] equations with equation block."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let ((replace-limit (or end (point-max))))
      (while (search-forward-regexp "\\\\\\[\\([^]]+\\)\\\\\\]" replace-limit t)
        (replace-match "\\\\begin{equation}
\\1
\\\\end{equation}" nil nil)))))


;; remove conflict files from syncthing
(defun get-headings-from-file (filename)
  "Get headings from org file with filename."
  (with-temp-buffer
    (insert-file-contents filename)
    (let* ((tree (org-element-parse-buffer))
           (headings (org-element-map tree 'headline (lambda (hl) hl))))
      headings)))

(defun compare-files (file1 file2)
  "Compare the headings in file1 and file2, returning the headings
in file1 that don't show in file2."
  (let* ((headings1 (get-headings-from-file file1))
         (headings2 (get-headings-from-file file2))
         (titles2 (mapcar (lambda (h) (org-element-property :raw-value h)) headings2))
         diff-list)
    (dolist (h1 headings1 diff-list)
      (let ((title1 (org-element-property :raw-value h1)))
        (unless (member title1 titles2)
          (push h1 diff-list))))))


(defun compare-files-save-results (file1 file2 output-file)
  "Compare the headings in file1 and file2, save the differences to output-file.
Includes all sub-elements of the headings."
  (let ((diff-headings1 (compare-files file1 file2))
        (diff-headings2 (compare-files file2 file1)))
    (with-temp-buffer
      (insert (format "* Differences in files %s not in %s\n" file1 file2))
      (dolist (h diff-headings1)
        (insert (org-element-interpret-data h) "\n"))
      (insert (format "* Differences in files %s not in %s\n" file2 file1))
      (dolist (h diff-headings2)
        (insert (org-element-interpret-data h) "\n"))
      (write-file output-file))))

;; (compare-files-save-results "/home/bolatu/main/org/tmp/diff_a.org" "/home/bolatu/main/org/tmp/diff_b.org" "/home/bolatu/main/org/tmp/diff_fileA_fileB_diff.org")
;;(compare-files-save-results "/home/bolatu/main/org/gtd/inbox.org" "/home/bolatu/main/org/tmp/inbox_tp.org" "/home/bolatu/main/org/tmp/diff_inbox.org")
;; (compare-files-save-results "/home/bolatu/main/org/gtd/inbox.org" "/home/bolatu/main/org/tmp/inbox_backup_230911.org" "/home/bolatu/main/org/tmp/diff_inbox_230911.org")
;;(compare-files-save-results "/home/bolatu/main/tmp/gtd_gpixel/bookmarks.org" "/home/bolatu/main/tmp/gtd_thinkpad/bookmarks.org" "/home/bolatu/main/tmp/diff_bookmarks_231228.org")
;;(compare-files-save-results "/home/bolatu/main/tmp/gtd_gpixel/inbox.org" "/home/bolatu/main/tmp/gtd_thinkpad/inbox.org" "/home/bolatu/main/tmp/diff_inbox_231228.org")
;; (compare-files-save-results "/home/bolatu/main/tmp/gtd_gpixel/elfeed.org" "/home/bolatu/main/tmp/gtd_thinkpad/elfeed.org" "/home/bolatu/main/tmp/diff_elfeed_231228.org")
;; (compare-files-save-results "/home/bolatu/main/tmp/gtd_gpixel/me.org" "/home/bolatu/main/tmp/gtd_thinkpad/me.org" "/home/bolatu/main/tmp/diff_me_231228.org")
;;(compare-files-save-results "/home/bolatu/main/tmp/gtd_gpixel/work.org" "/home/bolatu/main/tmp/gtd_thinkpad/work.org" "/home/bolatu/main/tmp/diff_work_231228.org")


;;;###autoload
(defun ub/diff-export-org-files ()
  "Compare the headings in file1 and file2, save the differences to output-file."
  (interactive)
  ;; ask file paths file1 and file2 and output file path
  (let ((file1 (read-file-name "File 1: "))
        (file2 (read-file-name "File 2: "))
        (output-file (read-file-name "Output file: ")))
    (compare-files-save-results file1 file2 output-file)
    ;; open the saved output-file diff buffer
    (find-file output-file)))


;;;###autoload
(defun ub/org-ql-captures-all ()
  (interactive)
  (org-ql-search `(,ub/gtd-inbox-file)
    '()
    :sort '(date)
    ;; NOTE that we are using forked org-super-agenda for reverse sorting for super-groups
    :super-groups '((:auto-ts t))))
(map! "C-c u i C" #'ub/org-ql-captures-all)

;;;###autoload
(defun ub/org-ql-captures-14d ()
  (interactive)
  (org-ql-search `(,ub/gtd-inbox-file)
    '(ts :from -14 :to today)
    :sort '(date)
    :super-groups '((:auto-ts t))))
(map! "C-c u i c" #'ub/org-ql-captures-14d)


;;;###autoload
(defun ub/org-ql-captures-14d-tags ()
  (interactive)
  (org-ql-search `(,ub/gtd-inbox-file)
    '(ts :from -14 :to today)
    :sort '(date)
    :super-groups '((:auto-tags t))))
;;(map! "C-c u i c" #'ub/org-ql-captures-14d-tags)


;;;###autoload
(defun ub/org-ql-deadlines-all ()
  (interactive)
  (org-ql-search (append `(,ub/gtd-me-file ,ub/gtd-work-file ,ub/gtd-inbox-file)
                         ;;ub/gtd-me-projects-main-files-list
                         ;;ub/gtd-work-projects-main-files-list
                         )
    '(or (and (deadline auto) (not (done))))
    ;;(and (tags "soi") (ts-active)) ;; NOTE ts-active not providing date info at the heading similar to deadline (e.g., in 15d) so set deadlines for the important event

    :sort '(date)))
;;:super-groups '((:deadline t))
(map! "C-c u j d" #'ub/org-ql-deadlines-all)

;;;###autoload
(defun ub/org-ql-project-all-headings ()
  (interactive)
  (org-ql-search `(,ub/gtd-me-file ,ub/gtd-work-file)
    '(todo "PROJ" "J@IN" "J@HOLD" "J@DONE" "J@KILL")
    :title "All Projects Headings"
    :sort '(todo priority)
    :super-groups '((:auto-category t))))
(map! "C-c u j A" #'ub/org-ql-project-all-headings)


(message "org-mode-autoload.el load end")
