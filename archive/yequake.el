;;; src/yequake.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; begining of equake

;; TODO migrate to doom's +org-capture/open-frame, currently is too messy.

;; below add to hit to startup 0.0x ms
;; it is too messy atm so we leave to utilize use-package properly :p
;; maybe already


;; FIXME - use regular org capture after doom-emacs#5714 is resolved
;; FIXME - workaround for org capture
;; see https://github.com/hlissner/doom-emacs/issues/5714
;; copying the implementation into a new namespace so it can be referenced without being circular
(defun df/restart-mode-h nil "Restart `org-mode', but only once."
       (if doom-debug-p
           (progn
             (org-mode-restart))
         (let
             ((inhibit-message t)
              (save-silently t))
           (prog1
               (org-mode-restart)
             (message ""))))
       (setq org-agenda-new-buffers
             (delq
              (current-buffer)
              org-agenda-new-buffers))
       (remove-hook 'doom-switch-buffer-hook #'+org--restart-mode-h 'local)
       (run-hooks 'find-file-hook))

(defvar df/restart-mode-fn 'df/restart-mode-h)

(defalias '+org--restart-mode-h (lambda () (funcall df/restart-mode-fn)))

(defun df/org-capture (&optional goto keys)
  (interactive "P")
  (message "ignoring restart-mode")
  (setq df/restart-mode-fn #'ignore)
  (org-capture goto keys))

(defun df/restore-restart-mode ()
  (message "restoring restart-mode")
  (setq df/restart-mode-fn 'df/restart-mode-h))

(add-hook! 'org-capture-before-finalize-hook #'df/restore-restart-mode)


(use-package! yequake
  ;;:ensure (:fetcher github :repo "alphapapa/yequake")
  ;;:defer 0.1
  ;;:defer t
  :custom
  (yequake-frames
   '(("org-capture"
      (buffer-fns . (ub/yequake-org-capture))
      (buffer-fns . (org-capture))
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.95)
      (top . 0.10)
      (frame-parameters . (
                           (undecorated . t)
                           ;;(skip-taskbar . t)
                           (sticky . t))))


     ;; ("org-protocol-capture"
     ;;  (buffer-fns . (ub/yequake-org-protocol-capture))
     ;;  (width . 0.75)
     ;;  (height . 0.5)
     ;;  (alpha . 0.95)
     ;;  (top . 0.10)
     ;;  (frame-parameters . ((undecorated . t)
     ;;                       (skip-taskbar . t)
     ;;                       (sticky . t))))
     ;; ("org-protocol-capture-with-menu"
     ;;  (buffer-fns . (ub/yequake-org-protocol-capture))
     ;;  (width . 0.75)
     ;;  (height . 0.5)
     ;;  (alpha . 0.95)
     ;;  (top . 0.10)
     ;;  (frame-parameters . ((undecorated . t)
     ;;                       (skip-taskbar . t)
     ;;                       (sticky . t))))

     ("org-protocol-capture"
      (buffer-fns . (ub/yequake-org-protocol-capture))
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.95)
      (top . 0.10)
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (sticky . t)
                           )))


     ;; TODO improve eshell such as hook eshell exit and closing windows
     ;; also consider equake package since it designed for shell
     ;; and seems to has more functionality compared to yequake
     ;; DONE using equake instead of yequake for shell
     ;; ("eshell"
     ;;  (buffer-fns . (+eshell/here))
     ;;  (width . 0.75)
     ;;  (height . 0.5)
     ;;  (top . 0.10)
     ;;  (alpha . 0.95)
     ;;  (frame-parameters . ((undecorated . t)
     ;;                       (skip-taskbar . t)
     ;;                       (sticky . t))))

     )))




;; REF: original org-protocol-capture function
;; instead of passing a key for a specific template,
;; we prompt the capture menu for selecting desired template
(defun ub/org-protocol-capture-with-menu (info)
  "
Instead of going passing template key, show org-capture menu"
  (let* ((parts
          (pcase (org-protocol-parse-parameters info)
            ;; New style links are parsed as a plist.
            ((let `(,(pred keywordp) . ,_) info) info)
            ;; Old style links, with or without template key, are
            ;; parsed as a list of strings.
            (p
             (let ((k (if (= 1 (length (car p)))
                          '(:template :url :title :body
                            '(:url :title :body)))))
               (org-protocol-assign-parameters p k)))))
         (template (or (plist-get parts :template)
                       org-protocol-default-template-key))
         (url (and (plist-get parts :url)
                   (org-protocol-sanitize-uri (plist-get parts :url))))
         (type (and url
                    (string-match "^\\([a-z]+\\):" url)
                    (match-string 1 url)))
         (title (or (plist-get parts :title) ""))
         (region (or (plist-get parts :body) ""))
         (orglink
          (if (null url) title
            (org-link-make-string url (or (org-string-nw-p title) url))))
         ;; Avoid call to `org-store-link'.
         (org-capture-link-is-already-stored t))
    ;; Only store link if there's a URL to insert later on.
    (when url (push (list url title) org-stored-links))
    (org-link-store-props :type type
                          :link url
                          :description title
                          :annotation orglink
                          :initial region
                          :query parts)
    (raise-frame)
    ;; NOTE modified line
    (org-capture nil nil)
    (message "Item captured.")
    ;; Make sure we do not return a string, as `server-visit-files',
    ;; through `server-edit', would interpret it as a file name.
    nil))


;; NOTE ugly hack for fixing ugly hack [[id:93adfc46-653d-44ad-b4b2-626ac39916ef][org-capture-mode doesn't start properly after opening agenda]]
;; org-capture replaced w/ new function
(defun ub/org-protocol-capture (info)
  "Process an org-protocol://capture style url with INFO.

The sub-protocol used to reach this function is set in
`org-protocol-protocol-alist'.

This function detects an URL, title and optional text, separated
by `/'.  The location for a browser's bookmark looks like this:

  javascript:location.href = \\='org-protocol://capture?\\=' +
        new URLSearchParams({
              url: location.href,
              title: document.title,
              body: window.getSelection()})

or to keep compatibility with Org versions from 9.0 to 9.4:

  javascript:location.href = \\='org-protocol://capture?url=\\='+ \\
        encodeURIComponent(location.href) + \\='&title=\\=' + \\
        encodeURIComponent(document.title) + \\='&body=\\=' + \\
        encodeURIComponent(window.getSelection())

By default, it uses the character `org-protocol-default-template-key',
which should be associated with a template in `org-capture-templates'.
You may specify the template with a template= query parameter, like this:

  javascript:location.href = \\='org-protocol://capture?template=b\\='+ ...

Now template ?b will be used."
  (let* ((parts
	  (pcase (org-protocol-parse-parameters info)
	    ;; New style links are parsed as a plist.
	    ((let `(,(pred keywordp) . ,_) info) info)
	    ;; Old style links, with or without template key, are
	    ;; parsed as a list of strings.
	    (p
	     (let ((k (if (= 1 (length (car p)))
			  '(:template :url :title :body)
			'(:url :title :body))))
	       (org-protocol-assign-parameters p k)))))
	 (template (or (plist-get parts :template)
		       org-protocol-default-template-key))
	 (url (and (plist-get parts :url)
		   (org-protocol-sanitize-uri (plist-get parts :url))))
	 (type (and url
		    (string-match "^\\([a-z]+\\):" url)
		    (match-string 1 url)))
	 (title (or (plist-get parts :title) ""))
	 (region (or (plist-get parts :body) ""))
	 (orglink
	  (if (null url) title
	    (org-link-make-string url (or (org-string-nw-p title) url))))
	 ;; Avoid call to `org-store-link'.
	 (org-capture-link-is-already-stored t))
    ;; Only store link if there's a URL to insert later on.
    (when url (push (list url title) org-stored-links))
    (org-link-store-props :type type
			  :link url
			  :description title
			  :annotation orglink
			  :initial region
			  :query parts)
    (raise-frame)
    (org-capture nil template)
    (message "Item captured.")
    ;; Make sure we do not return a string, as `server-visit-files',
    ;; through `server-edit', would interpret it as a file name.
    nil))


;; REF: ~/.emacs.d/.local/straight/repos/yequake/yequake.el
(defun ub/yequake-org-capture (&optional goto keys)
  "Call `org-capture' in a Yequake frame.
Adds a function to `org-capture-after-finalize-hook' that closes
the recently toggled Yequake frame and removes itself from the
hook."
  (let* ((remove-hook-fn (lambda ()
                           (remove-hook 'org-capture-after-finalize-hook #'org-capture-goto-last-stored))))
    ;;(add-hook 'org-capture-after-finalize-hook remove-hook-fn)
    ;;(add-hook 'org-capture-after-finalize-hook #'yequake-retoggle)
    ;; MAYBE: Propose an `org-capture-switch-buffer-fn' variable that could be rebound here.

    ;; ;; NOTE ub
    ;; ;; instead of retoggling the frame, go to last stored captured item.
    ;; ;; this is done because it gives a possibilty to refile,
    ;; ;; after finalize hook for capture is runned
    ;; ;; even if I hit C-c C-w for refiling.
    ;; ;; so instead, leave the frame open for possible refiling actions
    ;; ;; or quit with extra key hit.
    ;; (add-hook 'org-capture-after-finalize-hook remove-hook-fn)
    ;; (add-hook 'org-capture-after-finalize-hook #'org-capture-goto-last-stored)

    ;; NOTE: We override `org-switch-to-buffer-other-window' because
    ;; it always uses `switch-to-buffer-other-window', and we want to
    ;; display the template menu and capture buffer in the existing
    ;; window rather than splitting the frame.
    (cl-letf* (((symbol-function
                 ;;#'org-switch-to-buffer-other-window) ;; this is deprecated
                 ;;#'switch-to-buffer-in-dedicated-window) ;; this keeps the other window open
                 #'pop-to-buffer) ;; works
                (symbol-function #'switch-to-buffer)))
      (condition-case nil
          (progn
            (yequake--toggle-frame
             "org-capture"
             '((buffer-fns nil)
               (width . 0.75)
               (height . 0.5)
               (top . 0.10)
               (alpha . 0.95)
               (frame-parameters
                ;;(undecorated . t)
                ;;(skip-taskbar . t)
                ;;(sticky . t)
                )))
            (df/org-capture nil nil))
        ;; Be sure to return the "CAPTURE-" buffer, which is the current
        ;; buffer at this point.
        ;;(current-buffer)

        ((error quit)
         ;; Capture aborted: remove the hook and hide the capture frame.
         ;;(remove-hook 'org-capture-after-finalize-hook #'yequake-retoggle)

         (message "Item captured.")
         ;; NOTE ub
         ;; (remove-hook 'org-capture-after-finalize-hook #'org-capture-goto-last-stored)
         (yequake-retoggle))))))


;;;;;;;;;;;;;;;;
;; REF: ~/.emacs.d/.local/straight/repos/yequake/yequake.el
(defun ub/yequake-org-bib-async-capture (&optional goto keys)
  "Call `org-capture' in a Yequake frame.
Adds a function to `org-capture-after-finalize-hook' that closes
the recently toggled Yequake frame and removes itself from the
hook."
  (let* ((remove-hook-fn (lambda ()
                           (remove-hook 'org-capture-after-finalize-hook #'org-capture-goto-last-stored))))
    ;;(add-hook 'org-capture-after-finalize-hook remove-hook-fn)
    ;;(add-hook 'org-capture-after-finalize-hook #'yequake-retoggle)
    ;; MAYBE: Propose an `org-capture-switch-buffer-fn' variable that could be rebound here.

    ;; ;; NOTE ub
    ;; ;; instead of retoggling the frame, go to last stored captured item.
    ;; ;; this is done because it gives a possibilty to refile,
    ;; ;; after finalize hook for capture is runned
    ;; ;; even if I hit C-c C-w for refiling.
    ;; ;; so instead, leave the frame open for possible refiling actions
    ;; ;; or quit with extra key hit.
    ;; (add-hook 'org-capture-after-finalize-hook remove-hook-fn)
    ;; (add-hook 'org-capture-after-finalize-hook #'org-capture-goto-last-stored)

    ;; NOTE: We override `org-switch-to-buffer-other-window' because
    ;; it always uses `switch-to-buffer-other-window', and we want to
    ;; display the template menu and capture buffer in the existing
    ;; window rather than splitting the frame.
    ;; (cl-letf* (((symbol-function #'org-switch-to-buffer-other-window)
    ;;             (symbol-function #'switch-to-buffer)))
    (cl-letf* (((symbol-function
                 ;;#'org-switch-to-buffer-other-window) ;; this is deprecated
                 ;;#'switch-to-buffer-in-dedicated-window) ;; this keeps the other window open
                 #'pop-to-buffer) ;; works
                (symbol-function #'switch-to-buffer)))
      (condition-case nil
          (progn
            (yequake--toggle-frame
             "org-capture"
             '((buffer-fns nil)
               (width . 0.75)
               (height . 0.5)
               (top . 0.10)
               (alpha . 0.95)
               (frame-parameters
                ;;(undecorated . t)
                ;;(skip-taskbar . t)
                ;;(sticky . t)
                )))
            ;; (df/org-capture nil nil)
            ;;
            (df/org-capture nil "BB")
            )
        ;; Be sure to return the "CAPTURE-" buffer, which is the current
        ;; buffer at this point.
        ;;(current-buffer)

        ((error quit)
         ;; Capture aborted: remove the hook and hide the capture frame.
         ;;(remove-hook 'org-capture-after-finalize-hook #'yequake-retoggle)

         (message "Item captured.")
         ;; NOTE ub
         ;;(remove-hook 'org-capture-after-finalize-hook #'org-capture-goto-last-stored)
         (yequake-retoggle))))))







;; combining with org-protocol
;; ref: https://www.reddit.com/r/emacs/comments/fjou3c/open_orgprotocol_in_a_standalone_frame/
(defun ub/yequake-org-protocol-capture (info)
  "Call `org-protocol-capture' in a Yequake frame.
Adds a function to `org-capture-after-finalize-hook' that closes
the recently toggled Yequake frame and removes itself from the
hook."
  (let* ((remove-hook-fn (lambda ()
                           (remove-hook 'org-capture-after-finalize-hook #'org-capture-goto-last-stored))))

    ;; (add-hook 'org-capture-after-finalize-hook remove-hook-fn)
    ;; (add-hook 'org-capture-after-finalize-hook #'org-capture-goto-last-stored)

    (require 'org-roam-protocol)

    ;; NOTE: We override `org-switch-to-buffer-other-window' because
    ;; it always uses `switch-to-buffer-other-window', and we want to
    ;; display the template menu and capture buffer in the existing
    ;; window rather than splitting the frame.
    ;; (cl-letf* (((symbol-function #'org-switch-to-buffer-other-window)
    ;;             (symbol-function #'switch-to-buffer)))
    (cl-letf* (((symbol-function
                 ;;#'org-switch-to-buffer-other-window) ;; this is deprecated
                 ;;#'switch-to-buffer-in-dedicated-window) ;; this keeps the other window open
                 #'pop-to-buffer) ;; works
                (symbol-function #'switch-to-buffer)))
      (condition-case nil
          (progn
            ;; Hacky solution for opening the drop-down window
            (yequake--toggle-frame
             "org-capture"
             '((buffer-fns nil)
               (width . 0.75)
               (height . 0.5)
               (top . 0.10)
               (alpha . 0.95)
               (frame-parameters
                ;;(undecorated . t)
                ;;(skip-taskbar . t)
                ;;(sticky . t)
                )))
            (cond ((string-match-p "org-protocol-with-menu:" info)
                   (ub/org-protocol-capture-with-menu
                    ;;(message info)
                    (org-protocol-parse-parameters (s-chop-prefix "org-protocol-with-menu://capture\?" info) t)
                    ))
                  ((string-match-p "org-protocol:" info)
                   (ub/org-protocol-capture (org-protocol-parse-parameters (s-chop-prefix "org-protocol://capture\?" info) t)))
                  ((string-match-p "org-roam-protocol:" info)
                   ;;(message info)
                   (org-roam-protocol-open-ref (org-protocol-parse-parameters (s-chop-prefix "org-roam-protocol://roam-ref\?" info) t))
                   )
                  (t (message "Unknown protocol")))
            ;; Be sure to return the "CAPTURE-" buffer, which is the current
            ;; buffer at this point.
            (current-buffer))
        ((error quit)
         ;; Capture aborted: remove the hook and hide the capture frame.
         ;; (remove-hook 'org-capture-after-finalize-hook #'org-capture-goto-last-stored)
         (yequake-retoggle))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;; end of yequake
