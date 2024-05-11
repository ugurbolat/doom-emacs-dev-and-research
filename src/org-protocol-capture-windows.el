;;; src/org-protocol-capture-windows.el -*- lexical-binding: t; -*-

;; customized org-capture frame parameters in
;; /home/bolatu/emacs-configs/doom-done-right/modules/lang/org/autoload/org-capture.el
(setq +org-capture-frame-parameters
      `((name . "doom-capture")
        ;; (width . 70)
        ;; (height . 25)
        (width . 0.6)
        (height . 0.4)
        (alpha . 0.95)
        (top . 0.10)
        (left . 0.10)
        (transient . t)
        ,@(when (featurep :system 'linux)
            `((window-system . ,(if (boundp 'pgtk-initialized) 'pgtk 'x))
              (display . ,(or (getenv "WAYLAND_DISPLAY")
                              (getenv "DISPLAY")
                              ":0"))))
        ,(if (featurep :system 'macos) '(menu-bar-lines . 1))))


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


;;;###autoload
(defvar +org-capture-protocol-with-separate-window-frame-parameters
  `((name . "org-capture-protocol-with-separate-window")
    (width . 0.6)
    (height . 0.4)
    (alpha . 0.95)
    (top . 0.10)
    (left . 0.10)
    ;;(transient . t)
    ,@(when (featurep :system 'linux)
        `((window-system . ,(if (boundp 'pgtk-initialized) 'pgtk 'x))
          (display . ,(or (getenv "WAYLAND_DISPLAY")
                          (getenv "DISPLAY")
                          ":0"))))
    ,(if (featurep :system 'macos) '(menu-bar-lines . 1)))
  "TODO")

;;;###autoload
(defun +org-capture-protocol-with-separate-window-frame-p ()
  "Checks if the current frame is a Zotra capture frame."
  (equal (frame-parameter nil 'name) "org-capture-protocol-with-separate-window"))

;;;###autoload
(defun +org-capture-protocol-with-separate-window-cleanup ()
  "Cleans up the Zotra capture frame after use."
  (when (+zotra-capture-frame-p)
    (delete-frame nil t)))

;;;###autoload
;;(defun +org-capture-protocol-with-separate-window/open-frame (&optional initial-input key)
(defun +org-capture-protocol-with-separate-window/open-frame (info)
  "Opens the org-capture-protocol-with-separate-window window in a floating frame that cleans itself up once
you're done. This can be called from an external shell script.

Instead of passing a key for a specific template,
we prompt the capture menu for selecting desired template,
but we store the link captured by org-protocol-capture.

1. parse parameters from info which contains the protocol string to be parsed
2. we store the link captured by org-protocol-capture which is parsed from info
3. when we call org-capture (or org-roam-capture), we will prompt the capture menu for selecting desired template
4. after selecting the template, we will call org-capture and store the link captured by org-protocol-capture

"
  (interactive)

  (let* ((frame-title-format "")
         (frame (if (+org-capture-frame-p)
                    (selected-frame)
                  (make-frame +org-capture-protocol-with-separate-window-frame-parameters))))
    (select-frame-set-input-focus frame)  ; fix MacOS not focusing new frames

    (with-selected-frame frame
      (require 'org-capture)
      (require 'org-roam-protocol)
      (condition-case ex
          (letf! ((#'pop-to-buffer #'switch-to-buffer))
            (switch-to-buffer (doom-fallback-buffer))

            (cond ((string-match-p "org-protocol-with-menu:" info)
                   (message "org-protocol-with-menu: %s" info)
                   (ub/org-protocol-capture-with-menu
                    (org-protocol-parse-parameters
                     (s-chop-prefix "org-protocol-with-menu://capture\?" info) t)))
                  ;;((string-match-p "org-roam-protocol:" info)
                  ((string-match-p "org-roam-protocol://roam-ref?" info)
                   ;; replace org-roam-protocol://roam-ref with org-roam-protocol so that we can use org-roam-protocol-open-ref
                   ;;(replace-regexp-in-string "org-roam-protocol://roam-ref" "org-roam-protocol" info)
                   (message "org-roam-protocol: %s" info)
                   ;; NOTE url needs to contain template keys definedy by org-roam-capture-ref-templates
                   ;; currently i for dump and YY for youtube
                   (org-roam-protocol-open-ref
                    (org-protocol-parse-parameters
                     (s-chop-prefix "org-roam-protocol://roam-ref\?" info) t)))
                  (t (message "Unknown protocol")))
            )
        ('error
         (message "org-capture: %s" (error-message-string ex))
         (delete-frame frame)))))
  )


;; Dummy data for testing
;; (+org-capture-protocol-with-separate-window/open-frame
;;  "org-protocol-with-menu://capture?template=Z&url=https://www.google.com&title=Google")

;; (+org-capture-protocol-with-separate-window/open-frame
;;  "org-protocol://roam-ref?template=i&ref=https%3A%2F%2Fwww.wetteronline.de%2Fwetter%2Fbremen&title=Wetter%20Bremen%20-%20aktuelle%20Wettervorhersage%20von%20WetterOnline&body=")
