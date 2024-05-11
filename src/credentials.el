
(message "crendentials.el 🤐 load start")

(setq ub/key-openai-token (expand-file-name "key-openai.gpg" doom-user-dir))

(defun ub/load-encrypted-file (filename)
  "Load an encrypted file."
  ;; emacs add a newlinea to file on save, which I don't know why
  ;; so we check if newline exist in the end of string,
  ;; if so we remove it and return a string
  (with-temp-buffer
    (insert-file-contents filename)
    (setq content (buffer-string)))
  (if (string-suffix-p "\n" content)
      (substring content 0 -1)))


(defun ub/load-key-openai-token ()
  "Load the OpenAI token."
  (interactive)
  (ub/load-encrypted-file ub/key-openai-token))


;; Instead, consider using gpg-agent which
;; does the same job in a safer way.  See Info node (epa) Caching
;; Passphrases for more information.
;; Note that this option has no effect if you use GnuPG 2.0.
;;(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(defun set-api-key ()
  (setq org-ai-openai-api-token (ub/load-key-openai-token)))

(run-with-idle-timer 1 nil #'set-api-key)

(message "crendentials.el 🤐 load end")
