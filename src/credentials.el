
(message "crendentials.el 🤐 load start")

(setq ub/key-openai-token (expand-file-name "key-openai.gpg" doom-user-dir))
(setq ub/key-anthropic-token (expand-file-name "key-anthropic.gpg" doom-user-dir))
(setq ub/key-perplexity-token (expand-file-name "key-perplexity.gpg" doom-user-dir))
(setq ub/key-openrouter-token (expand-file-name "key-openrouter.gpg" doom-user-dir))

;; (defun ub/load-encrypted-file (filename)
;;   "Load an encrypted file."
;;   ;; emacs add a newlinea to file on save, which I don't know why
;;   ;; so we check if newline exist in the end of string,
;;   ;; if so we remove it and return a string
;;   (with-temp-buffer
;;     (insert-file-contents filename)
;;     ;;(setq content (buffer-string))
;;     (string-trim (buffer-string))
;;     )
;;   ;; (if (string-suffix-p "\n" content)
;;   ;;     (substring content 0 -1))
;;   )
(defun ub/load-encrypted-file (filename)
  "Load an encrypted file and return its contents as a clean string."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents filename)
      (string-trim (decode-coding-string (buffer-string) 'utf-8-unix)))))

(defun ub/load-key-openai-token ()
  (interactive)
  (ub/load-encrypted-file ub/key-openai-token))

(defun ub/load-key-anthropic-token ()
  (interactive)
  (ub/load-encrypted-file ub/key-anthropic-token))
;; (defun ub/load-key-anthropic-token ()
;;   "Load Anthropic API key from encrypted file."
;;   (with-temp-buffer
;;     (insert-file-contents ub/key-anthropic-token)
;;     (string-trim (buffer-string))))


(defun ub/load-key-perplexity-token ()
  (interactive)
  (ub/load-encrypted-file ub/key-perplexity-token))

(defun ub/load-key-openrouter-token ()
  (interactive)
  (ub/load-encrypted-file ub/key-openrouter-token))

;; Instead, consider using gpg-agent which
;; does the same job in a safer way.  See Info node (epa) Caching
;; Passphrases for more information.
;; Note that this option has no effect if you use GnuPG 2.0.
;;(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(defun set-api-key ()
  (setq org-ai-openai-api-token (ub/load-key-openai-token))
  (setenv "ANTHROPIC_API_KEY" (ub/load-key-anthropic-token))
  )

(run-with-idle-timer 1 nil #'set-api-key)

(message "crendentials.el 🤐 load end")
