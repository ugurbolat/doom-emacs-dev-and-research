(message "autoload.el load start")


;;;###autoload
(defun ub/run-if-server-active (body-fun)
  "Execute BODY-FUN if an Emacs server is running."
  ;;(message "server is active")
  (when (bound-and-true-p server-process)
    (funcall body-fun)))
;;;###autoload
(defun ub/run-if-else-server-active (body-fun &optional else-fun)
  "Execute BODY-FUN if an Emacs server is running, else execute ELSE-FUN."
  (if (bound-and-true-p server-process)
      (funcall body-fun)
    (when else-fun (funcall else-fun))))

;;;###autoload
(defun ub/insert-commit-prefix-w-emoji ()
  (interactive)
  (let* ((string-list
          '(":tada: init: "
            ":construction: wip: "
            ":christmas-tree: christmas tree bill (torba yasa)"
            ":bookmark: tag: "
            ":sparkles: feat: "
            ":bug: fix: "
            ":books: docs: "
            ":lipstick: style: "
            ":hammer: refactor: "
            ":rotating_light: test: "
            ":smiling-imp: customize:"
            ":wrench: chore:"
            ":ok_hand: review: "
            ":card_index: meta: "
            ;;":bulb: source: "
            ":racehorse: perf: "
            ":white_check_mark: addTest: "
            ":heavy_check_mark: passTest: "
            ":zap: update: "
            ":art: fmt: "
            ":fire: remove: "
            ":truck: move: "
            ":green_heart: ci: "
            ":lock: sec: "
            ":arrow_up: upDep: "
            ":arrow_down: downDep: "
            ":shirt: lint: "
            ;;":alien: i18n: "
            ;;":pencil: txt: "
            ":ambulance: hotfix: "
            ":rocket: deploy: "
            ":apple: fixMac: "
            ":penguin: fixLinux: "
            ":checkered_flag: fixWin: "
            ":construction_worker: ciBuild: "
            ":chart_with_upwards_trend: analytics: "
            ":heavy_minus_sign: removeDep: "
            ":heavy_plus_sign: addDep: "
            ":whale: docker: "
            ;;":wrench: config: "
            ;;":package: pkgJson: "
            ":twisted_rightwards_arrows: merge: "
            ":hankey: badCode: "
            ":rewind: revert: "
            ":boom: breaking: "
            ;;":wheelchair: a11y: "
            ))
         (selected-string (completing-read "Select a string: " string-list)))
    (insert selected-string)))


;;;###autoload
(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (call-interactively 'fill-paragraph)))

;;;###autoload
(defun unfill-region (start end)
  "Replace newline chars in region from START to END by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

;;;###autoload
(defun unfill-toggle ()
  "Toggle filling/unfilling of the current region.
Operates on the current paragraph if no region is active."
  (interactive)
  (let (deactivate-mark
        (fill-column
         (if (eq last-command this-command)
             (progn (setq this-command nil)
                    most-positive-fixnum)
           fill-column)))
    (call-interactively 'fill-paragraph)))


(message "autoload.el load start")
