(defun code-buffer? (name)
  "Returns wether or the NAME is a name for a code buffer"
  (not (string-match-p "^\*" name)))

(defun circle-code-buffers (circle-fn)
  (let ((bread-crumb (buffer-name)))
    (funcall circle-fn)
    (while
        (and
         (not (code-buffer? (buffer-name)))
         (not (equal bread-crumb (buffer-name))) )
      (funcall circle-fn))))

(defun next-code-buffer ()
  "Open next active buffer, ignoring non-code related buffers."
  (interactive)
  (circle-code-buffers 'next-buffer))

(defun previous-code-buffer ()
  "Open next active buffer, ignoring non-code related buffers."
  (interactive)
  (circle-code-buffers 'previous-buffer))

;; Save buffer when exiting insert mode
(defun save-if-code-buffer ()
  "Saves the current buffer if code buffer"
  (if (code-buffer? (buffer-name))
      (save-buffer)))

;; Clear emacs shell
(defun shell-clear-buffer ()
  (interactive)
  ;; Size one because of my 2-line prompt
  (let ((comint-buffer-maximum-size 1))
    (comint-truncate-buffer)))

(defun cycle-theme (&optional reverse)
  "Load the next (or previous if REVERSE is true) available theme."
  (interactive)
  (let* ((current-theme (car custom-enabled-themes))
         (all-themes (if reverse
                         (reverse (custom-available-themes))
                       (custom-available-themes)))
         (first-theme (car all-themes))
         (go (lambda (theme)
               (message "Loading %s." (symbol-name theme))
               (disable-theme current-theme)
               (load-theme theme)))
         theme)
    (if (catch 'done
          (while (setq theme (pop all-themes))
            (if (and (eq theme current-theme)
                     (setq theme (pop all-themes)))
                (progn
                  (funcall go theme)
                  (throw 'done nil))))
          t)
        (funcall go first-theme))))

(provide 'init-functions)
