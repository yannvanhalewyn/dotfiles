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

(provide 'init-functions)
