(defvar wpt:positions ()
  "The stack of waypoints managed by wpt")

(defun wpt:push ()
  "Push the current position to the front of the queue"
  (interactive)
  (set-variable 'wpt:positions
		(cons (list (buffer-name) (point))
		      wpt:positions)))

(defun wpt:pop ()
  "Pop a position from the stack and jump to it."
  (interactive)
  (unless wpt:positions
    (error "No waypoints to pop!"))
  (let* ((address (car wpt:positions))
	 (buff (car address))
	 (pt (cadr address))
	 (rest (cdr wpt:positions)))
    (set-variable 'wpt:positions rest)
    (pop-to-buffer buff)
    (goto-char pt)))

(provide 'wpt)
