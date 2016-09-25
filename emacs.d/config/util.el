(defun edit-config ()
  "Goes to the main emacs config file (init-plugins.el)"
  (interactive)
  (find-file "~/.emacs.d/config/init-plugins.el"))

(defun add-hooks (hook modes)
  "Applies hook to all mode (hooks)"
  (dolist (mode modes)
    (add-hook mode hook)))

(defun build-keymap (&rest key-commands)
  "Builds a new sparse keymap containing given commands"
  (let ((new-map (make-sparse-keymap)))
    (while (not (cl-endp key-commands))
      (define-key new-map (kbd (pop key-commands)) (pop key-commands)))
    new-map))

(provide 'util)
