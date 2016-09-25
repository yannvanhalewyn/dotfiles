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

(defun shell-command-to-string-async (command callback)
  "Execute shell command COMMAND asynchronously in the
  background. Return the temporary output buffer which command is
  writing to during execution. When the command is finished, call
  CALLBACK with the resulting output as a string."
  (lexical-let ((output-buffer (generate-new-buffer " *tmp-async*"))
                (callback callback))
    (set-process-sentinel
     (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
             (funcall callback output-string)))
         (kill-buffer output-buffer))))
    output-buffer))

(provide 'util)
