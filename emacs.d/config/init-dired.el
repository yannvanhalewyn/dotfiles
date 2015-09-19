(defun dired-project-dir ()
  (interactive)
  (dired project-root-dir))

(require 'dired)

(define-key dired-mode-map (kbd "q") 'image-dired-kill-buffer-and-window)

(provide 'init-dired)
