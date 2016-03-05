;; (defun dired-project-dir ()
;;   (interactive)
;;   (dired project-root-dir))

(require 'dired)

(define-key dired-mode-map (kbd "q") 'kill-this-buffer)

(provide 'init-dired)
