(defun dired-current-dir ()
  (interactive)
  (dired ""))

(require 'dired)

(define-key dired-mode-map (kbd "q") 'kill-this-buffer)
(setq-default dired-listing-switches "-alh")

(provide 'init-dired)
