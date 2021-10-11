(package-initialize)

(defun load-config()
  (interactive)
  (org-babel-load-file (locate-user-emacs-file "configuration.org")))

(load-config)
