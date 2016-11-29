(defun coffee-open-above ()
  (interactive)
  (evil-open-above 1)
  (coffee-indent-line))

(defun coffee-open-below ()
  (interactive)
  (evil-open-below 1)
  (coffee-indent-line))

(provide 'coffee-evil-extensions)
