;;; ../dotfiles/.doom.d/fixes.el -*- lexical-binding: t; -*-

(when (featurep! :checkers syntax +childframe)
  (defun yvh/flycheck-posframe-monitor-post-command ()
    (when (not (flycheck-posframe-check-position))
      (posframe-hide flycheck-posframe-buffer)))

  (defun yvh/fix-flycheck-posframe-not-hiding-immediately ()
    (cond (flycheck-posframe-mode
           (add-hook 'post-command-hook 'yvh/flycheck-posframe-monitor-post-command nil t))
          ((not flycheck-posframe-mode)
           (remove-hook 'post-command-hook 'flycheck-posframe-monitor-post-command t))))

  (add-hook! flycheck-posframe-mode #'yvh/fix-flycheck-posframe-not-hiding-immediately))

(provide 'fixes)
