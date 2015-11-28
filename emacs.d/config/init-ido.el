(require 'ido)
(ido-mode 'both)

(setq
 ido-save-directory-list-file (concat user-emacs-directory "cache/ido.last")
 ido-case-fold t			;; Case insensitive
 ido-enable-last-directory-history t	;; Remember last directory chosen
 ido-max-work-directory-list 30		;; Should be ok
 ido-max-work-file-list 50		;; Should be ok
 ido-use-filename-at-point nil		;; Don't try to match filename under cursor
 ido-enable-flex-matching t		;; Enable fuzzy completion
 ido-max-prospects 8)			;; Don't spam the minibuffer

;; Prevent emacs from auto-changing default dir
(defun find-file-wo-cd ()
  (interactive)
  (let ((saved-default-directory default-directory)
	(default-directory project-root-dir))
    (ido-find-file)
    (message "Find file wo CD")
    (message (concat "resetting dir: " saved-default-directory))
    (setq default-directory saved-default-directory)))
(global-set-key (kbd "C-x C-f") 'find-file-wo-cd)

;; Use ido for M-x
(global-set-key
 (kbd "M-x") 
 (lambda ()
   (interactive)
   (call-interactively
    (intern (ido-completing-read "M-x " (all-completions "" obarray 'commandp))))))

(provide 'init-ido)
