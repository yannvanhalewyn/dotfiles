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

(provide 'init-ido)
