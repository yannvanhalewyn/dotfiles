(require-package 'powerline)
(require-package 'zenburn-theme)
(require-package 'gruvbox-theme)
(require-package 'base16-theme)

;; LAYOUT/Load custom themes
(load-theme 'base16-default-dark t)	;; ColorTheme
(global-linum-mode t)			;; Show line numbers
(setq linum-format "%d ")		;; Format line numbers
(show-paren-mode 1)			;; Show matching paren
(setq c-basic-offset 4)			;; Set offset for c
(menu-bar-mode 0)			;; Hide menu bar
(tool-bar-mode -1)			;; Hide toolbar (GUI)

(if window-system
    (progn
      (scroll-bar-mode -1)))	;; Hide scrollbar (GUI)

     ;; Powerline


(powerline-default-theme)

(provide 'layout)
