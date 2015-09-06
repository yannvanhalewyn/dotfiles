;; LAYOUT/Load custom themes
(load-theme 'zenburn t)
(global-linum-mode t)
(setq linum-format "%d ")
(show-paren-mode 1)
(setq c-basic-offset 4)
(menu-bar-mode 0)
(tool-bar-mode -1)
(if window-system
    (progn
      (scroll-bar-mode -1)))

;; Powerline
(require 'powerline)
(powerline-default-theme)

(provide 'layout)
