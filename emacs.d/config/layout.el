(use-package smyx-theme)
(use-package powerline)
;; (use-package zenburn-theme)
;; (use-package gruvbox-theme)
;; (use-package base16-theme)

(use-package haml-mode :defer t)
(use-package sass-mode :defer t)
(use-package coffee-mode :defer t)

;; LAYOUT/Load custom themes
;; (load-theme 'smyx t)      ;; ColorTheme
(global-linum-mode t)     ;; Show line numbers
;; (setq linum-format "%d") ;; Format line numbers
(show-paren-mode 1)       ;; Show matching paren
(setq c-basic-offset 4)   ;; Set offset for c
(menu-bar-mode 0)         ;; Hide menu bar
(tool-bar-mode -1)        ;; Hide toolbar (GUI)

;; Fringe color
(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

(if window-system
    (progn
      (scroll-bar-mode -1))) ;; Hide scrollbar (GUI)

;; Show trailing whitespace and tabs
(global-whitespace-mode)
(setq whitespace-line nil)
(setq whitespace-line-column 85)

;; Powerline
(powerline-default-theme)

(provide 'layout)
