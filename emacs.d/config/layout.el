(require 'ci-status)
(add-hook 'magit-status-mode-hook 'cis/update)

(use-package base16-theme
  :init
  (load-theme 'base16-ashes t))

(use-package smart-mode-line
  :config
  (sml/setup)
  (column-number-mode)
  (setq mode-line-position nil
        evil-mode-line-format nil
        mode-line-end-spaces '(:eval (cis/modeline-status))))

;; LAYOUT/Load custom themes
(global-linum-mode t)     ;; Show line numbers
(show-paren-mode 1)       ;; Show matching paren
(menu-bar-mode 0)         ;; Hide menu bar
(tool-bar-mode -1)        ;; Hide toolbar (GUI)

;; Fringe color
(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

(if window-system
    (progn
      (scroll-bar-mode -1))) ;; Hide scrollbar (GUI)

(provide 'layout)
