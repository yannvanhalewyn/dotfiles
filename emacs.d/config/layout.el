(require 'ci-status)
(add-hook 'magit-status-mode-hook 'cis/update)

(set-face-attribute 'default nil :height 125)

(use-package base16-theme
  :init
  (load-theme 'base16-default-dark t))

(use-package smart-mode-line
  :config
  (sml/setup)
  (column-number-mode)
  (add-to-list 'sml/replacer-regexp-list '("components" ":C:") t)
  (add-to-list 'sml/replacer-regexp-list '("flux" ":F:") t)
  (add-to-list 'sml/replacer-regexp-list '("redux" ":R:") t)
  (add-to-list 'sml/replacer-regexp-list '("spec" ":T:") t)

  (add-to-list 'sml/replacer-regexp-list '("app" ":A:") t)
  (add-to-list 'sml/replacer-regexp-list '("assets/javascripts" ":J:") t)
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

;; Thou shalt not cross 80 columns in thy file
(defvar red-bg-face 'red-bg)
(defface red-bg '((t :background "red")) "")
(use-package column-marker
  :config
  (column-marker-create column-marker-4 red-bg-face)
  (add-hooks (lambda () (column-marker-4 80))
             '(coffee-mode-hook ruby-mode-hook javascript-mode-hook)))

(if window-system
    (progn (scroll-bar-mode -1))) ;; Hide scrollbar (GUI)

(provide 'layout)

;; Nice Themes
;; Zenburn
;; Smyx
;; base16-ashes
;; base16-atelier-cave
;; base16-atelier-heath
;; base16-atelier-plateau
;; base16-bespin
;; base16-chalk
;; base16-default-dark
;; base16-eighties
;; base16-harmonic16-dark
;; base16-hopscotch
;; base16-materia
;; base16-mocha
;; base16-monokai
;; base16-ocean
;; base16-oceanicnext
;; base16-solarized-light
;; base16-3024
;; Misterioso
