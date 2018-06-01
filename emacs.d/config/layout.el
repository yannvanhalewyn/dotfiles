(require 'ci-status)
(add-hook 'magit-status-mode-hook 'cis/update)

(set-face-attribute 'default nil :height 145)

;; (use-package jbeans-theme :init (load-theme 'jbeans t))
;; (use-package spacemacs-theme :init :init (load-theme 'spacemacs-dark t))
;; (use-package base16-theme :init (load-theme 'base16-ashes t))
;; (use-package smyx-theme :init (load-theme 'smyx t))
(use-package monokai-theme :init (load-theme 'monokai t))

(use-package all-the-icons)
(require 'modeline)

;; LAYOUT/Load custom themes
(fringe-mode 0)
(global-hl-line-mode t)    ;; Highlight current line
(global-display-line-numbers-mode)
(show-paren-mode 1)        ;; Show matching paren
(menu-bar-mode 0)          ;; Hide menu bar
(tool-bar-mode -1)         ;; Hide toolbar (GUI)
(setq-default tab-width 2) ;; When tabs are in a file, display them as 2 columns io 8

(if window-system
    (progn (scroll-bar-mode -1))) ;; Hide scrollbar (GUI)

(provide 'layout)

;; Nice Themes
;; Zenburn
;; Smyx
;; Jbeans
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
