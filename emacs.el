;; set up packages repos and search paths
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
(require 'package)
(package-initialize)
(add-to-list 'load-path (concat user-emacs-directory "config"))
(require 'my-ibuffer)

;; Setup evil-mode with evil-leader
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(require 'evil)
(evil-mode 1)


;; Evil mode key bindings
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "M-F") 'dired)
(define-key evil-normal-state-map (kbd "M-b") 'ibuffer)

;; Dired mode mappings
(add-hook 'dired-mode-hook (lambda ()
    (define-key dired-mode-map "h" 'dired-up-directory)
    (define-key dired-mode-map "l" 'dired-find-alternate-file)
    (define-key dired-mode-map "c" 'dired-create-directory)
    (define-key dired-mode-map "/" 'dired-isearch-filenames)
))

;; Fuzzy file finder ignores/setup
(setq fiplr-ignored-globs '((directories (".git" ".svn" "*vim"))
			    (files ("*.jpg" "*.png" "*.zip" "*~"))))
(global-set-key (kbd "<f5>") 'fiplr-reload-list)
(define-key evil-normal-state-map (kbd "C-p") 'fiplr-find-file)

;; LAYOUT/Load custom themes
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
(load-theme 'gruvbox t)
(global-linum-mode t)
(setq linum-format "%d ")
;; Powerline
(require 'powerline)
(powerline-default-theme)
(set-face-attribute 'mode-line nil
		    :foreground "Black"
		    :background "DarkOrange"
		    :box nil)
(setq powerline-defaul-separator 'arrow)

;; Leader keys
(evil-leader/set-key "n" 'neotree-toggle)
(evil-leader/set-key "b" 'ibuffer)
(evil-leader/set-key "w" 'other-window)
(evil-leader/set-key "gs" 'magit-status)

;; Delimiters
(require 'autopair)
(autopair-global-mode t)
