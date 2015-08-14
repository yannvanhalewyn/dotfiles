;                          .         .
; 8 8888888888            ,8.       ,8.                   .8.           ,o888888o.       d888888o.
; 8 8888                 ,888.     ,888.                 .888.         8888     `88.   .`8888:' `88.
; 8 8888                .`8888.   .`8888.               :88888.     ,8 8888       `8.  8.`8888.   Y8
; 8 8888               ,8.`8888. ,8.`8888.             . `88888.    88 8888            `8.`8888.
; 8 888888888888      ,8'8.`8888,8^8.`8888.           .8. `88888.   88 8888             `8.`8888.
; 8 8888             ,8' `8.`8888' `8.`8888.         .8`8. `88888.  88 8888              `8.`8888.
; 8 8888            ,8'   `8.`88'   `8.`8888.       .8' `8. `88888. 88 8888               `8.`8888.
; 8 8888           ,8'     `8.`'     `8.`8888.     .8'   `8. `88888.`8 8888       .8' 8b   `8.`8888.
; 8 8888          ,8'       `8        `8.`8888.   .888888888. `88888.  8888     ,88'  `8b.  ;8.`8888
; 8 888888888888 ,8'         `         `8.`8888. .8'       `8. `88888.  `8888888P'     `Y8888P ,88P'
;
(add-to-list 'load-path (concat user-emacs-directory "config"))
(load "my-ibuffer")
(load "load-packages")

;; Setup evil-mode with evil-leader
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(require 'evil)
(evil-mode 1)

;; Hide splash screen
(setq inhibit-startup-message t)

;; No backup files
(setq make-backup-files nil)

;; Remember cursor position of files when opening
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

; ;; Evil mode key bindings
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "-") 'delete-other-windows)

;; NeoTree evil mappings
(add-hook 'neotree-mode-hook
  (lambda ()
    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "o") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "r") 'neotree-refresh)
    (define-key evil-normal-state-local-map (kbd "m") 'neotree-rename-node)
    (define-key evil-normal-state-local-map (kbd "n") 'neotree-create-node)
    (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)
    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;; Fuzzy file finder ignores/setup
(setq fiplr-ignored-globs '((directories (".git" ".svn" "*vim"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))
(global-set-key (kbd "<f5>") 'fiplr-reload-list)
(define-key evil-normal-state-map (kbd "C-p") 'fiplr-find-file)

;; LAYOUT/Load custom themes
(load-theme 'zenburn t)
(global-linum-mode t)
(setq linum-format "%d ")
(show-paren-mode 1)

;; Powerline
(require 'powerline)
(powerline-default-theme)

;; Smooth scroll
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

;; Leader keys
(evil-leader/set-key "n" 'neotree-toggle)
(evil-leader/set-key "b" 'ibuffer)
(evil-leader/set-key "gs" 'magit-status)
(evil-leader/set-key "cc" 'comment-or-uncomment-region)
(evil-leader/set-key "q" 'kill-this-buffer)
(evil-leader/set-key "SPC" 'execute-extended-command)

;; Delimiters
(require 'autopair)
(autopair-global-mode t)

;; Surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;; Magit something the plugin told me, have no idea
(setq magit-last-seen-setup-instructions "1.4.0")

;; Setup fly-check
(add-hook 'after-init-hook #'global-flycheck-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
