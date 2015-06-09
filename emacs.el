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

;; set up packages repos and search paths
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(require 'package)
(package-initialize)
(add-to-list 'load-path (concat user-emacs-directory "config"))
(require 'my-ibuffer)

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

;; Evil mode key bindings
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "M-F") 'dired)
(define-key evil-normal-state-map (kbd "M-b") 'ibuffer)
(define-key evil-normal-state-map (kbd "-") 'delete-other-windows)

;; Dired mode mappings
(add-hook 'dired-mode-hook (lambda ()
    (define-key dired-mode-map "h" 'dired-up-directory)
    (define-key dired-mode-map "l" 'dired-find-alternate-file)
    (define-key dired-mode-map "c" 'dired-create-directory)
    (define-key dired-mode-map "/" 'dired-isearch-filenames)
))

(global-set-key (kbd "M-q") 'evil-quit-all)

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
(setq powerline-defaul-separator 'arrow) ;; Not working
;; Smooth scroll
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

;; Leader keys
(evil-leader/set-key "n" 'neotree-toggle)
(evil-leader/set-key "b" 'ibuffer)
(evil-leader/set-key "w" 'other-window)
(evil-leader/set-key "gs" 'magit-status)
(evil-leader/set-key "cc" 'comment-or-uncomment-region)
(evil-leader/set-key "q" 'kill-this-buffer)
(evil-leader/set-key "pp" 'list-packages)
(evil-leader/set-key "j" 'ace-jump-line-mode)
(evil-leader/set-key "h" 'ace-jump-word-mode)

;; Delimiters
(require 'autopair)
(autopair-global-mode t)

;; Surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;; Magit something the plugin told me, have no idea
(setq magit-last-seen-setup-instructions "1.4.0")

;; Escape out of everything
(defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
	(setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)
