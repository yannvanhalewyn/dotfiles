;; Start evil mode
(require 'evil)
(evil-mode 1)

;;; Evil mode key bindings
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-i") 'describe-mode)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-operator)
(define-key evil-normal-state-map (kbd "-") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "C-p") 'fiplr-find-file)

;; Add hjkl for magit and ibuffer (actually just j..)
(evil-add-hjkl-bindings magit-status-mode-map)
(evil-set-initial-state 'magit-log-edit-mode 'emacs)
(evil-set-initial-state 'nav-mode 'emacs)
(evil-set-initial-state 'grep-mode 'emacs)
(evil-set-initial-state 'ibuffer-mode 'normal)

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

;; Setup evil-mode with evil-leader
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

;; Leader keys
(evil-leader/set-key "n" 'neotree-toggle)
(evil-leader/set-key "b" 'ibuffer)
(evil-leader/set-key "gs" 'magit-status)
(evil-leader/set-key "cc" 'comment-or-uncomment-region)
(evil-leader/set-key "q" 'kill-this-buffer)
(evil-leader/set-key "r" 'recompile)
(evil-leader/set-key "f" 'ff-find-other-file)
(evil-leader/set-key "SPC" 'execute-extended-command)

;; Surround
(require 'evil-surround)
(global-evil-surround-mode 1)

(provide 'init-evil)
