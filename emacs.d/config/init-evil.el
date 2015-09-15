(require-package 'evil)
(require-package 'evil-leader)
(require-package 'evil-nerd-commenter)
(require-package 'magit)
(require-package 'neotree)
(require-package 'evil-surround)
(require-package 'evil-commentary)
(require-package 'evil-numbers)

;; Start evil mode
(require 'evil)
(evil-mode 1)
(evil-commentary-mode)

; Evil mode key bindings
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-i") 'describe-mode)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-operator)
(define-key evil-normal-state-map (kbd "C-b") 'helm-buffers-list)
(define-key evil-normal-state-map (kbd "C-p") 'helm-ls-git-ls)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-s-x") 'evil-numbers/dec-at-pt)
(define-key evil-visual-state-map (kbd "RET") 'align-regexp)

;; Quitting everything
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Cursor colors
(setq evil-normal-state-cursor '("white" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("white" bar))
(setq evil-replace-state-cursor '("red" box))
(setq evil-operator-state-cursor '("red" hollow))

;; Add hjkl for magit and ibuffer (actually just j..)
(evil-set-initial-state 'magit-log-edit-mode 'emacs)
(evil-set-initial-state 'nav-mode 'emacs)
(evil-set-initial-state 'grep-mode 'emacs)
(evil-set-initial-state 'ibuffer-mode 'normal)

(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
(evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
    "h" 'magit-toggle-diff-refine-hunk)

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
(evil-leader/set-key
  "n" 'neotree-toggle
  "b" 'ibuffer
  "gs" 'magit-status
  "cc" 'comment-or-uncomment-region
  "q" 'kill-this-buffer
  "r" 'recompile
  "e" 'eval-current-buffer
  "f" 'ff-find-other-file
  "SPC" 'helm-M-x
  "c" 'call-ag-with
  "d" 'dired-project-dir
  "o" 'ido-find-file)

(add-hook 'js-mode-hook '(lambda ()
			   (evil-leader/set-key
			     "a" 'mocha-run-all-specs
			     "t" 'mocha-run-current-file
			     "s" 'mocha-run-nearest-spec
			     "l" 'mocha-run-last-spec)))

;; Surround
(require 'evil-surround)
(global-evil-surround-mode 1)

(provide 'init-evil)
