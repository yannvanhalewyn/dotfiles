(use-package evil :defer t)
(use-package evil-surround :defer t)
(use-package evil-commentary :defer t :diminish evil-commentary-mode)
(use-package evil-numbers :defer t)

;; Start evil mode
(require 'evil)
(evil-mode 1)
(evil-commentary-mode)

;; Evil mode key bindings
;; ======================
(defun nnoremap (key action) (define-key evil-normal-state-map (kbd key) action))
(defun inoremap (key action) (define-key evil-insert-state-map (kbd key) action))

;; Window movement
(nnoremap "C-h" 'evil-window-left)
(nnoremap "C-i" 'describe-mode)
(nnoremap "C-j" 'evil-window-down)
(nnoremap "C-k" 'evil-window-up)
(nnoremap "C-l" 'evil-window-right)

;; Evil comments
(nnoremap "gc" 'evilnc-comment-operator)

;; Helm file browsing
(nnoremap "gs" 'projectile-switch-project)

;; Unimpaired
(nnoremap "[e" 'flycheck-previous-error)
(nnoremap "]e" 'flycheck-next-error)
(nnoremap "[b" 'previous-buffer)
(nnoremap "]b" 'next-buffer)

;; Incrementing numbers
(nnoremap "C-a" 'evil-numbers/inc-at-pt)
(nnoremap "C-s-x" 'evil-numbers/dec-at-pt)

;; Alignment
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
(evil-add-hjkl-bindings package-menu-mode-map 'emacs)
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk)

;; Surround
(require 'evil-surround)
(global-evil-surround-mode 1)

(provide 'init-evil)
