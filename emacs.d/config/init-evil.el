(use-package evil :defer t)
(use-package evil-leader :defer t)
(use-package evil-surround :defer t)
(use-package evil-commentary :defer t)
(use-package evil-numbers :defer t)

;; Start evil mode
(require 'evil)
(evil-mode 1)
(evil-commentary-mode)

; Evil mode key bindings
; ======================

;; Window movement
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-i") 'describe-mode)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;; Evil comments
(define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-operator)

;; Helm file browsing
(define-key evil-normal-state-map (kbd "C-b") 'helm-buffers-list)
(define-key evil-normal-state-map (kbd "C-p") 'helm-for-files)

;; Incrementing numbers
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-s-x") 'evil-numbers/dec-at-pt)

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
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
    "h" 'magit-toggle-diff-refine-hunk)

;; Setup evil-mode with evil-leader
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

;; Leader keys
(evil-leader/set-key
  "B" 'ibuffer
  "b" 'ido-switch-buffer
  "F" 'ido-find-file
  "gs" 'magit-status
  "gd" 'magit-diff
  "q" 'kill-this-buffer
  "Q" 'delete-other-windows
  "r" 'recompile
  "e" 'eval-last-sexp
  "E" 'eval-buffer
  "f" 'helm-projectile
  "SPC" 'helm-M-x
  "c" 'projectile-ag
  "d" 'dired-current-dir
  "m" 'rename-file
  "o" 'ido-find-file
  "p" '(lambda () (interactive) (find-file "~/.emacs.d/config/init-plugins.el")))

(add-hook 'js-mode-hook '(lambda ()
                           (evil-leader/set-key
                             "a" 'mocha-run-all-specs
                             "t" 'mocha-run-current-file
                             "s" 'mocha-run-nearest-spec
                             "l" 'mocha-run-last-spec)))

(add-hook 'clojure-mode-hook '(lambda ()
                                (evil-leader/set-key
                                  "e" 'cider-eval-defun-at-point
                                  "E" 'cider-eval-buffer
                                  "t" 'cider-test-run-tests
                                  "k" 'cider-load-buffer
                                  "t" 'cider-test-run-test
                                  "a" 'cider-test-run-tests
                                  "l" 'cider-test-rerun-tests)))

(add-hook 'rspec-mode-hook '(lambda ()
                              (evil-leader/set-key
                                "t" 'rspec-verify
                                "a" 'rspec-verify-all
                                "s" 'rspec-verify-single
                                "l" 'rspec-rerun)))

(add-hook 'projectile-rails-mode-hook
          '(lambda ()
             (define-key evil-normal-state-map "gf" 'projectile-rails-goto-file-at-point)
             (define-key evil-normal-state-map "gm" 'projectile-rails-find-current-model)
             (define-key evil-normal-state-map "gv" 'projectile-rails-find-current-view)
             (define-key evil-normal-state-map "gc" 'projectile-rails-find-current-controller)
             (define-key evil-normal-state-map "gt" 'rspec-find-spec-or-target-other-window)))

;; Surround
(require 'evil-surround)
(global-evil-surround-mode 1)

(provide 'init-evil)
