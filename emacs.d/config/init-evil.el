(use-package evil
  :config
  (evil-mode t)
  (keys :states 'motion
        "[e" 'flycheck-previous-error
        "]e" 'flycheck-next-error
        "[b" 'previous-code-buffer
        "]b" 'next-code-buffer
        "]t" 'cycle-theme)
  (keys "C-h" 'evil-window-left
        "C-j" 'evil-window-down
        "C-k" 'evil-window-up
        "C-l" 'evil-window-right))

(use-package evil-commentary
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode)
  (keys :states 'normal "gc" 'evilnc-comment-operator))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-numbers
  :defer t
  :config
  (keys "C-a" 'evil-numbers/inc-at-pt
        "C-s-x" 'evil-numbers/dec-at-pt))

;; Add hjkl for magit and ibuffer
(evil-set-initial-state 'ibuffer-mode 'normal)
(evil-add-hjkl-bindings package-menu-mode-map 'emacs)
(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs)
(evil-add-hjkl-bindings git-rebase-mode-map 'emacs)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs)

(provide 'init-evil)
