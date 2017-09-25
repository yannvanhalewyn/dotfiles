(defun fix-evil-underscore ()
  (interactive)
  (modify-syntax-entry ?_ "w"))

(use-package evil
  :init
  (add-hook #'after-change-major-mode-hook 'fix-evil-underscore)
  (setq evil-want-fine-undo t)
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

(use-package evil-cleverparens
  :defer t
  :diminish evil-cleverparens-mode
  :config
  ;; Evil CP overwrites "c" for change. This will re-enable "cs"
  ;; motion "change surrounding" of evil-surround
  (evil-cp--enable-surround-operators)
  :init
  ;; Don't use crazy bindings for {, [, } and ] from evil-cleverparens
  (setq evil-cleverparens-use-additional-movement-keys nil))

(use-package evil-numbers
  :config
  (keys :prefix "g"
        "a" 'evil-numbers/inc-at-pt
        "x" 'evil-numbers/dec-at-pt))

;; Add hjkl for magit and ibuffer
(evil-add-hjkl-bindings git-rebase-mode-map 'emacs)
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs)
(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs)
(evil-add-hjkl-bindings magit-diff-mode-map 'emacs)
(evil-add-hjkl-bindings package-menu-mode-map 'emacs)
(evil-add-hjkl-bindings ibuffer-mode-map 'emacs)

(provide 'init-evil)
