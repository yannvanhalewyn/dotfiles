;; Hide splash screen
(setq inhibit-startup-message t)

;; No backup files
(setq make-backup-files nil)
(setq create-lockfiles nil)
(auto-save-mode nil)

;; Prevent # -*- coding: utf-8 -*-
(setq ruby-insert-encoding-magic-comment nil)

;; Spaces over tabs
(setq-default indent-tabs-mode nil)

;; Remember cursor position of files when opening
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; Always follow symlinks
(setq vc-follow-symlinks t)

;; Smooth scroll
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

;; Javascript indentation
(setq js-indent-level 2)

;; Don't confirm when creating new file
(setq confirm-nonexistent-file-or-buffer nil)

(require 'savehist)
(savehist-mode t)

;; Use helm-M-x
(global-set-key (kbd "M-x ") 'helm-M-x)

;; Deal with temp files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Get shell PATH
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Easier font scaling
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(provide 'general-behavior)
