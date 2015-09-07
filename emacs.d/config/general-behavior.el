;; Hide splash screen
(setq inhibit-startup-message t)

;; No backup files
(setq make-backup-files nil)
(auto-save-mode nil)

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

;; Don't confirm when creating new file
(setq confirm-nonexistent-file-or-buffer nil)

(require 'savehist)
(savehist-mode t)

(provide 'general-behavior)
