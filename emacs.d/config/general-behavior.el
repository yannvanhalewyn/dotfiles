;; Hide splash screen
(setq inhibit-startup-message t)

;; No backup files
(setq make-backup-files nil)

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

(require 'savehist)
(savehist-mode t)

(provide 'general-behavior)
