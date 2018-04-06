;; Hide splash screen
(setq inhibit-startup-message t)

;; No backup files
(setq make-backup-files nil)
(setq create-lockfiles nil)
(auto-save-mode nil)

;; Just type y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Automaticaly add newline at and of document
(setq require-final-newline t)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Fix tabs and clear trailing-whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Prevent # -*- coding: utf-8 -*-
(setq ruby-insert-encoding-magic-comment nil)

;; Spaces over tabs
(setq-default indent-tabs-mode nil)

;; Silence bell
(setq ring-bell-function 'ignore)

;; Remember cursor position of files when opening
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; Always follow symlinks
(setq vc-follow-symlinks t)

;; Scratch buffer in eLisp
(setq initial-major-mode 'emacs-lisp-mode)

;; Centering after { and }
(setq scroll-margin 3
      scroll-conservatively 9999
      scroll-step 1)

;; Indentation
(setq js-indent-level 2
      css-indent-offset 2
      tab-width 2
      c-basic-offset 4)

;; Don't confirm when creating new file
(setq confirm-nonexistent-file-or-buffer nil)

;; Scroll in compilation mode
(setq compilation-scroll-output t)


(require 'savehist)
(savehist-mode t)

;; Deal with temp files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Get shell PATH
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

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

;; C++ header files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Easier font scaling
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Open general config files with conf mode
(let* ((conf-files '("aliases" "functions" "gitignore" "zshrc"))
       (conf-regexp (concat (regexp-opt conf-files t) "\\'")))
  (add-to-list 'auto-mode-alist (cons conf-regexp 'conf-mode)))

;; Save buffer when exiting evil insert mode
(defun set-save-hook ()
  (interactive)
  (add-hook 'evil-insert-state-exit-hook 'save-if-code-buffer))

(defun clear-save-hook ()
  (interactive)
  (remove-hook 'evil-insert-state-exit-hook 'save-if-code-buffer))
(set-save-hook)
;; (add-hook 'evil-insert-state-exit-hook 'evil-cp-insert)

;; Get colorized compilation buffers (mocha tests)
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


(provide 'general-behavior)
