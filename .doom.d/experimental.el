;;; experimental.el --- Some experimental configurations to play around with  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Yann Vanhalewyn

;; Author: Yann Vanhalewyn <yann.vanhalewyn@gmail.com>


(message "Loaded experimental")

(load-file "~/.doom.d/cape-yasnippet.el")
(require 'cape-yasnippet)

(use-package! corfu
  :config
  (custom-set-faces! `(corfu-bar :background ,(doom-color 'magenta)))
  (custom-set-faces! `(corfu-current :background ,(doom-color 'magenta)))

  (mapc #'evil-declare-ignore-repeat
        '(corfu-next
          corfu-previous
          corfu-first
          corfu-last))

  (mapc #'evil-declare-change-repeat
        '(corfu-insert
          corfu-complete))

  (defun yvh/corfu-quit ()
    (interactive)
    (call-interactively 'corfu-quit)
    (evil-normal-state +1))

  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-prefix 1
        corfu-auto-delay 0.01
        corfu-separator ?\s
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match t
        corfu-preview-current nil
        corfu-preselect-first t
        corfu-on-exact-match nil
        corfu-echo-documentation nil
        corfu-scroll-margin 10
        corfu-popupinfo-delay 0)

  (map! :map global-map
        :nvi "C-SPC" #'completion-at-point)

  (map! :map corfu-map
        :i "<tab>" #'corfu-insert
        :i "<ret>" nil ;; TODO make this work
        ;; :i "S-<tab>" #'corfu-previous
        ;; :nvi "C-j" #'corfu-next
        ;; :nvi "C-k" #'corfu-previous
        ;; :nvi "C-l" #'corfu-insert
        ;; :nvi "C-;" #'corfu-insert
        ;; :nvi "TAB" #'corfu-insert
        ;; :nvi "<tab>" #'corfu-insert
        ;; :nvi "<escape>" #'++corfu-quit
        :i "<escape>" #'yvh/corfu-quit)

  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package! kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; (use-package lsp-mode
;;   :custom
;;   (lsp-completion-provider :none) ;; we use Corfu!
;;   ;; :init
;;   ;; (defun my/lsp-mode-setup-completion ()
;;   ;;   (message "MY LSP MODE SETUP COMPLETION")
;;   ;;   (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;   ;;         '(orderless))) ;; Configure orderless
;;   ;; :hook
;;   ;; (lsp-completion-mode . my/lsp-mode-setup-completion)
;;   )

(defun yvh/load-capf--programming ()
  (interactive)
  (dolist (capf '(cape-file cape-yasnippet cape-dabbrev))
    (add-to-list 'completion-at-point-functions capf)))

(defun yvh/load-capf--writing ()
  (interactive)
  (dolist (capf '(cape-file cape-symbol cape-dabbrev cape-dict cape-ispell cape-line  cape-rfc1345 cape-yasnippet))
    (add-to-list 'completion-at-point-functions capf)))

(add-hook 'prog-mode-hook 'yvh/load-capf--programming)

;; Completion at point extension
(use-package! cape
  ;; Should work like this but doesn't, probably because eglot overwrites them
  ;; or something?
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  (add-to-list 'completion-at-point-functions #'cape-yasnippet)

  :config
  (setq cape-dabbrev-min-length 2
        cape-dabbrev-check-other-buffers 'some))

(setq ;; cider-ns-refresh-before-fn "bardistry.core/refresh"
 cider-ns-refresh-after-fn "dev/reset")

(use-package! html-to-hiccup
  ;; :bind (:localleader
  ;;        :map clojure-mode-map
  ;;        ("c h c" . html-to-hiccup-convert-region)
  ;;        ("c h y" . html-to-hiccup-yank))
  :config
  (setq html-to-hiccup-use-shorthand-p t))

(set-face-foreground 'line-number (face-foreground 'font-lock-doc-face))

(setq display-line-numbers-type 'relative)

;; (face-attribute 'font-lock-doc-face :background)

;; (custom-set-faces
;;  '(line-number ((t (:inherit font-lock-doc-face)))))

(use-package! vundo
  :config
  (map! (:leader :n "s u" 'vundo)))
