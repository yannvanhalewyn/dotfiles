;;; ../dotfiles/.doom.d/minial.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; (setq package-selected-packages '(clojure-mode lsp-mode cider lsp-treemacs flycheck company))
(setq package-selected-packages '(use-package evil))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic settings

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      ; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

(use-package clojure-mode :ensure t)

;; (use-package lsp-mode :ensure t)
;; (add-hook 'clojure-mode-hook 'lsp)
;; (add-hook 'clojurescript-mode-hook 'lsp)
;; (add-hook 'clojurec-mode-hook 'lsp)

(use-package eglot :ensure t)
(add-hook 'clojure-mode-hook 'eglot)
(add-hook 'clojurescript-mode-hook 'eglot)
(add-hook 'clojurec-mode-hook 'eglot)

(use-package cider :ensure t)
(use-package lsp-treemacs :ensure t)
(use-package flycheck :ensure t)

(use-package evil
  :ensure t
  :bind (("C-." . completion-at-point)
         :map evil-normal-state-map
         ("[ b" . previous-buffer)
         ("] b" . next-buffer))
  :init (evil-mode t))

(use-package corfu
  :ensure t
  :bind (:map corfu-map
              ;; ("<tab>" . corfu-insert)
              ("C-j" . corfu-next)
              )
  :config
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

  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

;; (use-package company
;;   :ensure t
;;   :diminish company-mode
;;   ;; :init (global-company-mode)
;;   :bind (:map company-active-map
;;          ("<tab>" . company-select-next)
;;          ("S-<tab>" . company-select-previous)
;;          ("RET" . company-complete)
;;          ("C-h" . nil)
;;          ("C-d" . company-show-doc-buffer)
;;          ("C-s" . company-filter-candidates)
;;          ("C-n" . company-select-next)
;;          ("C-p" . company-select-previous))
;;   :config
;;   (setq company-idle-delay 0
;;         ;; company-require-match nil
;;         ;; company-frontends '(company-tng-frontend
;;         ;;                     company-pseudo-tooltip-frontend
;;         ;;                     company-echo-metadata-frontend)
;;         ))
