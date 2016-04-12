(require-package 'yasnippet)
(require-package 'cider)
(require-package 'paredit)
(require-package 'magit)
(require-package 'rainbow-delimiters)
(require-package 'helm)
(require-package 'helm-ls-git)
(require-package 'helm-projectile)
;; (require-package 'smartparens) ;; Needed for cleverparens?
(require-package 'evil-cleverparens)
(require-package 'aggressive-indent)
;; Maybe enable it for css with (add-hook 'css-mode-hook  'emmet-mode)
;; For some shortcuts
(require-package 'emmet-mode)

;; Yasnippet
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        "~/.emacs.d/elpa/yasnippet-20150811.1222/snippets"))
(yas-global-mode 1)

;; Magit something the plugin told me, have no idea
(setq magit-last-seen-setup-instructions "1.4.0")

;; Helm
(setq helm-buffers-fuzzy-matching t)
(setq helm-M-x-fuzzy-match t)

;; Load up rainbow delimiters/paredit when writing el
(defun enable-parainbow ()
  (paredit-mode)
  (evil-cleverparens-mode)
  (aggressive-indent-mode)
  (rainbow-delimiters-mode))

;; Don't use crazy bindings for {, [, } and ] from evil-cleverparens
(setq evil-cleverparens-use-additional-movement-keys nil)

(add-hook 'emacs-lisp-mode-hook #'enable-parainbow)
(add-hook 'clojure-mode-hook #'enable-parainbow)
(add-hook 'cider-repl-mode-hook #'enable-parainbow)

(provide 'init-plugins)
