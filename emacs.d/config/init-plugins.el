(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"
          "~/.emacs.d/elpa/yasnippet-20150811.1222/snippets")))

;; Ruby
(use-package rspec-mode)
(use-package "inf-ruby")

;; Using pry in rspec buffers
(use-package flycheck)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-hook 'ruby-mode-hook #'flycheck-mode)
(eval-after-load 'rspec-mode '(rspec-install-snippets))

;; Lisps
(use-package cider)
(use-package paredit)
(use-package evil-cleverparens)
(use-package aggressive-indent)
(use-package clj-refactor)

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-require-project-root nil))

(use-package magit)
(use-package rainbow-delimiters)
(use-package helm)
(use-package helm-ls-git)
(use-package helm-projectile)
;; Maybe enable it for css with (add-hook 'css-mode-hook  'emmet-mode)
;; For some shortcuts
(use-package emmet-mode)

;; Yasnippet

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

;; CIDER performance
;; (setq cider-request-dispatch 'static)
(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")


(provide 'init-plugins)
