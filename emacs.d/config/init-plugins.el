(use-package yasnippet
  :defer t
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

;; Ruby
(use-package rspec-mode
  :defer t
  :config
  (eval-after-load 'rspec-mode '(rspec-install-snippets)))

;; Using pry in rspec buffers
(use-package inf-ruby
  :defer t
  :config
  (add-hook 'after-init-hook 'inf-ruby-switch-setup))

;; Rubocop
(use-package flycheck
  :defer t
  :config
  (add-hook 'ruby-mode-hook #'flycheck-mode))

;; Lisps
(use-package cider :defer t)
(use-package paredit :defer t)
(use-package evil-cleverparens
  :defer t
  :config
  ;; Don't use crazy bindings for {, [, } and ] from evil-cleverparens
  (setq evil-cp-additional-movement-keys nil))
(use-package aggressive-indent :defer t)
(use-package clj-refactor :defer t)
(use-package rainbow-delimiters :defer t)

;; Projects
(use-package projectile
  :defer t
  :config
  (projectile-global-mode)
  (setq projectile-require-project-root nil))

(use-package helm
  :defer t
  :config
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-M-x-fuzzy-match t))

(use-package helm-ls-git :defer t)
(use-package helm-projectile :defer t)

(use-package emmet-mode :defer t)

(use-package magit
  :defer t
  :config
  ;; Magit something the plugin told me, have no idea
  (setq magit-last-seen-setup-instructions "1.4.0"))

;; Load up rainbow delimiters/paredit when writing el
(defun enable-parainbow ()
  (paredit-mode)
  (evil-cleverparens-mode)
  (aggressive-indent-mode)
  (rainbow-delimiters-mode))

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
