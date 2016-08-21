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
  (setq evil-cleverparens-use-additional-movement-keys nil))
(use-package aggressive-indent :defer t)
(use-package clj-refactor :defer t)
(use-package rainbow-delimiters :defer t)

;; Projects
(use-package projectile
  :defer t
  :config
  (projectile-global-mode)
  (setq projectile-require-project-root nil))

;; Projectile-ag
(use-package ag)

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

;; Close do-end blocks in ruby
(require 'smartparens-ruby)
(add-hook 'ruby-mode-hook #'smartparens-mode)

(use-package projectile-rails
  :config
  ;; Won't start unless rails project
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; CIDER performance
;; (setq cider-request-dispatch 'static)
(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

(provide 'init-plugins)
