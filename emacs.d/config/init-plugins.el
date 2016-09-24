(use-package scss-mode)

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package company
  :defer t
  :diminish company-mode
  :config (global-company-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode t))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1)
  (which-key-setup-side-window-bottom))

;; Ruby
;; ====
(use-package rspec-mode
  :defer t
  :config
  (eval-after-load 'rspec-mode '(rspec-install-snippets)))

;; Using pry in rspec buffers
(use-package inf-ruby
  :config
  (add-hook 'after-init-hook 'inf-ruby-switch-setup))

;; Close do-end blocks in ruby
(require 'smartparens-ruby)
(add-hook 'ruby-mode-hook #'smartparens-mode)

;; Rubocop
(use-package flycheck
  :defer t
  :config
  (add-hook 'ruby-mode-hook #'flycheck-mode))

;; For goto file in require statements
(use-package bundler :defer t)

(use-package yard-mode
  :defer t
  :config (add-hook 'ruby-mode-hook 'yard-mode))

;; Lisps
;; =====
(use-package cider :defer t)
(use-package clj-refactor :defer t)
(use-package rainbow-delimiters :defer t)

(use-package paredit
  :defer t
  :diminish paredit-mode)

(use-package evil-cleverparens
  :defer t
  :diminish evil-cleverparens-mode
  :config
  ;; Don't use crazy bindings for {, [, } and ] from evil-cleverparens
  (setq evil-cleverparens-use-additional-movement-keys nil))

(use-package aggressive-indent
  :defer t
  :diminish aggressive-indent-mode)

;; Load up rainbow delimiters/paredit when writing el
(defun enable-parainbow ()
  (paredit-mode)
  (evil-cleverparens-mode)
  (aggressive-indent-mode)
  (rainbow-delimiters-mode))

(add-hook 'emacs-lisp-mode-hook #'enable-parainbow)
(add-hook 'clojure-mode-hook #'enable-parainbow)
(add-hook 'clojurescript-mode-hook #'enable-parainbow)
(add-hook 'cider-repl-mode-hook #'enable-parainbow)

;; Project navigation
;; ==================
(use-package projectile
  :diminish projectile-mode
  :defer t
  :config
  (projectile-global-mode)
  (setq projectile-require-project-root nil
        projectile-switch-project-action 'helm-projectile-find-file))

;; Projectile-ag
(use-package ag
  :defer t
  :init (setq ag-reuse-buffers t))

(use-package helm
  :defer t
  :config
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-M-x-fuzzy-match t))

(use-package helm-projectile :defer t)

(use-package magit
  :defer t
  :config (use-package magithub))

(use-package projectile-rails
  :config
  ;; Won't start unless rails project
  (add-hook 'projectile-mode-hook 'projectile-rails-on)
  (setq projectile-tags-file-name ".git/tags"))

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
