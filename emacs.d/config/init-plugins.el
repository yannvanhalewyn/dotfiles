(require 'util)

(use-package scss-mode)

(use-package general
  :config
  (setq general-default-states '(normal emacs motion))
  (general-create-definer keys-l :prefix "SPC")
  (defalias 'keys 'general-define-key)

  (keys-l :keymaps '(emacs-lisp-mode-map scheme-mode-map)
          "e" 'eval-defun
          "E" 'eval-print-last-sexp)

  (keys-l
   "B" 'ibuffer
   "b" 'ido-switch-buffer
   "c" (build-keymap
        "u" 'cis/update
        "o" 'cis/open-ci-build)
   "d" 'dired-current-dir
   "f" 'helm-projectile
   "v" (build-keymap
        "e" 'edit-evil
        "f" 'edit-functions
        "g" 'edit-general-behavior
        "l" 'edit-layout
        "p" 'edit-plugins
        "t" 'edit-todo)
   "h" (build-keymap
        "k" 'describe-key
        "m" 'describe-mode
        "f" 'describe-function
        "a" 'helm-apropos)
   "i" (build-keymap
        "u" 'ucs-insert)
   "m" 'rename-current-buffer-file
   "o" 'ido-find-file
   "Q" 'delete-other-windows
   "q" 'kill-this-buffer
   "r" 'chrome-reload
   "w" 'buff-swap
   "x" 'projectile-ag
   "X" 'ag))

(use-package auto-complete :config (global-auto-complete-mode t))

(use-package evil-mc
  :defer t
  :init (global-evil-mc-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package ace-jump-mode
  :init
  (keys-l "j" 'ace-jump-mode
          "J" 'ace-jump-char-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode t))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1)
  (setq which-key-idle-delay 0.5)
  (which-key-setup-side-window-bottom))

(use-package google-this
  :defer t
  :config
  (keys-l "G" 'google-this))

;; Ruby/Rails
;; ==========
(use-package haml-mode :defer t)
(use-package yaml-mode :defer t)
(use-package sass-mode :defer t)

(use-package coffee-mode
  :defer t
  :config
  (require 'coffee-evil-extensions)
  (setq coffee-tab-width 2)
  (keys :keymaps '(coffee-mode-map)
        "o" 'coffee-open-below
        "O" 'coffee-open-above
        "<" 'coffee-indent-shift-left
        ">" 'coffee-indent-shift-right
        "g" (build-keymap
             "m" 'coffee-find-model
             "v" 'coffee-find-component
             "r" 'coffee-find-redux
             "t" 'coffee-find-test)))

(use-package rspec-mode
  :defer t
  :init
  (eval-after-load 'rspec-mode '(rspec-install-snippets))
  (keys-l :keymaps '(ruby-mode-map)
          "t" 'rspec-verify
          "a" 'rspec-verify-all
          "s" 'rspec-verify-single
          "l" 'rspec-rerun))

(require 'mocha)
(keys-l :keymaps '(coffee-mode-map js-mode-map)
        "a" 'mocha-test-project
        "t" 'mocha-test-file
        "s" 'mocha-test-at-point)
(setq mocha-project-test-directory "frontend/test"
      mocha-environment-variables "NODE_PATH=./frontend/src"
      mocha-options "--watch ./tmp/static.js ./frontend/test/config.coffee"
      mocha-reporter "spec")

;; Using pry in rspec buffers
(use-package inf-ruby
  :config
  (add-hook 'after-init-hook 'inf-ruby-switch-setup))

;; Close do-end blocks in ruby
(use-package smartparens
  :defer t
  :config (require 'smartparens-ruby))

(add-hooks #'smartparens-mode '(coffee-mode-hook ruby-mode-hook js-mode-hook))

(defun logit ()
  (interactive)
  (message "ok, javascript!"))

;; Rubocop
(use-package flycheck
  :defer t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)))

;; For goto file in require statements
(use-package bundler :defer t)

(use-package yard-mode
  :defer t
  :config (add-hook 'ruby-mode-hook 'yard-mode))

;; Lisps
;; =====
(use-package cider
  :defer t
  :config
  (defvar cider-mode-maps
    '(cider-repl-mode-map
      clojure-mode-map
      clojurescript-mode-map))

  (keys :keymaps cider-mode-maps
        :prefix "g"
        "f" 'cider-find-var
        "v" 'cljs-find-component
        "d" 'cljs-find-card)

  (keys-l :keymaps cider-mode-maps
          "a" 'cider-test-run-project-tests
          "c" (build-keymap
               "a" 'cider-apropos
               "c" 'cider-connect-local
               "d" 'cider-doc
               "j" 'cider-jack-in
               "k" 'cider-repl-clear-buffer
               "m" 'cider-macro-expand-1
               "q" 'cider-quit
               "r" 'cider-restart)
          "e" 'cider-eval-defun-at-point
          "E" 'cider-eval-buffer
          "k" 'cider-load-buffer
          "l" 'cider-test-rerun-tests
          "t" 'cider-test-run-test))

(use-package clj-refactor :defer t)
(use-package rainbow-delimiters :defer t)

(use-package paredit
  :defer t
  :diminish paredit-mode)

(use-package aggressive-indent
  :defer t
  :diminish aggressive-indent-mode
  :init
  (setq clojure-indent-style :always-align))

;; Load up rainbow delimiters/paredit when writing el
(defun parainbow-mode ()
  (interactive)
  (paredit-mode)
  (evil-cleverparens-mode)
  (aggressive-indent-mode)
  (rainbow-delimiters-mode)
  (eldoc-mode))

(defun clj-mode ()
  (interactive)
  (message "Doing initial CLJ setup")
  (clj-refactor-mode)
  (dolist (word '(fori match facts fact))
    (put-clojure-indent word 1)))

(defvar lisp-mode-hooks '(clojure-mode-hook
                          scheme-mode
                          clojurescript-mode-hook
                          cider-repl-mode-hook
                          emacs-lisp-mode-hook))

(defvar clj-mode-hooks '(clojure-mode-hook clojurescript-mode-hook))

(add-hooks #'parainbow-mode lisp-mode-hooks)
(add-hooks #'clj-mode clj-mode-hooks)

;; Project navigation
;; ==================
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (setq projectile-require-project-root nil
        projectile-switch-project-action 'helm-projectile)
  (keys "gs" 'projectile-switch-project)
  (keys-l "p" 'projectile-command-map))

(use-package neotree
  :defer t
  :init (keys-l "n" 'neotree-project-root)
  :config
  ;; Open current file in tree
  (evil-make-overriding-map neotree-mode-map 'normal t)
  (keys :keymaps '(neotree-mode-map)
        "d" 'neotree-delete-node
        "J" 'neotree-select-down-node
        "K" 'neotree-select-up-node
        "q" 'neotree-hide
        "m" 'neotree-rename-node
        "o" 'neotree-enter
        "x" (lambda () (interactive) (neotree-select-up-node) (neotree-enter))
        "<tab>" 'neotree-quick-look))

;; Projectile-ag
(use-package ag
  :defer t
  :init (setq ag-reuse-buffers t))

(use-package helm
  :defer t
  :config
  (setq helm-buffers-fuzzy-matching t
        helm-M-x-fuzzy-match t
        helm-apropos-fuzzy-match t))

(use-package helm-projectile :defer t)

(use-package magit
  :defer t
  :init
  (keys-l "g" (build-keymap
               "B" 'magit-blame-quit
               "c" 'magit-checkout
               "C" 'magit-branch-and-checkout
               "b" 'magit-blame
               "d" 'vc-diff
               "f" 'magit-fetch-all
               "F" 'magit-pull-popup
               "l" 'magit-log
               "o" 'browse-current-line-github
               "p" 'magit-push-current-to-upstream
               "s" 'magit-status
               "r" (build-keymap
                    "a" 'magit-rebase-abort
                    "c" 'magit-rebase-continue
                    "i" 'magit-rebase-interactive
                    "s" 'magit-rebase-skip)
               "l" 'magit-log))
  :config
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  (keys :keymaps 'magit-blame-mode-map
        "q" 'magit-blame-quit)
  (keys :keymaps 'git-rebase-mode-map
        "J" 'git-rebase-move-line-down
        "K" 'git-rebase-move-line-up
        "d" 'git-rebase-kill-line
        "p" 'git-rebase-pick)
  (keys :keymaps 'magit-status-mode-map
        "TAB" 'magit-section-toggle
        "K" 'magit-discard))

(use-package projectile-rails
  :config
  ;; Won't start unless rails project
  (add-hook 'projectile-mode-hook 'projectile-rails-on)
  (setq projectile-tags-file-name ".git/tags")
  (keys :prefix "g"
        :keymaps  'ruby-mode-map
        "r" 'projectile-rails-find-current-controller
        "R" 'projectile-rails-find-controller
        "f" 'projectile-rails-goto-file-at-point
        "m" 'projectile-rails-find-current-model
        "M" 'projectile-rails-find-model
        "v" 'projectile-rails-find-current-view
        "V" 'projectile-rails-find-view
        "i" 'open-current-ticket-in-redmine
        "t" 'split-window-with-rspec-alternate-file
        "T" 'projectile-rails-find-spec))

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
