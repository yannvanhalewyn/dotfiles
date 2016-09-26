(require 'util)

(use-package scss-mode)

(use-package general
  :config
  (setq general-default-states '(normal emacs motion))
  (general-create-definer keys-l :prefix "SPC")
  (defalias 'keys 'general-define-key)

  (keys-l :keymaps 'emacs-lisp-mode-map
          "e" 'eval-defun
          "E" 'eval-print-last-sexp)

  (keys-l
   "B" 'ibuffer
   "b" 'ido-switch-buffer
   "c" (build-keymap
        "u" 'cis/update
        "o" 'cis/open-ci-build)
   "d" 'dired-current-dir
   "y" 'edit-config
   "f" 'helm-projectile
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
   "x" 'projectile-ag))

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package ace-jump-mode
  :init
  (keys :states 'normal
        "F" 'ace-jump-mode))

(use-package company
  :defer t
  :diminish company-mode
  :init (global-company-mode)
  :config
  (define-key prog-mode-map (kbd "<tab>") 'company-complete)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode t))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1)
  (which-key-setup-side-window-right))

;; Ruby/Rails
;; ==========
(use-package haml-mode :defer t)
(use-package sass-mode :defer t)
(use-package coffee-mode :defer t)

(use-package rspec-mode
  :defer t
  :init
  (eval-after-load 'rspec-mode '(rspec-install-snippets))
  (keys-l :keymaps 'projectile-rails-mode-map
          "t" 'rspec-verify
          "a" 'rspec-verify-all
          "s" 'rspec-verify-single
          "l" 'rspec-rerun))

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
(use-package cider
  :defer t
  :config
  (defvar cider-mode-maps
    '(cider-repl-mode-map
      clojure-mode-map
      clojurescript-mode-map))

  (keys :keymaps cider-mode-maps
        "gf" 'cider-find-var)

  (keys-l :keymaps cider-mode-maps
          "a" 'cider-test-run-tests
          "c" (build-keymap
               "a" 'cider-apropos
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
  (rainbow-delimiters-mode)
  (eldoc-mode))

(defvar lisp-mode-hooks '(clojure-mode-hook
                          clojurescript-mode-hook
                          cider-repl-mode-hook
                          emacs-lisp-mode-hook))

(add-hooks #'enable-parainbow lisp-mode-hooks)

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
  :config
  (use-package magithub
    :config
    (magithub-toggle-ci-status-header))
  :init
  (keys :keymaps 'magit-blame-mode-map
        "q" 'magit-blame-quit)
  (keys :keymaps 'git-rebase-mode-map
        "J" 'git-rebase-move-line-down
        "K" 'git-rebase-move-line-up
        "d" 'git-rebase-kill-line
        "p" 'git-rebase-pick)
  (keys :keymaps 'magit-status-mode-map
        "TAB" 'magit-section-toggle
        "K" 'magit-discard)
  (keys-l "g" (build-keymap
               "B" 'magit-blame-quit
               "b" 'magit-blame
               "d" 'magit-diff
               "l" 'magit-log
               "o" 'browse-current-line-github
               "s" 'magit-status
               "r" (build-keymap
                    "a" 'magit-rebase-abort
                    "c" 'magit-rebase-continue
                    "i" 'magit-rebase-interactive
                    "s" 'magit-rebase-skip)
               "l" 'magit-log)))

(use-package projectile-rails
  :init
  ;; Won't start unless rails project
  (add-hook 'projectile-mode-hook 'projectile-rails-on)
  (setq projectile-tags-file-name ".git/tags")
  (keys :prefix "g"
        :keymaps 'projectile-rails-mode-map
        "f" 'projectile-rails-goto-file-at-point
        "m" 'projectile-rails-find-current-model
        "M" 'projectile-rails-find-model
        "v" 'projectile-rails-find-current-view
        "V" 'projectile-rails-find-view
        "r" 'projectile-rails-find-current-controller
        "R" 'projectile-rails-find-controller
        "t" 'rspec-find-spec-or-target-other-window
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
