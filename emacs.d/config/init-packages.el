(require 'util)

;; General use
;; ===========
(use-package general
  :config
  (setq general-default-states '(normal emacs motion))
  (general-create-definer keys-l :prefix "SPC")
  (defalias 'keys 'general-define-key)

  (keys-l :keymaps '(emacs-lisp-mode-map scheme-mode-map)
          "e" 'eval-defun
          "E" 'eval-print-last-sexp)

  (keys-l
   "a" (build-keymap
        "c" 'quick-calc
        "C" 'calc-dispatch)
   "B" 'ibuffer
   "b" 'ido-switch-buffer
   "c" (build-keymap
        "u" 'cis/update
        "o" 'cis/open-ci-build
        "t" 'comment-as-title)
   "d" 'dired-current-dir
   "F" 'helm-projectile-find-file
   "f" (build-keymap
        "r" 'helm-recentf
        "f" 'helm-projectile-find-file
        "m" 'rename-current-buffer-file
        "c" 'copy-current-buffer-file
        "d" 'delete-current-buffer-file
        "s" 'save-buffer
        "S" 'save-some-buffers
        "j" 'junk-file/new
        "J" 'junk-file/find)
   "v" (build-keymap
        "e" 'edit-evil
        "f" 'edit-functions
        "g" 'edit-general-behavior
        "l" 'edit-layout
        "p" 'edit-packages
        "t" 'edit-todo)
   "h" (build-keymap
        "a" 'helm-apropos
        "f" 'describe-function
        "k" 'which-key-show-top-level
        "K" 'describe-key
        "m" 'describe-mode
        "p" 'describe-package
        "v" 'describe-variable)
   "i" (build-keymap
        "u" 'ucs-insert
        "U" 'helm-ucs)
   "o" 'ido-find-file
   "O" (build-keymap
        "t" 'org-todo
        "T" 'org-insert-todo-heading)
   "Q" 'delete-other-windows
   "q" 'kill-this-buffer
   "R" 'chrome-reload
   "S" 'shell
   "s" (build-keymap
        "s" 'shell
        "k" 'shell-clear-buffer)
   "w" 'buff-swap
   "x" 'projectile-ag
   "X" 'ag))

(use-package evil
  :init
  (setq evil-want-fine-undo t)
  (add-hook #'after-change-major-mode-hook
            (lambda () (interactive)
              (modify-syntax-entry ?_ "w")))

  :config
  (evil-mode t)

  (evil-add-hjkl-bindings package-menu-mode-map 'emacs)
  (evil-add-hjkl-bindings ibuffer-mode-map 'emacs)

  (keys :states 'motion
        "[e" 'flycheck-previous-error
        "]e" 'flycheck-next-error
        "[b" 'previous-code-buffer
        "]b" 'next-code-buffer
        "]t" 'cycle-theme)
  (keys "C-h" 'evil-window-left
        "C-j" 'evil-window-down
        "C-k" 'evil-window-up
        "C-l" 'evil-window-right)

  (use-package evil-nerd-commenter
    :diminish evil-commentary-mode
    :config
    (evil-commentary-mode)
    (keys :states 'normal "gc" 'evilnc-comment-operator)
    (keys-l :states 'normal
            "c y" 'evilnc-copy-and-comment-lines
            "c a t" 'comment-as-title))

  (use-package evil-surround
    :config (global-evil-surround-mode 1))

  (use-package evil-cleverparens
    :defer t
    :diminish evil-cleverparens-mode
    :config
    ;; Evil CP overwrites "c" for change. This will re-enable "cs"
    ;; motion "change surrounding" of evil-surround
    (evil-cp--enable-surround-operators)
    :init
    ;; Don't use crazy bindings for {, [, } and ] from evil-cleverparens
    (setq evil-cleverparens-use-additional-movement-keys nil))

  (use-package evil-numbers
    :config
    (keys :prefix "g"
          "a" 'evil-numbers/inc-at-pt
          "x" 'evil-numbers/dec-at-pt)))

(use-package magit
  :defer t
  :init
  (keys-l "g" (build-keymap
               "B" 'magit-blame-quit
               "c" 'magit-checkout
               "C" 'magit-branch-and-checkout
               "b" 'magit-blame
               "d" 'vc-diff
               "D" 'magit-diff
               "f" 'magit-find-file
               "F" 'magit-pull-popup
               "l" 'magit-log-popup
               "o" 'browse-current-line-github
               "p" 'magit-push-current-to-upstream
               "P" 'force-push-with-lease
               "s" 'magit-status
               "S" 'magit-stash
               "r" (build-keymap
                    "r" 'magit-rebase
                    "a" 'magit-rebase-abort
                    "c" 'magit-rebase-continue
                    "i" 'magit-rebase-interactive
                    "s" 'magit-rebase-skip)))
  :config
  (use-package evil-magit)
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  (keys :keymaps '(magit-revision-mode-map diff-mode-map)
        :states 'visual
        "y" 'yank-from-revision-buffer)
  (keys :keymaps 'magit-blame-mode-map "q" 'magit-blame-quit)
  (keys :keymaps 'git-rebase-mode-map "q" 'magit-rebase-abort)
  (keys :keymaps 'magit-status-mode-map "K" 'magit-discard))

(use-package magithub
  :after magit
  :config (magithub-feature-autoinject t))

(use-package company
  :diminish company-mode
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (keys :states '(insert)
        "<tab>" 'company-complete-common-or-cycle)
  (keys :keymaps 'company-active-map
        :states nil
        "C-s" 'company-filter-candidates
        "<tab>" 'company-complete-common-or-cycle
        "S-<tab>" 'company-select-previous-or-abort))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (keys :states '(insert)
        "S-<tab>" 'yas-expand))

(use-package ace-jump-mode
  :defer t
  :init
  (keys-l "SPC" 'ace-jump-mode
          "S-SPC" 'ace-jump-char-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode t))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1)
  (setq which-key-idle-delay 0.5)
  (which-key-setup-side-window-bottom)
  (which-key-add-key-based-replacements
    "SPC a" "Applications"
    "SPC c" "Cider / CI / Comment"
    "SPC f" "Files"
    "SPC g" "Git"
    "SPC g r" "Rebase"
    "SPC h" "Help"
    "SPC i" "Insert"
    "SPC p" "Project"
    "SPC s" "Sexp / Shell"
    "SPC v" "View configuration"))

;; Ruby/Rails
;; ==========
(use-package haml-mode :defer t)
(use-package yaml-mode :defer t)
(use-package css-mode)
(use-package sass-mode :defer t)
(use-package scss-mode)

(use-package coffee-mode
  :defer t
  :config
  (setq coffee-tab-width 2)
  (require 'coffee-evil-extensions)
  (require 'mocha)

  (setq mocha-project-test-directory "frontend/test"
        mocha-environment-variables "NODE_PATH=./frontend/src"
        mocha-options "--watch ./tmp/static.js ./frontend/test/config.coffee"
        mocha-reporter "spec")

  (keys-l :keymaps '(coffee-mode-map js-mode-map)
          "a" 'mocha-test-project
          "t" 'mocha-test-file
          "s" 'mocha-test-at-point)

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

(use-package prettier-js
  :defer t
  :config
  (setq prettier-js-args '("--trailing-comma" "all"))
  :init
  (add-hooks #'prettier-js-mode '(js2-mode-hook js-mode-hook)))

;; Using pry in rspec buffers
(use-package inf-ruby
  :config
  (add-hook 'after-init-hook 'inf-ruby-switch-setup))

;; Close do-end blocks in ruby
(use-package smartparens
  :defer t
  :config
  (require 'smartparens-ruby)
  (sp-local-pair 'c++-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'c-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'js2-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'glsl-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

  (keys-l "s" (build-keymap
               "a" 'sp-absorb-sexp
               "c" 'paredit-convolute-sexp
               "l" 'sp-forward-slurp-sexp
               "h" 'sp-forward-barf-sexp
               "b" 'sp-forward-barf-sexp
               "B" 'sp-backward-barf-sexp
               "s" 'sp-foward)))

;; GLSL
;; ====
(use-package glsl-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.vert") . glsl-mode)
  (add-to-list 'auto-mode-alist '("\\.frag") . glsl-mode))

(add-hooks #'smartparens-mode '(coffee-mode-hook ruby-mode-hook js-mode-hook c-mode-common-hook))

;; Rubocop
(use-package flycheck
  :diminish flycheck-mode
  :defer t
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (use-package flycheck-flow)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (add-hook 'c-mode-common-hook
            (lambda ()
              (setq flycheck-gcc-language-standard "c++14")
              (setq flycheck-clang-language-standard "c++14"))))

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
  (setq cider-repl-display-help-banner nil)
  (defvar cider-mode-maps
    '(cider-repl-mode-map
      clojure-mode-map
      clojurescript-mode-map))

  (keys :keymaps cider-mode-maps
        :prefix "g"
        "f" 'cider-find-var
        "v" 'cider-find-cljs
        "b" 'cider-find-clj
        "d" 'cljs-find-card)

  (keys-l :keymaps cider-mode-maps
          "c" (build-keymap
               "a" 'cider-apropos
               "c" 'cider-connect-local
               "d" 'cider-doc
               "i" 'cider-inspect-last-result
               "j" 'cider-jack-in
               "k" 'cider-repl-clear-buffer
               "m" 'cider-macro-expand-1
               "n" 'cider-repl-set-ns
               "q" 'cider-quit)
          "e" 'cider-eval-defun-at-point
          "E" 'cider-eval-buffer
          "t" (build-keymap
               "s" 'cider-test-run-test
               "t" 'cider-test-run-ns-tests
               "f" 'cider-test-rerun-failed-tests
               "l" 'cider-test-rerun-test
               "a" 'cider-test-run-project-tests
               "A" 'cider-auto-test-mode)))

(use-package clj-refactor :defer t
  :config
  (let ((cljr-map (make-sparse-keymap)))
    (dolist (details cljr--all-helpers)
      (define-key cljr-map (car details) (cadr details)))
    (keys-l :keymaps 'clojure-mode-map
            "r" cljr-map)))

(use-package rainbow-delimiters :defer t)

(use-package paredit
  :defer t
  :diminish paredit-mode)

(use-package aggressive-indent
  :defer t
  :diminish aggressive-indent-mode)

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
  (setq clojure-indent-style :align-arguments)
  (dolist (word '(assoc-if transform match facts fact assoc render))
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
  (define-key projectile-command-map (kbd "C") 'projectile-compile-project)
  (define-key projectile-command-map (kbd "c") 'recompile)
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
        "n" 'neotree-create-node
        "c" 'neotree-copy-node
        "o" 'neotree-enter
        "x" (lambda () (interactive) (neotree-select-up-node) (neotree-enter))
        "<tab>" 'neotree-quick-look))

;; Projectile-ag
(use-package ag
  :defer t
  :init (setq ag-reuse-buffers t))

(use-package helm
  :diminish helm-mode
  :config
  (helm-mode)
  (add-to-list 'helm-completing-read-handlers-alist '(rename-current-buffer-file))
  (add-to-list 'helm-completing-read-handlers-alist '(copy-current-buffer-file))
  (add-to-list 'helm-completing-read-handlers-alist '(neotree-create-node))
  (setq helm-mode-fuzzy-match     t
        helm-M-x-fuzzy-match      t
        helm-apropos-fuzzy-match  t
        helm-recentf-fuzzy-match  t))

(use-package helm-projectile)

(use-package projectile-rails
  :config
  ;; Won't start unless rails project
  (add-hook 'projectile-mode-hook 'projectile-rails-on)
  ;; (setq projectile-tags-file-name ".git/tags")
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
  :defer t
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'init-packages)