;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "functions")

(setq user-full-name "Yann Vanhalewyn"
      user-mail-address "yann.vanhalewyn@gmail.com"
      delete-by-moving-to-trash nil

      doom-font (font-spec :family "Fira Code" :size 16)
      doom-theme 'doom-one
      doom-localleader-key ","

      ;; 80 is ok but this gets reset
      fill-column 81)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG

(use-package! org
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (push '("[ ]" . "☐") prettify-symbols-alist)
              (push '("[X]" . "☑" ) prettify-symbols-alist)
              (push '("[-]" . "❍" ) prettify-symbols-alist)
              (push '("#+begin_src" . "λ") prettify-symbols-alist)
              (push '("#+end_src" . "λ") prettify-symbols-alist)
              (prettify-symbols-mode)))

  (map!
   (:leader :n "o l" 'org-store-link)
   (:map org-mode-map
    :n "-" 'org-toggle-checkbox
    :n "RET" 'org-open-at-point))

  (setq org-ellipsis "↷"
        ;; org-ellipsis "▼"
        org-todo-keywords '((sequence "TODO" "DONE"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General

(map!
 :n "C-h" 'evil-window-left
 :n "C-j" 'evil-window-down
 :n "C-k" 'evil-window-up
 :n "C-l" 'evil-window-right
 :n "M-." 'dumb-jump-go
 :n "M-," 'dumb-jump-back
 :n "j"   'evil-next-visual-line
 :n "k"   'evil-previous-visual-line
 :i "M-<tab>" 'yas-expand
 :i "C-y" 'yank
 :i "C-i" 'insert-char
 :n "[ r" 'lsp-ui-find-prev-reference
 :n "] r" 'lsp-ui-find-next-reference
 ;; :n "[ e" 'flymake-goto-prev-error
 ;; :n "] e" 'flymake-goto-next-error
 :n "[ e" 'flycheck-previous-error
 :n "] e" 'flycheck-next-error
 :n "[ <space>" '+default/newline-above
 :n "] <space>" '+default/newline-below
 :n "[ <tab>" '+fold/close-all
 :n "] <tab>" '+fold/open-all
 :n "/"   'consult-line
 :n "|"   'yvh/transpose-windows

 (:map smartparens-mode-map
  :n ")"   'sp-forward-slurp-sexp
  :n "("   'sp-forward-barf-sexp
  :n "s-)" 'sp-backward-barf-sexp
  :n "s-(" 'sp-backward-slurp-sexp)

 (:leader
  "q" 'kill-current-buffer
  "Q" 'doom/window-maximize-buffer
  "S" 'shell
  "x" '+default/search-project
  "d" 'yvh/dired-current-dir
  "D" 'yvh/dired-project-root
  "n" '+neotree/find-this-file
  "R" 'yvh/chrome-reload
  (:prefix-map ("e" . "eDiff")
   "b" 'ediff-buffers
   "B" 'ediff-buffers3
   "f" 'ediff-files
   "F" 'ediff-files3)
  (:prefix ("f" . "file")
   "S" 'save-some-buffers
   "m" 'yvh/rename-current-buffer-file)
  "b a" 'persp-add-buffer
  "c d" 'lsp-ui-doc-show ;; 'code doc'
  "c t" 'yvh/comment-as-title
  "c r" '+lookup/references
  "c R" 'eglot-rename
  "t s" 'flyspell-mode
  "p t" 'projectile-toggle-between-implementation-and-test
  (:prefix "o"
   :desc "Capture inbox"
   "c" '(lambda () (interactive) (org-capture nil "t"))))

 (:prefix ("g" . "goto")
  :n "t" (yvh/find-file-i "~/Google Drive/My Drive/Documents/org/pilloxa_timesheet.org")
  :n "i" (yvh/find-file-i 'yvh/gtd-inbox)
  :n "s" (yvh/find-file-i 'yvh/gtd-someday)
  :n "h" (yvh/find-file-i 'yvh/org-timesheet)))

(setq js-indent-level 2)

(defun yvh/save-if-code-buffer ()
  (when (buffer-file-name) (save-buffer)))

(defun yvh/set-save-hook! ()
  (interactive)
  (add-hook 'evil-insert-state-exit-hook 'yvh/save-if-code-buffer))

(defun yvh/clear-save-hook! ()
  (interactive)
  (remove-hook 'evil-insert-state-exit-hook 'yvh/save-if-code-buffer))

(yvh/set-save-hook!)

(show-paren-mode 1)

(custom-set-faces
 '(show-paren-match ((t (:background "#0E9E97" :weight bold)))))

(use-package! clojure-mode
  :config

  ;; TODO check fix and make a Doom PR?
  (set-tree-sitter-lang! 'clojurescript-mode 'clojure)
  (set-tree-sitter-lang! 'clojurec-mode 'clojure)

  (after! clojure-mode
    (dolist (word '(try-let assoc-if assoc-some letsc t/do-at transform match facts fact assoc render for-all))
      (put-clojure-indent word 1)))

  (map!
   (:map (clojure-mode-map clojurescript-mode-map)
    :i "C-0" 'sp-forward-slurp-sexp
    :i "C-9" 'sp-forward-barf-sexp
    (:localleader
     :n "e f" 'lsp-clojure-extract-function
     :n "e t" 'lsp-clojure-expand-let
     :n "n c" 'lsp-clojure-clean-ns
     :n "n r" 'cider-ns-reload
     :n "n R" 'cider-ns-reload-all
     (:prefix ("e" . "Eval")
      :n "e" 'cider-eval-list-at-point
      :n "l" 'cider-eval-last-sexp
      :n "r" nil
      :n "r l" 'yvh/rebl-eval-last-sexp
      :n "r d" 'yvh/rebl-eval-defun
      (:prefix ("p" . "Pretty Print")
       :n "d" 'cider-pprint-eval-defun-at-point
       :n "c" 'cider-pprint-eval-defun-to-comment
       :n "p" 'cider-pprint-eval-last-sexp))
     (:prefix ("t" . "Test")
      "a" 'cider-test-rerun-test
      "l" 'cider-test-rerun-test
      "b" 'cider-test-run-ns-tests)
     (:prefix ("r" . "REPL / Refactor")
      :n "a m" 'lsp-clojure-add-missing-libspec
      :n "t l" 'lsp-clojure-thread-last
      :n "t L" 'lsp-clojure-thread-last-all
      :n "t f" 'lsp-clojure-thread-first
      :n "t F" 'lsp-clojure-thread-first-all)))
   (:map cider-inspector-mode-map
    :n "-" 'cider-inspector-pop)))


(evil-declare-not-repeat 'flycheck-next-error)
(evil-declare-not-repeat 'flycheck-previous-error)

(use-package! markdown-mode
  :config
  (map!
   (:map markdown-mode-map
    :n "C-c <space>" 'markdown-table-align
    (:localleader
     (:prefix ("t" . "align")
      :n "a" 'markdown-table-align)))))

(use-package! evil-cleverparens
  :hook ((clojure-mode clojurescript-mode cider-repl-mode emacs-lisp-mode)
         . evil-cleverparens-mode)
  :config
  ;; Evil CP overwrites "c" for change. This will re-enable "cs"
  ;; motion "change surrounding" of evil-surround
  (after! evil-surround
    (evil-cp--enable-surround-operators))
  :init
  ;; Don't use crazy bindings for {, [, } and ] from evil-cleverparens
  (setq evil-cleverparens-use-additional-movement-keys nil
        evil-cleverparens-use-regular-insert t))

(use-package! eval-sexp-fu
  :config
  (set-face-attribute 'eval-sexp-fu-flash nil
                      :background (face-attribute 'success :foreground)
                      :foreground "#292b2e")
  (set-face-attribute 'eval-sexp-fu-flash-error nil
                      :background (face-attribute 'error :foreground)
                      :foreground "#292b2e")

  (setq eval-sexp-fu-flash-duration 0.1)

  (use-package! cider-eval-sexp-fu))

(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  ;; Disabled because not sure if we should use it.
  (setq completion-cycle-threshold 3
        tab-always-indent 'complete

        ;; see uniquify--create-file-buffer-advice for more customisation
        uniquify-buffer-name-style 'forward
        uniquify-min-dir-content 3

        +evil-want-o/O-to-continue-comments nil
        which-key-idle-delay 0.5)
  (global-display-fill-column-indicator-mode))

(use-package! consult
  :config
  (setq consult-line-start-from-top t)

  (defun noct-consult-line-evil-history (&rest _)
    "Add latest `consult-line' search pattern to the evil search history ring.
This only works with orderless and for the first component of the search."
    (when (and (bound-and-true-p evil-mode)
               (eq evil-search-module 'evil-search))
      (let ((pattern (car (orderless-pattern-compiler (car consult--line-history)))))
        (add-to-history 'evil-ex-search-history pattern)
        (setq evil-ex-search-pattern (list pattern t t))
        (setq evil-ex-search-direction 'forward)
        (when evil-ex-search-persistent-highlight
          (evil-ex-search-activate-highlight evil-ex-search-pattern)))))

  (advice-add #'consult-line :after #'noct-consult-line-evil-history))

(use-package! neotree
  :config
  (setq doom-themes-neotree-file-icons t)
  (map!
   (:map
    neotree-mode-map
    :n "c" 'neotree-copy-node
    :n "o" 'neotree-enter
    :n "x" (lambda () (interactive) (neotree-select-up-node) (neotree-enter))
    :n "<tab>" 'neotree-quick-look)))

(use-package! git-link
  :defer t
  :config
  (setq git-link-open-in-browser t))

(use-package! popper
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*xref\\*"
          "\\*rg\\*"
          "\\*cider-test-report\\*"
          "\\*cider-error\\*"
          "\\*cider-result\\*"
          "\\*cider-inspect\\*"
          "\\*cider-repl.*"
          "\\*Embark Export:.*"
          "\\*Backtrace\\*"
          "\\*helpful.*"
          "\\*ert\\*"
          "\\*shell\\*"
          help-mode
          compilation-mode))

  :config
  (setq nrepl-use-ssh-fallback-for-remote-hosts t)

  (map!
   "M-p" 'popper-toggle-latest
   "M-P" 'popper-cycle
   (:leader "t p" 'popper-toggle-type)
   ;; Disable M-p in shell
   (:map comint-mode-map "M-p" nil)
   ;; Disable M-p in cider repl
   (:map cider-repl-mode-map
    "M-p" nil
    "M-P" nil))

  (popper-mode +1)
  (popper-echo-mode +1)

  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-below-selected)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.4)))

  (setq display-buffer-alist nil)
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-Display-Action-Alists.html
  (dolist (buffer-regex (list (rx bos "*help" (zero-or-more any) eos)
                              (rx bos "*messages*" eos)
                              (rx bos "*cider-result*" eos)
                              (rx bos "*cider-error*" eos)
                              (rx bos "*cider-inspect*" eos)))
    (add-to-list 'display-buffer-alist
                 `(,buffer-regex
                   ;; (display-buffer-reuse-window
                   ;;  display-buffer-pop-up-window)
                   (display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (window-width . 0.5)
                   (side . right)
                   (slot . -1)))))

(use-package! embark
  :config
  ;; Enable fuzzy search
  ;; (setq orderless-matching-styles '(orderless-flex))
  (setq orderless-matching-styles '(orderless-literal orderless-regexp)))

(use-package! glsl-mode)

(load-file "~/.doom.d/experimental.el")
