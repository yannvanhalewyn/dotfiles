;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "functions")

(setq user-full-name "Yann Vanhalewyn"
      user-mail-address "yann.vanhalewyn@gmail.com")

(setq delete-by-moving-to-trash nil)

(setq doom-font (font-spec :family "Fira Code" :size 16)
      doom-theme 'doom-one
      doom-leader-alt-key "C-SPC")
(setq fill-column 81) ;; 80 is ok but this gets reset

;; (map! :leader "SPC" nil)
(setq doom-localleader-key ",")

(global-display-fill-column-indicator-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG

;; (add-hook 'org-mode-hook '(lambda () (interactive) (org-content 3)))
(add-hook 'org-mode-hook
          (lambda ()
            (push '("[ ]" . "☐") prettify-symbols-alist)
            (push '("[X]" . "☑" ) prettify-symbols-alist)
            (push '("[-]" . "❍" ) prettify-symbols-alist)
            (push '("#+BEGIN_SRC" . "λ") prettify-symbols-alist)
            (push '("#+END_SRC" . "λ") prettify-symbols-alist)
            (prettify-symbols-mode)))

(map!
 (:map org-mode-map
  :n "-" 'org-toggle-checkbox
  :n "RET" 'org-open-at-point))

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
  "SPC" 'projectile-find-file
  "q" 'kill-current-buffer
  ;; "r" 'clj-refactor-map
  "Q" 'doom/window-maximize-buffer
  "S" 'shell
  "x" 'consult-ripgrep
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
   "f" '+ivy/projectile-find-file
   "S" 'save-some-buffers
   "o" 'find-file
   "m" 'yvh/rename-current-buffer-file)
  "b a" 'persp-add-buffer
  "c d" 'lsp-ui-doc-show ;; 'code doc'
  "c t" 'yvh/comment-as-title
  "c r" 'lsp-find-references
  "c R" 'lsp-rename
  "t s" 'flyspell-mode
  "p t" 'yvh/view-test-file-in-other-window
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

(after! clojure-mode
  (dolist (word '(try-let assoc-if assoc-some letsc t/do-at transform match facts fact assoc render for-all))
    (put-clojure-indent word 1)))

(setq which-key-idle-delay 0.5)
;; Disable flycheck lsp checker, it conflicts with kondo too much
;; Enable for RUST, but disable for CLojure?
;; Probably add or modify the predicate for the 'lsp checker
;; (setq lsp-diagnostic-package :none)
(evil-declare-not-repeat 'flycheck-next-error)
(evil-declare-not-repeat 'flycheck-previous-error)

(use-package! cider
  :config
  (map!
   (:map (clojure-mode-map clojurescript-mode-map)
    :i "C-0" 'sp-forward-slurp-sexp
    :i "C-9" 'sp-forward-barf-sexp
    (:localleader
     (:prefix ("e" . "eval")
      :n "e" 'yvh/cider-eval-sexp-up-to-point
      :n "l" 'cider-eval-last-sexp
      :n "r" nil
      :n "r l" 'yvh/rebl-eval-last-sexp
      :n "r d" 'yvh/rebl-eval-defun
      (:prefix ("p" . "pprint")
       :n "d" 'cider-pprint-eval-defun-at-point
       :n "c" 'cider-pprint-eval-defun-to-comment
       :n "p" 'cider-pprint-eval-last-sexp))
     (:prefix ("t" . "test")
      "l" 'cider-test-rerun-test
      "b" 'cider-test-run-ns-tests)
     (:prefix ("r" . "repl / refactor")
      ;; "c" nil
      ;; "c n" 'cljr-clean-ns
      ;; TODO have entire cljr-map somewhere
      "a m" 'cljr-add-missing-libspec
      "t l" 'cljr-thread-last-all
      "t f" 'cljr-thread-first-all)))))

(use-package! emmet-mode
  :config
  (map!
   ;; Disable annoying default keybind
   (:map emmet-mode-keymap
    "C-j" nil)))

(use-package! markdown-mode
  :config
  (map!
   (:map markdown-mode-map
    :n "C-c <space>" 'markdown-table-align
    (:localleader
     (:prefix ("t" . "align")
      :n "a" 'markdown-table-align)))))

(setq +evil-want-o/O-to-continue-comments nil)

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

(use-package! clj-refactor
  :config
  (let ((cljr-map (make-sparse-keymap)))
    (dolist (details cljr--all-helpers)
      (define-key cljr-map (car details) (cadr details)))
    (map!
     (:leader "r" cljr-map))) )

;; Performance killer, find alternative
;; (use-package! aggressive-indent
;;   :hook ((clojure-mode clojurescript-mode emacs-lisp-mode)
;;          . aggressive-indent-mode))

(use-package! company
  :config
  (setq company-idle-delay 0)
  (map!
   (:map company-active-map
    "C-h" nil
    "C-d" 'company-show-doc-buffer)))

(use-package! neotree
  :config
  (map!
   (:map
    neotree-mode-map
    :n "c" 'neotree-copy-node
    :n "o" 'neotree-enter
    :n "x" (lambda () (interactive) (neotree-select-up-node) (neotree-enter))
    :n "<tab>" 'neotree-quick-look)))

(use-package! lsp-ui
  :config
  (setq lsp-ui-sideline-show-code-actions nil))

(use-package! git-link
  :defer t
  :config
  (setq git-link-open-in-browser t))

(use-package popper
  :config
  (map!
   "M-p" 'popper-toggle-latest
   "M-P" 'popper-cycle
   (:leader "t p" 'popper-toggle-type))

  ;; Disable conflicting bindings in cider repl
  (map!
   :map cider-repl-mode-map
   "M-p" nil
   "M-P" nil)

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
          "\\*cider-repl.*"
          "\\*Embark Export:.*"
          "\\*Backtrace\\*"
          "\\*helpful.*"
          "\\*ert\\*"
          "\\*ivy-occur.*"
          help-mode
          compilation-mode))

  (defun yvh/popper-height (win)
    (fit-window-to-buffer
     win
     (floor (frame-height) 2)
     (floor (frame-height) 3)))

  (setq popper-window-height #'yvh/popper-height)

  (popper-mode +1)
  (popper-echo-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust

;; (setq rustic-lsp-server 'rust-analyzer)