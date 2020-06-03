;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "functions")

(setq user-full-name "Yann Vanhalewyn"
      user-mail-address "yann.vanhalewyn@gmail.com")

(setq doom-font (font-spec :family "Monaco" :size 14 :weight 'semi-light))
(setq doom-theme 'doom-one)

(map! :leader "SPC" nil)
(setq doom-localleader-key "SPC SPC")


(setq display-line-numbers-type t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG

(defconst yvh/gtd-main (expand-file-name "gtd.org" org-directory))
(defconst yvh/gtd-inbox (expand-file-name "inbox.org" org-directory))
(defconst yvh/gtd-someday (expand-file-name "someday.org" org-directory))
(defconst yvh/org-timesheet (expand-file-name "timesheet.org" org-directory))

(setq org-directory "~/Google Drive/Documents/org"

      org-refile-targets '((yvh/gtd-main :maxlevel . 1)
                           (yvh/gtd-someday :level . 1))

      org-capture-templates `(("t" "Todo [inbox]" entry
                               (file ,yvh/gtd-inbox)
                               "* TODO %i%?"))

      org-tags-column 75
      org-agenda-files `(,yvh/gtd-main ,yvh/gtd-inbox)
      org-ellipsis "↷")

(add-hook 'org-mode-hook '(lambda () (interactive) (org-content 3)))
(add-hook 'org-mode-hook
          (lambda ()
            (push '("[ ]" . "☐") prettify-symbols-alist)
            (push '("[X]" . "☑" ) prettify-symbols-alist)
            (push '("[-]" . "❍" ) prettify-symbols-alist)
            (push '("#+BEGIN_SRC" . "λ") prettify-symbols-alist)
            (push '("#+END_SRC" . "λ") prettify-symbols-alist)
            (prettify-symbols-mode)))

(setq ivy-re-builders-alist '((counsel-ag . ivy--regex)
                              (swiper . ivy--regex-plus)
                              (t . ivy--regex-fuzzy)))

(map!
 (:map org-mode-map
  :n "-" 'org-toggle-checkbox
  (:localleader
   "r" 'org-refile
   "a" 'org-archive-subtree-default-with-confirmation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General

(map!
 "C-h" 'evil-window-left
 "C-j" 'evil-window-down
 "C-k" 'evil-window-up
 "C-l" 'evil-window-right

 :i "C-y" 'yank
 :n "[ r" 'lsp-ui-find-prev-reference
 :n "] r" 'lsp-ui-find-next-reference
 :n "[ e" 'flycheck-previous-error
 :n "] e" 'flycheck-next-error
 :n "/"   'counsel-grep-or-swiper

 (:map smartparens-mode-map
  :n ")"   'sp-forward-slurp-sexp
  :n "("   'sp-forward-barf-sexp
  :n "s-)" 'sp-backward-barf-sexp
  :n "s-(" 'sp-backward-slurp-sexp)

 (:leader
  "q" 'kill-current-buffer
  "Q" 'doom/window-maximize-buffer
  "S" 'shell
  "x" 'counsel-projectile-ag
  "d" 'yvh/dired-current-dir
  "D" 'yvh/dired-project-root
  "n" '+neotree/open
  "N" '+neotree/find-this-file
  (:prefix "f"
   "f" '+ivy/projectile-find-file
   "S" 'save-some-buffers
   "o" 'counsel-find-file
   "m" 'yvh/rename-current-buffer-file)
  "c t" 'yvh/comment-as-title--bm
  "p t" 'yvh/view-test-file-in-other-window
  (:prefix "o"
   :desc "Capture inbox"
   "c" '(lambda () (interactive) (org-capture nil "t"))))

 (:prefix "g"
  :n "t" (yvh/find-file-i 'yvh/gtd-main)
  :n "i" (yvh/find-file-i 'yvh/gtd-inbox)
  :n "s" (yvh/find-file-i 'yvh/gtd-someday)
  :n "h" (yvh/find-file-i 'yvh/org-timesheet))

 ;; Clojure bindings
 (:map (clojure-mode-map clojurescript-mode-map)
  :i "C-0" 'sp-forward-slurp-sexp
  :i "C-9" 'sp-forward-barf-sexp)

 ;; :leader will put them in a doom-leader map. I want leader bindings that are not localleader
 (:localleader  
  (:map (clojure-mode-map clojurescript-mode-map)
   (:prefix ("e" . "eval")
    :n "e" 'yvh/cider-eval-sexp-up-to-point
    :n "d" 'cider-eval-defun-at-point
    :n "b" 'cider-eval-buffer
    :n "l" 'cider-eval-last-sexp
    (:prefix ("p" . "print")
     :n "d" 'cider-pprint-eval-defun-at-point
     :n "c" 'cider-pprint-eval-defun-to-comment
     :n "l" 'cider-eval-print-last-sexp
     :n "p" 'cider-pprint-eval-last-sexp))
   (:prefix "s"
    :n "c" 'sp-convolute-sexp)
   (:prefix ("t" . "test")
    "l" 'cider-test-rerun-test
    "b" 'cider-test-run-ns-tests)
   (:prefix ("r" . "repl")
    :n "c" 'yvh/cider-connect-local))))


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
  (dolist (word '(try-let assoc-if letsc t/do-at transform match facts fact assoc render for-all))
    (put-clojure-indent word 1)))

(setq which-key-idle-delay 0.5)
;; Disable flycheck lsp checker, it conflicts with kondo too much
(setq lsp-diagnostic-package :none)

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

(use-package! aggressive-indent
  :hook ((clojure-mode clojurescript-mode emacs-lisp-mode)
         . aggressive-indent-mode))
