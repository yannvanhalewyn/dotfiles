;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "functions")

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Yann Vanhalewyn"
      user-mail-address "yann.vanhalewyn@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Google Drive/Documents/org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG

(defconst yvh/gtd-main (expand-file-name "gtd.org" org-directory))
(defconst yvh/gtd-inbox (expand-file-name "inbox.org" org-directory))
(defconst yvh/gtd-someday (expand-file-name "someday.org" org-directory))
(defconst yvh/org-timesheet (expand-file-name "timesheet.org" org-directory))

(setq org-refile-targets '((yvh/gtd-main :maxlevel . 1)
                           (yvh/gtd-someday :level . 1))

      org-capture-templates `(("t" "Todo [inbox]" entry
                               (file ,yvh/gtd-inbox)
                               "* TODO %i%?"))

      org-tags-column 75)

;; (add-hook 'org-capture-mode-hook 'evil-insert-state)
;;

(add-hook 'org-mode-hook '(lambda () (interactive) (org-content 3)))
(add-hook 'org-mode-hook
          (lambda ()
            (push '("[ ]" . "☐") prettify-symbols-alist)
            (push '("[X]" . "☑" ) prettify-symbols-alist)
            (push '("[-]" . "❍" ) prettify-symbols-alist)
            (push '("#+BEGIN_SRC" . "λ") prettify-symbols-alist)
            (push '("#+END_SRC" . "λ") prettify-symbols-alist)
            (prettify-symbols-mode)))

;; (setq org-agenda-files `(,yvh/gtd-main ,yvh/gtd-inbox)
;;       org-log-done 'time
;;       org-html-postamble nil
;;       org-ellipsis "↷")

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
 :n "/"   'counsel-grep-or-swiper

 (:map smartparens-mode-map
  :n ")"   'sp-forward-slurp-sexp
  :n "("   'sp-forward-barf-sexp
  :n "s-)" 'sp-backward-barf-sexp
  :n "s-(" 'sp-backward-slurp-sexp)

 (:leader
  "q" 'kill-current-buffer
  "x" 'counsel-projectile-ag
  "d" 'yvh/dired-current-dir
  "D" 'yvh/dired-project-root
  "n" '+neotree/open
  "N" '+neotree/find-this-file
  "f f" '+ivy/projectile-find-file
  "f o" 'counsel-find-file
  "c t" 'yvh/comment-as-title--bm
  (:prefix "o"
   "c" '(lambda () (interactive) (org-capture nil "t"))))

 (:prefix "g"
  :n "t" (yvh/find-file-i 'yvh/gtd-main)
  :n "i" (yvh/find-file-i 'yvh/gtd-inbox)
  :n "s" (yvh/find-file-i 'yvh/gtd-someday)
  :n "h" (yvh/find-file-i 'yvh/org-timesheet)))

(defun save-if-code-buffer ()
  (when (buffer-file-name) (save-buffer)))

(defun set-save-hook! ()
  (interactive)
  (add-hook 'evil-insert-state-exit-hook 'save-if-code-buffer))

(defun clear-save-hook! ()
  (interactive)
  (remove-hook 'evil-insert-state-exit-hook 'save-if-code-buffer))

(set-save-hook!)

(show-paren-mode 1)

(custom-set-faces
      '(show-paren-match ((t (:background "#0E9E97" :weight bold)))))

(setq which-key-idle-delay 0.5)

(setq evil-cleverparens-use-additional-movement-keys nil
      evil-cleverparens-use-regular-insert t)

(use-package! evil-cleverparens
  :commands evil-cleverparens-mode
  ;; :hook ((emacs-lisp-mode-hook clojure-mode-hook) . evil-cleverparens-mode)
  :config
  ;; Evil CP overwrites "c" for change. This will re-enable "cs"
  ;; motion "change surrounding" of evil-surround
  (after! evil-surround
    (evil-cp--enable-surround-operators))
  :init
  ;; Don't use crazy bindings for {, [, } and ] from evil-cleverparens
  (setq evil-cleverparens-use-additional-movement-keys nil
        evil-cleverparens-use-regular-insert t)

  (defun yvh/enable-lisp-modes ()
  (interactive)
  ;; (paredit-mode)
  (evil-cleverparens-mode))

  (yhv/add-hooks #'yvh/enable-lisp-modes
                 '(scheme-mode
                   clojure-mode-hook
                   clojurescript-mode-hook
                   cider-repl-mode-hook
                   emacs-lisp-mode-hook)))


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
