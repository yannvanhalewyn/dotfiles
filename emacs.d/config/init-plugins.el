(require-package 'yasnippet)
(require-package 'paredit)
(require-package 'magit)
(require-package 'rainbow-delimiters)
(require-package 'helm)
(require-package 'helm-ls-git)
;; Maybe enable it for css with (add-hook 'css-mode-hook  'emmet-mode)
;; For some shortcuts
(require-package 'emmet-mode)

;; Fuzzy file finder ignores/setup
(setq fiplr-ignored-globs '((directories (".git" ".svn" "*vim"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))
(global-set-key (kbd "<f5>") 'fiplr-reload-list)

;; Yasnippet
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        "~/.emacs.d/elpa/yasnippet-20150811.1222/snippets"))
(yas-global-mode 1)

;; Delimiters (autopair)
;; (autopair-global-mode t)

;; Magit something the plugin told me, have no idea
(setq magit-last-seen-setup-instructions "1.4.0")

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; Helm
(setq helm-buffers-fuzzy-matching t)
(global-set-key (kbd "M-x" ) 'helm-M-x)

;; Load up rainbow delimiters/paredit when writing el
(defun enable-parainbow ()
  (paredit-mode)
  (rainbow-delimiters-mode))

(add-hook 'emacs-lisp-mode-hook #'enable-parainbow)
(add-hook 'clojure-mode-hook #'enable-parainbow)

(provide 'init-plugins)
