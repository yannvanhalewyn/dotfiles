(require-package 'yasnippet)
(require-package 'autopair)
(require-package 'magit)
(require-package 'rainbow-delimiters)
(require-package 'helm)
(require-package 'helm-ls-git)

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
(autopair-global-mode t)

;; Magit something the plugin told me, have no idea
(setq magit-last-seen-setup-instructions "1.4.0")

;; Load up rainbow delimiters when writing el
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; Helm
(setq
 helm-buffers-fuzzy-matching t)

(provide 'init-plugins)
