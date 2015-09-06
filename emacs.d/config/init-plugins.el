;; Fuzzy file finder ignores/setup
(setq fiplr-ignored-globs '((directories (".git" ".svn" "*vim"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))
(global-set-key (kbd "<f5>") 'fiplr-reload-list)

;; Yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        "~/.emacs.d/elpa/yasnippet-20150811.1222/snippets"))
(yas-global-mode 1)

;; Delimiters
(require 'autopair)
(autopair-global-mode t)

;; Magit something the plugin told me, have no idea
(setq magit-last-seen-setup-instructions "1.4.0")

(provide 'init-plugins)
