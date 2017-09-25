(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("code" (or
                        (mode . clojure-mode)
                        (mode . clojurec-mode)
			(mode . c-mode)
			(mode . ruby-mode)
                        (mode . javascript-mode)
			(mode . java-mode)
			(mode . js-mode)
                        (mode . coffee-mode)
                        (mode . clojurescript-mode)))
	       ("configs" (or
                           (mode . emacs-lisp-mode)
                           (mode . org-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")
                         (name . "^\\*Completions\\*$")))
               ("Magit" (name . "\*magit"))
               ("Help" (or (name . "\*Help\*")
                           (name . "\*Apropos\*")
                           (name . "\*info\*")))
               ("tmp" (or (mode . dired-mode)
                          (name ."^\\*")))))))

(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

(provide 'init-ibuffer)
