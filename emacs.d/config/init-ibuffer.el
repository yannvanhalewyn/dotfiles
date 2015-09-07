(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("dired" (mode . dired-mode))
	       ("sheet_bucket" (filename . "reactjs/sheet_something/"))
	       ("el" (mode . emacs-lisp-mode))
	       ("code" (or
			(mode . c-mode)
			(mode . ruby-mode)
			(mode .java-mode)
			(mode . js-mode)))
	       ("planner" (or
			   (mode . org-mode)))
	       ("emacs" (or
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Messages\\*$")
			 (name . "^\\*Completions\\*$")))
	       ("git" (name ."^\\*magit"))))))

(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

(provide 'init-ibuffer)
