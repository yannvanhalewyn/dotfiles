;; Define the package repos
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;; Init package manager
(require 'package)
(package-initialize)

;; Define all required packages
(defvar required-packages '(evil evil-leader zenburn-theme neotree magit
                            autopair powerline evil-surround fiplr flycheck
			    gruvbox-theme yasnippet))

;; Utitities (common lisp)
(require 'cl)

;; Function to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; Check if all packages are installed
(unless (packages-installed-p)
  ;; If not update packages db
  (message "%s" "Refreshing package database")
  (package-refresh-contents)
  (message "%s" "Done!")

  ;; Install mising packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))
