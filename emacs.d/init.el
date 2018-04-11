;;                          .         .
;; 8 8888888888            ,8.       ,8.                   .8.           ,o888888o.       d888888o.
;; 8 8888                 ,888.     ,888.                 .888.         8888     `88.   .`8888:' `88.
;; 8 8888                .`8888.   .`8888.               :88888.     ,8 8888       `8.  8.`8888.   Y8
;; 8 8888               ,8.`8888. ,8.`8888.             . `88888.    88 8888            `8.`8888.
;; 8 888888888888      ,8'8.`8888,8^8.`8888.           .8. `88888.   88 8888             `8.`8888.
;; 8 8888             ,8' `8.`8888' `8.`8888.         .8`8. `88888.  88 8888              `8.`8888.
;; 8 8888            ,8'   `8.`88'   `8.`8888.       .8' `8. `88888. 88 8888               `8.`8888.
;; 8 8888           ,8'     `8.`'     `8.`8888.     .8'   `8. `88888.`8 8888       .8' 8b   `8.`8888.
;; 8 8888          ,8'       `8        `8.`8888.   .888888888. `88888.  8888     ,88'  `8b.  ;8.`8888
;; 8 888888888888 ,8'         `         `8.`8888. .8'       `8. `88888.  `8888888P'     `Y8888P ,88P'
;;


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "config"))
(setq custom-file "~/.emacs-custom.el")
(load custom-file)

(require 'init-use-package)
(require 'general-behavior)
(require 'init-functions)
(require 'init-packages)
(require 'init-ibuffer)
(require 'init-dired)
(require 'layout)
