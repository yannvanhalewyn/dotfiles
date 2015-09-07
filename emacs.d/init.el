;                          .         .
; 8 8888888888            ,8.       ,8.                   .8.           ,o888888o.       d888888o.
; 8 8888                 ,888.     ,888.                 .888.         8888     `88.   .`8888:' `88.
; 8 8888                .`8888.   .`8888.               :88888.     ,8 8888       `8.  8.`8888.   Y8
; 8 8888               ,8.`8888. ,8.`8888.             . `88888.    88 8888            `8.`8888.
; 8 888888888888      ,8'8.`8888,8^8.`8888.           .8. `88888.   88 8888             `8.`8888.
; 8 8888             ,8' `8.`8888' `8.`8888.         .8`8. `88888.  88 8888              `8.`8888.
; 8 8888            ,8'   `8.`88'   `8.`8888.       .8' `8. `88888. 88 8888               `8.`8888.
; 8 8888           ,8'     `8.`'     `8.`8888.     .8'   `8. `88888.`8 8888       .8' 8b   `8.`8888.
; 8 8888          ,8'       `8        `8.`8888.   .888888888. `88888.  8888     ,88'  `8b.  ;8.`8888
; 8 888888888888 ,8'         `         `8.`8888. .8'       `8. `88888.  `8888888P'     `Y8888P ,88P'
;
(add-to-list 'load-path (concat user-emacs-directory "config"))

(require 'general-behavior)
(require 'project-root)
(require 'init-packages)
(require 'init-evil)
(require 'init-plugins)
(require 'init-compile)
(require 'init-ibuffer)
(require 'init-dired)
(require 'layout)
(require 'some-funcs)
(require 'mocha)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "a444b2e10bedc64e4c7f312a737271f9a2f2542c67caa13b04d525196562bf38" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
