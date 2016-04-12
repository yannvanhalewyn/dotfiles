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
(require 'init-use-package)
(require 'init-env)
(require 'init-evil)
(require 'init-plugins)
(require 'init-compile)
(require 'init-ibuffer)
(require 'init-dired)
(require 'init-ido)
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
    ("50e7f9d112e821e42bd2b8410d50de966c35c7434dec12ddea99cb05dd368dd8" "0b6645497e51d80eda1d337d6cabe31814d6c381e69491931a688836c16137ed" "ff02e8e37c9cfd192d6a0cb29054777f5254c17b1bf42023ba52b65e4307b76a" "f77b66fa762568d66fc00a5e2013aae76d78f0142669c55b7eb3c8e5d4d41e7d" "19352d62ea0395879be564fc36bc0b4780d9768a964d26dfae8aad218062858d" "5d1434865473463d79ee0523c1ae60ecb731ab8d134a2e6f25c17a2b497dd459" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "a444b2e10bedc64e4c7f312a737271f9a2f2542c67caa13b04d525196562bf38" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
