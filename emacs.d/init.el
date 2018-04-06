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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   (quote
    ("b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "a858b6e42bebab0a2dce72d0208f18fda03b94cc41420d766b85af6775a418c1" "25c06a000382b6239999582dfa2b81cc0649f3897b394a75ad5a670329600b45" "4bf5c18667c48f2979ead0f0bdaaa12c2b52014a6abaa38558a207a65caeb8ad" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "3f546f127564de59f1c42dfd65d32bd7650fa5a614cd58c2df061da052ea3ec0" "64f2981274fd8740b794bce9feee7949ed87b88fc0b4654bd98594e1aa81abcd" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "eae831de756bb480240479794e85f1da0789c6f2f7746e5cc999370bbc8d9c8a" "5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "8288b9b453cdd2398339a9fd0cec94105bc5ca79b86695bd7bf0381b1fbe8147" "75c0b9f9f90d95ac03f8647c75a91ec68437c12ff598e2abb22418cd4b255af0" "8ffaf449297bd9a08517f4b03a4df9dbf3e347652746cefceb3ee57c8e584b9f" "e8e744a1b0726814ac3ab86ad5ccdf658b9ff1c5a63c4dc23841007874044d4a" "b6db49cec08652adf1ff2341ce32c7303be313b0de38c621676122f255ee46db" "badc4f9ae3ee82a5ca711f3fd48c3f49ebe20e6303bba1912d4e2d19dd60ec98" "50e7f9d112e821e42bd2b8410d50de966c35c7434dec12ddea99cb05dd368dd8" "0b6645497e51d80eda1d337d6cabe31814d6c381e69491931a688836c16137ed" "ff02e8e37c9cfd192d6a0cb29054777f5254c17b1bf42023ba52b65e4307b76a" "f77b66fa762568d66fc00a5e2013aae76d78f0142669c55b7eb3c8e5d4d41e7d" "19352d62ea0395879be564fc36bc0b4780d9768a964d26dfae8aad218062858d" "5d1434865473463d79ee0523c1ae60ecb731ab8d134a2e6f25c17a2b497dd459" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "a444b2e10bedc64e4c7f312a737271f9a2f2542c67caa13b04d525196562bf38" default)))
 '(fci-rule-color "#151515")
 '(flycheck-javascript-flow-args nil)
 '(package-selected-packages
   (quote
    (evil-nerd-commenter darkokai-theme monokai-theme monokai spacemacs-theme dashboard emacs-dashboard company-mode glsl-mode flycheck-flow prettier-js jbeans-theme smyx-theme column-marker realgud smart-mode-line base16-theme evil-numbers evil-surround evil-commentary magit helm-projectile helm ag aggressive-indent yard-mode scss-mode sass-mode rainbow-delimiters general flycheck exec-path-from-shell evil-cleverparens company coffee-mode bundler ace-jump-mode)))
 '(safe-local-variable-values (quote ((no-byte-compile t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'init-use-package)
(require 'general-behavior)
(require 'init-functions)
(require 'init-packages)
(require 'init-ibuffer)
(require 'init-dired)
(require 'layout)
