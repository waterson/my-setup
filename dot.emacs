;; -*- mode: lisp -*-

(require 'dash)
(require 'dash-functional)
(require 's)

(setq-default indent-tabs-mode nil)
(global-set-key (kbd "C-c m") 'compile)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("b763ab51d5268b8e678eeb4a82f0ea2481ccb65515171a8e2aebbfb12e82d535" default)))
 '(eshell-banner-message "")
 '(font-use-system-font t)
 '(menu-bar-mode nil)
 '(org-babel-load-languages (quote ((shell . t) (emacs-lisp . t))))
 '(safe-local-variable-values (quote ((delete-trailing-whitespace)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
