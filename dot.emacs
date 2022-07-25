;; -*- mode: lisp -*-
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'dash)
(require 'dash-functional)
(require 's)

(setq-default indent-tabs-mode nil)
(global-set-key (kbd "C-c m") 'compile)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Org-mode stuff
(require 'ob-eshell nil t)
(setq org-adapt-indentation t)
(setq org-confirm-babel-evaluate nil)
(setq org-export-use-babel nil)
(setq org-export-with-broken-links t)
(setq org-export-with-sub-superscripts nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-startup-truncated nil)
(setq org-html-table-caption-above nil)
(setq org-entities-user
      '(("btc" "?" nil "&#x20bf" "BTC" "BTC" "BTC")))
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md'\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'turn-on-flyspell)
(add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)
(when (require 'visual-fill-column nil t)
  (add-hook 'markdown-mode-hook 'turn-on-visual-fill-column-mode))

;; Keys
(global-set-key (kbd "<f6>") 'menu-bar-open)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

(setq display-time-day-and-date t)
(display-time)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   '("f86038f1ca9917a3bb9eddbfcb03d72d4ad07d533ba9a083d29630fcf4393993" "deecabf4a71b8dc231a0df31ad2a73719e37d419c67a47cc95b2338a57fcd5fc" "3f6d8873ccf85cc02883e5c9f2322fc030537d07e0beee4c6847052458820335" "a8f2041ae258fe7522910b091a7c8fc48c4ca79364a7d9bfc8418133a6459613" "2a5dcf895b5dcfc4b145a15b0c78ef4ac2a72b9862a6d991d7933f4e789e63ea" "cdf658cbde86131ddd3c4d68da9c3e415f74166c3105ce84b724182c75e083cc" "3982648f6a386905f586ccb7423d65b1ae62895f2b0a2a1eb3b0631ed3bd5d89" "d449e57bad1b6c54aa233e0ad8c4f81896157a663308bf9a6dd092a34ac0bbe3" "ff4fa5fbe72d7c14f0b14b1df190bda2f21b66b2124190b8d86798e0dbb2338c" "d5d7a06c237d48a4e62f651b98241d12837c24bdce705bef40bb870e8aaf2ce3" "a71f09694083d8d8b9be6cf6e84a99813312d60f84c9f2e902308a4a7f411a2a" "b763ab51d5268b8e678eeb4a82f0ea2481ccb65515171a8e2aebbfb12e82d535" default))
 '(eshell-banner-message "")
 '(font-use-system-font t)
 '(menu-bar-mode nil)
 '(org-babel-load-languages '((shell . t) (emacs-lisp . t)))
 '(package-selected-packages
   '(kubernetes magit protobuf-mode visual-fill-column markdown-mode dockerfile-mode go-dlv request deferred pylint yaml-mode blacken typescript-mode prettier-js prettier go-mode xclip osc s dash))
 '(safe-local-variable-values '((delete-trailing-whitespace)))
 '(typescript-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(unless window-system
  (xterm-mouse-mode t))

;; Fix copy paste to work with native clipboard This stuff is specific
;; to running inside OS/X. Probably should be conditioned on that.
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; Allow narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; Golang tabs to 2
(add-hook 'go-mode-hook (lambda () (setq tab-width 2)))

;; XXX might still need to load prettier-js into typescript mode?
(require 'typescript-mode)
(require 'prettier-js)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;; Blacken model reformats python on save.
(add-hook 'python-mode-hook 'blacken-mode)
(add-hook
 'python-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c l") 'pylint)
   (setq-local pylint-command (concat (getenv "HOME") "/bin/pylint.sh"))))

;; Shell echoes.
(add-hook
 'comint-mode-hook
 (lambda ()
   (setq comint-process-echoes t)))

;; Start server so we can use emacsclient (to ourself) as the EDITOR
;; for stuff like git.
(require 'server)
(setq server-name (format "emacs-server-%d" (emacs-pid))) 
(setenv "EDITOR" (concat "emacsclient -s " server-name))
(server-start)

(add-to-list 'load-path "~/work/src/elisp")
(require 'lightspark)


(-when-let (path (getenv "VIRTUAL_ENV"))
  (setq virtual-env-name (car (reverse (split-string path "/"))))
  (setq
   eshell-prompt-function
   (lambda ()
     (concat "(" virtual-env-name ") "
             (abbreviate-file-name (eshell/pwd))
             (if (= (user-uid) 0) " # " " $ ")))))
