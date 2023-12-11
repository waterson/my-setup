;; -*- mode: lisp -*-
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; To update packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(require 'dash)
(require 's)
(require 'f)
(require 'lsp-mode)

(setq-default fill-column 78)
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "C-c m") 'compile)
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Programming modes should clean up trailing whitespace.
(add-hook
 'before-save-hook
 (lambda ()
   (when (derived-mode-p 'prog-mode)
     (delete-trailing-whitespace))))

;; Org-mode stuff
(require 'ob-eshell nil t)
(setq org-adapt-indentation t)
(setq org-babel-default-header-args:emacs-lisp '(("lexical" . t)))
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

;;; PlantUML source blocks.
(setq org-plantuml-jar-path "/opt/homebrew/opt/plantuml/libexec/plantuml.jar")
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t) (sql . t)))

(require 'request)
(setq request-curl-options (list "--insecure"))
(setq request-log-level 'debug)

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md'\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'turn-on-flyspell)
;;(add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)
;; (when (require 'visual-fill-column nil t)
;;   (add-hook 'markdown-mode-hook 'turn-on-visual-fill-column-mode))

;; Keys
(global-set-key (kbd "<f6>") 'menu-bar-open)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

(add-hook 'rust-mode-hook (lambda () (setq tab-width 2)))
(add-hook 'rust-mode-hook 'lsp)
(add-hook 'rust-mode-hook 'flyspell-prog-mode)
(setq rust-format-on-save t)

(setq display-time-day-and-date t)
(display-time)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(csv-separators '("\11"))
 '(custom-safe-themes
   '("a939897b56010ef16d737b3a145ab3f935e0da6122ded3bf9cad28f88b09fd68" "b552c7afafa8bfa9cada9d1ccae15214f85ad8f60cdacfabcdf48cd6237a8a37" "f33d0e919e55be4c574b2f67d67b8c2f1b7c069eb1e0b52d62190e63344bc36c" "c13654d93696165281861faab8b078493d1c989c9978baa3120fa6fc9fd33b2e" "2c38137c4b7310af7b68910f19d83f94df4b2f7db3d0532ee6284a014132c063" "159af9f37e70cb7ad8dc0bb2fb210ed2f7ed7a5c916f8c20298f3b1e942c44d1" "24d83300e80ea0939b33ab7b1cfe364873831dd2de977044d5d3c542b603d40a" "13011b7a862703c3595cd2679708968e0ceb832592a7a99f073e30073bbe54da" "904ccc456f6be7860252f4bf47dab4bbe684328a925749a3cd11fab8faf4d8d0" "65f2186ebcfc320cdf1ec645f5587a8c70db539cfb748f1e53eaf736c94422fe" "f86038f1ca9917a3bb9eddbfcb03d72d4ad07d533ba9a083d29630fcf4393993" "deecabf4a71b8dc231a0df31ad2a73719e37d419c67a47cc95b2338a57fcd5fc" "3f6d8873ccf85cc02883e5c9f2322fc030537d07e0beee4c6847052458820335" "a8f2041ae258fe7522910b091a7c8fc48c4ca79364a7d9bfc8418133a6459613" "2a5dcf895b5dcfc4b145a15b0c78ef4ac2a72b9862a6d991d7933f4e789e63ea" "cdf658cbde86131ddd3c4d68da9c3e415f74166c3105ce84b724182c75e083cc" "3982648f6a386905f586ccb7423d65b1ae62895f2b0a2a1eb3b0631ed3bd5d89" "d449e57bad1b6c54aa233e0ad8c4f81896157a663308bf9a6dd092a34ac0bbe3" "ff4fa5fbe72d7c14f0b14b1df190bda2f21b66b2124190b8d86798e0dbb2338c" "d5d7a06c237d48a4e62f651b98241d12837c24bdce705bef40bb870e8aaf2ce3" "a71f09694083d8d8b9be6cf6e84a99813312d60f84c9f2e902308a4a7f411a2a" "b763ab51d5268b8e678eeb4a82f0ea2481ccb65515171a8e2aebbfb12e82d535" default))
 '(eshell-banner-message "")
 '(font-use-system-font t)
 '(ignored-local-variable-values '((rust-basic-offset . 2)))
 '(menu-bar-mode nil)
 '(org-agenda-files
   '("~/work/notes/ci.org" "/Users/waterson/work/notes/hermetic.org"))
 '(org-babel-load-languages '((shell . t) (emacs-lisp . t) (dot . t)))
 '(package-selected-packages
   '(csv-mode clang-format visual-fill-column flymake-diagnostic-at-point go-mode typescript-mode emacsql-pg protobuf-mode dockerfile-mode yaml-mode request-deferred blacken rust-mode autothemer web-mode request prettier lsp-mode))
 '(safe-local-variable-values '((delete-trailing-whitespace)))
 '(typescript-indent-level 2))

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
(setq gofmt-args '("-s"))
(add-hook 'go-mode-hook 'flyspell-prog-mode)
(add-hook 'go-mode-hook 'lsp)
(add-hook 'go-mode-hook (lambda () (setq tab-width 2)))
(add-hook 'go-mode-hook (lambda () (add-hook 'before-save-hook 'gofmt nil t)))

;; XXX might still need to load prettier-js into typescript mode?
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook
 'web-mode-hook
 (lambda ()
   (prettier-mode t)
   (setq web-mode-code-indent-offset 2)
   (setq web-mode-markup-indent-offset 2)
   (setq web-mode-css-indent-offset 2)))

;; Blacken model reformats python on save.
(add-hook 'python-mode-hook 'blacken-mode)
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-hook 'python-mode-hook 'lsp)
(add-hook
 'python-mode-hook
 (lambda ()
   (setq-local lsp-enable-snippt nil)))
(add-hook
 'python-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c l") 'my-pylint-run)))

;; Shell echoes.
(add-hook
 'comint-mode-hook
 (lambda ()
   (setq comint-process-echoes t)))

;; C++
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)
(add-hook
 'c-mode-common-hook
 (lambda ()
   (add-hook
    'before-save-hook
    (lambda ()
      (when (locate-dominating-file "." ".clang-format")
        (clang-format-buffer))
      ;; Continue to save.
      nil)
    nil
    ;; Buffer local hook.
    t)))

;; Start server so we can use emacsclient (to ourself) as the EDITOR
;; for stuff like git.
(require 'server)
(setq server-name (format "emacs-server-%d" (emacs-pid)))
(setenv "EDITOR" (concat "emacsclient -s " server-name))
(server-start)

(require 'vc-git)
(setq
 eshell-prompt-function
 (lambda ()
   (let (;; (kube-context (kubernetes-context))
         (virtual-env-name (-when-let (path (getenv "VIRTUAL_ENV"))
                             (--> path
                                  (split-string it "/")
                                  (reverse it)
                                  (car it)
                                  (replace-regexp-in-string "-[a-z0-9]+$" "" it))))
         (git-branch (car (vc-git-branches))))
     (concat
      (if virtual-env-name (concat "(" virtual-env-name ") ") "")
      ;; (if kube-context (concat "<" kube-context ">") "")
      (if git-branch (concat "[" git-branch "]") "")
      (if (or virtual-env-name git-branch) " " "")
      (abbreviate-file-name (eshell/pwd))
      (if (= (user-uid) 0) " # " " $ ")))))

(put 'set-goal-column 'disabled nil)

;; Fix python-mode's pylint. Ideally the custom-set-variables would be
;; a bit more nuanced e.g. to only set it when we're in sparktown.


(defun my-pylint-run ()
  (interactive)
  (let ((pylint-buffer-name "*Pylint*")
        (filename (buffer-file-name)))
    (set-buffer (get-buffer-create pylint-buffer-name))
    (erase-buffer)
    (async-shell-command (format "pylint.sh %s" filename) pylint-buffer-name)
    (pop-to-buffer pylint-buffer-name)))

(add-hook
 'python-mode-hook
 (lambda ()
   (fset 'py-pylint-run 'fixed-py-pylint-run)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(with-eval-after-load 'flymake
  (require 'flymake-diagnostic-at-point)
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))
(require 'typescript-mode)
(require 'prettier-js)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
