(setq-default tab-width 4
	      indent-tabs-mode nil
	      indent-line-function 'insert-tab)

(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(use-package ivy
  :delight
  :bind (("C-s"     . swiper-isearch)
         ("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("C-x d"   . counsel-dired)
         ("M-y"     . counsel-yank-pop)
         ("C-x b"   . ivy-switch-buffer)
         ("C-c v"   . ivy-push-view)
         ("C-c V"   . ivy-pop-view)
         ("C-c k"   . counsel-rg)
         ("C-c b"   . counsel-bookmark)
         ("C-c d"   . counsel-descbinds)
         ("C-c o"   . counsel-outline))
  :config
  (ivy-mode 1)
  :custom
  (ivy-count-format "(%d/%d) "))

(use-package counsel)

(use-package multiple-cursors
  :bind (("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this)))

(use-package projectile
  :init
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/Github" "~/Gitlab"))
  :bind
  (:map projectile-mode-map ("M-p" . projectile-command-map)))

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-0"))
  :init
  (persp-mode))

(use-package persp-projectile)

(use-package magit
             :bind ("C-x C-g" . magit-status))

(use-package exec-path-from-shell
             :config (when (memq window-system '(mac ns x)) (exec-path-from-shell-initialize)))

(use-package rainbow-delimiters
             :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key :config (which-key-mode))

(use-package ace-window :bind ("M-o" . ace-window))

(use-package haskell-mode)

(use-package cider)

(use-package flycheck-clj-kondo)

(use-package clojure-mode
         :config (require 'flycheck-clj-kondo))

(use-package company
         :config (add-to-list 'company-backends 'company-capf)
         (global-company-mode))

(use-package flycheck :config (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package lsp-mode
         :init (setq lsp-keymap-prefix "C-c l")
         :hook ((clojure-mode-hook          . lsp)
            (clojurescript-mode-hook    . lsp)
            (clojurec-mode-hook         . lsp)
            (haskell-mode-hook          . lsp)
                    (haskell-literate-mode-hook . lsp)
            ;; (lsp-mode . lsp-enable-which-key-integration)
            )
         :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package git-gutter
  :config (global-git-gutter-mode +1))

(use-package nix-mode :mode "\\.nix\\'")

(use-package monokai-theme)
(load-theme 'monokai :no-confirm)

(use-package direnv
             :config (when (executable-find "direnv") (direnv-mode)))

(use-package xclip)

(use-package elcord)

(use-package pdf-tools
  :init
  (pdf-loader-install))

(use-package nov
  :init (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package wakatime-mode
  :config (global-wakatime-mode))

(defun load-idris2-mode
    ()
  "Function to load idris2-mode."
  (add-to-list 'load-path "~/.emacs.d/idris2-mode/")
  (require 'idris2-mode))

(when (file-directory-p "~/.emacs.d/idris2-mode/") (load-idris2-mode))

(defun load-agda-mode
    ()
  "Function to load agda-mode."
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))

(when (executable-find "agda") (load-agda-mode))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-minimum-prefix-length 1
      lsp-enable-indentation nil
      inhibit-startup-screen t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(put 'dired-find-alternate-file 'disabled nil)

(set-face-attribute 'default nil :font "Monospace-14")

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(recentf-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-babel-clojure-backend 'cider)
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (shell      . t)
     (python     . t)
     (sql        . t)
     (clojure    . t)
     (haskell    . t)
     (js         . t)
     (sqlite     . t)))
 '(org-confirm-babel-evaluate nil)
 '(org-support-shift-select t)
 '(wakatime-api-key "")
 '(wakatime-cli-path "wakatime-cli")
 '(warning-suppress-types '((comp)))
 '(whitespace-style
   '(face trailing tabs spaces lines newline empty indentation space-after-tab space-before-tab space-mark tab-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
