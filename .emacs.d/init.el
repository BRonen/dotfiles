(setq-default tab-width 4
	      indent-tabs-mode nil
	      indent-line-function 'insert-tab)

(require 'use-package)
(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)

(package-initialize)

(use-package use-package
  :ensure nil
  :config
  (setq use-package-verbose nil
        use-package-expand-minimally t
        use-package-always-ensure t
        use-package-compute-statistics nil))

(use-package magit
	     :init (message "Loading Magit!")
	     :config (message "Loaded Magit!")
	     :bind ("C-x C-g" . magit-status))

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
	     :hook ((clojure-mode-hook       . lsp)
		    (clojurescript-mode-hook . lsp)
		    (clojurec-mode-hook      . lsp)
		    ;; (lsp-mode . lsp-enable-which-key-integration)
		    )
	     :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package treemacs
  :commands (treemacs treemacs-select-window)
  :hook (treemacs-mode . treemacs-project-follow-mode)
  :bind (("M-J" . treemacs-find-file))
  :config
  (setq treemacs-follow-after-init t
        treemacs-collapse-dirs 1
        treemacs-directory-name-transformer #'identity
        treemacs-file-name-transformer #'identity
        treemacs-show-cursor nil
        treemacs-display-current-project-exclusively t
        treemacs-filewatch-mode t
        treemacs-follow-mode nil
        treemacs-hide-dot-git-directory t
        treemacs-git-integration t
        treemacs-space-between-root-nodes t
        treemacs-hide-gitignored-files-mode t
        treemacs-git-mode 'extended
        treemacs-indentation 1
        treemacs-silent-refresh	t
        treemacs-sorting 'treemacs--sort-alphabetic-case-insensitive-asc
        treemacs-width 32))

(use-package treemacs-magit
  :after treemacs magit)

(use-package yaml-mode
  :commands (yaml-mode))

 (use-package json-mode
   :commands (json-mode))

(use-package catppuccin-theme)
(load-theme 'catppuccin :no-confirm)

(use-package direnv
 :config
 (direnv-mode))

(use-package xclip)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-enable-indentation nil
      lsp-enable-completion-at-point nil)

(global-linum-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(direnv catppuccin-theme json-mode yaml-mode treemacs-magit lsp-treemacs lsp-ui xclip use-package magit cider))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
