;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:

(setq doom-font (font-spec :family "MonaspiceKr Nerd Font Mono")
      doom-variable-pitch-font (font-spec :family "MonaspiceKr Nerd Font Mono" :weight 'bold))

(defun bronen/font-size-update (&optional _)
  (interactive)
  (let* ((geom (frame-monitor-geometry))
         (right (nth 2 geom)))
    (cond
     ((< right 1000)
      (set-face-attribute 'default nil :height 100)
      (set-face-attribute 'variable-pitch nil :height 120))
     ((< right 2000)
      (set-face-attribute 'default nil :height 150)
      (set-face-attribute 'variable-pitch nil :height 170))
     ((< right 3000)
      (set-face-attribute 'default nil :height 200)
      (set-face-attribute 'variable-pitch nil :height 220))
     (t
      (set-face-attribute 'default nil :height 250)
      (set-face-attribute 'variable-pitch nil :height 270)))))

(add-hook 'after-make-frame-functions #'bronen/font-size-update)
(add-hook 'window-size-change-functions #'bronen/font-size-update)
(add-hook 'after-setting-font-hook #'bronen/font-size-update)

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-flatwhite)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 't)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq projectile-project-search-path '("~/github/" "~/gitlab/"))

(use-package! clojure-mode
  :config
  (setq clojure-indent-style 'align-arguments
        clojure-align-forms-automatically t
        clojure-toplevel-inside-comment-form t))

(use-package! cider
  :after clojure-mode

  :config
  (setq cider-show-error-buffer t
        cider-font-lock-dynamically nil
        cider-eldoc-display-for-symbol-at-point nil
        cider-prompt-for-symbol nil
        cider-use-xref nil
        cider-repl-pop-to-buffer-on-connect 'display-only
        cider-repl-display-in-current-window nil
        cider-repl-result-prefix ";; => "
        clojure-enable-kaocha-runner t
        cider-repl-display-help-banner nil
        cider-print-fn 'puget
        cider-result-overlay-position 'at-point
        cider-overlays-use-font-lock t
        cider-repl-buffer-size-limit 200
        cider-repl-history-size 64)

  (set-lookup-handlers! '(cider-mode cider-repl-mode) nil)
  (set-popup-rule! "*cider-test-report*" :ignore t)
  (set-popup-rule! "^\\*cider-repl" :ignore t)

  ;; (add-hook 'cider-mode-hook
  ;;           (lambda ()
  ;;             (remove-hook 'completion-at-point-functions
  ;;                          #'cider-complete-at-point)))
  )

(use-package! zoom
  :hook   (doom-first-input . zoom-mode)
  :init   (map! "C->" #'mc/mark-next-like-this
                "C-<" #'mc/mark-previous-like-this)
  :config (setq zoom-size '(0.75)))

(use-package! drag-stuff
  :defer t
  :init  (map! "<M-up>"    #'drag-stuff-up
               "<M-down>"  #'drag-stuff-down
               "<M-left>"  #'drag-stuff-left
               "<M-right>" #'drag-stuff-right))

(use-package! wakatime-mode
  :defer 3
  :config (global-wakatime-mode))

(map! "M-p" #'+vertico/project-search
      "M-o" #'+vertico/switch-workspace-buffer
      "C-s" #'+vertico/search-symbol-at-point)

(after! kotlin-mode
  (setq flycheck-checkers '(kotlin-ktlint))
  (add-hook 'before-save-hook #'ktlint-fix nil t))

;; (after! lsp-mode
;;   (setq lsp-enable-symbol-highlighting nil
;;         lsp-enable-links nil
;;         lsp-enable-snippet nil
;;         lsp-enable-folding nil
;;         lsp-enable-on-type-formatting nil
;;         lsp-enable-indentation nil
;;         lsp-signature-auto-activate nil
;;         lsp-semantic-tokens-enable t
;;         lsp-file-watch-threshold 2000
;;         lsp-enable-file-watchers t))

(add-to-list 'auto-mode-alist '("\.qnt\'" . quint-mode))

(use-package! lsp-quint
  :commands (lsp-quint)
  :init (require 'lsp-quint))

(add-hook 'quint-mode-hook #'lsp!)

(use-package! lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil))

(after! svelte-mode
  (setq svelte-basic-offset 4))

(setq-default js-indent-level 4
	      typescript-indent-level 4
	      css-indent-offset 4)

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)

;; (use-package! lsp-vtsls
;;   :after lsp-mode
;;   :config (setq lsp-eldoc-render-all t
;; 		;; https://github.com/yioneko/vtsls#bad-performance-of-completion
;; 		lsp-vtsls-server-side-fuzzy-match t
;; 		lsp-vtsls-entries-limit 10))

(after! rust-mode
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode)))

;; (use-package! jinx
;;  :hook ((text-mode . jinx-mode)
;;         (markdown-mode . jinx-mode)
;;         (org-mode . jinx-mode)
;;         (git-commit-mode . jinx-mode))
;;  :config (map! :leader
;;		:desc "Correct spelling" "s c" #'jinx-correct))

(defun bronen/load-agda-mode ()
  (interactive)
  (when (and (stringp buffer-file-name)
             (string-match "\\.agda\\'" buffer-file-name)
             (executable-find "agda-mode"))
    (load-file (let ((coding-system-for-read 'utf-8))
                 (shell-command-to-string "agda-mode locate")))
    (agda2-mode)))

(add-to-list 'auto-mode-alist '("\\.astro\\'" . web-mode))

(after! typescript-mode
  (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode)))

;; literate babel org mode setup

(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(add-to-list 'org-babel-tangle-lang-exts '("lean4" . "lean"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:lean4 '())

(defun org-babel-expand-body:lean4 (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'inf-lean4 nil t)
  (let ((vars (org-babel--get-vars (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s=%S"
                (car pair) (org-babel-lean4-var-to-lean4 (cdr pair))))
      vars "\n")
     "\n" body "\n")))

(defun org-babel-execute:lean4 (body params)
  "Execute a block of lean4 code with org-babel.
This function is called by `org-babel-execute-src-block'"
    (let ((in-file (org-babel-temp-file "l" ".lean"))
          (verbosity (or (cdr (assq :verbosity params)) 0)))
      (with-temp-file in-file
        (insert body))
      (org-babel-eval
       (format "lean %s" (org-babel-process-file-name in-file))
       "")))

(defun org-babel-prep-session:lean4 (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-lean4-var-to-lean4 (var)
  "Convert an elisp var into a string of lean4 source code
specifying a var of the same value."
  (format "%S" var))

(defun org-babel-lean4-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

(defun org-babel-lean4-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")
    ))

(setq org-support-shift-select 't)

;; Quint Mode

(defconst quint-types '("int" "str" "bool" "Set" "List"))
(defconst quint-keywords '("Rec" "Tup" "not" "and" "or" "iff" "implies" "all" "any" "if" "else"))
(defconst quint-declarations '("module" "import" "from" "export" "as" "const" "var" "def" "val" "pure"
                               "nondet" "action" "temporal" "assume" "type"))
(defconst quint-constants '("Bool" "Int" "Nat" "false" "true"))

(defvar quint-font-lock-keywords
  (append
   `((,(regexp-opt quint-keywords 'symbols) . font-lock-keyword-face)
     (,(regexp-opt quint-declarations 'symbols) . font-lock-keyword-face)
     (,(regexp-opt quint-types 'symbols) . font-lock-type-face)
     (,(regexp-opt quint-constants 'symbols) . font-lock-constant-face))

   '(("\\<\\(?:def\\|action\\|run\\|temporal\\|pure\\|val\\)\\>\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-function-name-face)
     ("\\<module\\>\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-constant-face))))

(defvar quint-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; C-style comments: // and /* ... */
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for `quint-mode'.")

;;;###autoload
(define-derived-mode quint-mode prog-mode "Quint"
  "Major mode for the Quint specification language. https://github.com/informalsystems/quint/"
  :syntax-table quint-mode-syntax-table
  (setq-local font-lock-defaults '(quint-font-lock-keywords))
  (setq-local comment-start "/* ")
  (setq-local comment-end "*/ "))

(defun bronen/close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (delete (current-buffer) (buffer-list))))
