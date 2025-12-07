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

(pcase (getenv "USER")
  ("brenno.rodrigues"
   (setq doom-font (font-spec :family "MonaspiceKr Nerd Font Mono" :size 42)
         doom-variable-pitch-font (font-spec :family "MonaspiceKr Nerd Font Mono" :size 44 :weight 'bold)))
  (_
   (setq doom-font (font-spec :family "MonaspiceKr Nerd Font Mono" :size 22)
         doom-variable-pitch-font (font-spec :family "MonaspiceKr Nerd Font Mono" :size 24 :weight 'bold))))

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

(use-package zoom
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

(when (string= "bronen" (getenv "USER"))
  (use-package! elcord
    :defer  t
    :config (elcord-mode)))


(map! "M-p" #'+vertico/project-search
      "M-o" #'+vertico/switch-workspace-buffer
      "C-s" #'+vertico/search-symbol-at-point)

