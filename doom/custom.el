(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7de64ff2bb2f94d7679a7e9019e23c3bf1a6a04ba54341c36e7cf2d2e56e2bcc"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "72d9086e9e67a3e0e0e6ba26a1068b8b196e58a13ccaeff4bfe5ee6288175432"
     "3f24dd8f542f4aa8186a41d5770eb383f446d7228cd7a3413b9f5e0ec0d5f3c0"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
     "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36"
     "7c28419e963b04bf7ad14f3d8f6655c078de75e4944843ef9522dbecfcd8717d"
     "6a5584ee8de384f2d8b1a1c30ed5b8af1d00adcbdcd70ba1967898c265878acf"
     "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9"
     "e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40"
     "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d"
     "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1"
     "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e"
     "7758a8b8912ef92e8950a4df461a4d510484b11df0d7195a8a3d003965127abc"
     "c8c4baac2988652a760554e0e7ce11a0fe0f8468736be2b79355c9d9cc14b751"
     "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc"
     "6e18353d35efc18952c57d3c7ef966cad563dc65a2bba0660b951d990e23fc07"
     "113a135eb7a2ace6d9801469324f9f7624f8c696b72e3709feb7368b06ddaccc"
     "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554"
     "c8b3d9364302b16318e0f231981e94cbe4806cb5cde5732c3e5c3e05e1472434"
     "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad"
     "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478"
     "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun load-agda-mode ()
  (interactive)
  (when (and (stringp buffer-file-name)
             (string-match "\\.agda\\'" buffer-file-name)
             (executable-find "agda-mode"))
    (load-file (let ((coding-system-for-read 'utf-8))
                 (shell-command-to-string "agda-mode locate")))
    (agda2-mode)))

(add-to-list 'auto-mode-alist '("\\.astro\\'" . web-mode))

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
