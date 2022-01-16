;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Claudio Caseiro"
      user-mail-address "ccaseiro@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Operator Mono Lig" :size 14 :weight 'book :style 'book))
;; doom-variable-pitch-font (font-spec :family "Operator Mono" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/Notes")
;; (setq org-agenda-files '("~/Documents/Notes"))
(setq org-agenda-files '("~/Documents/Notes" "~/Documents/Notes/journal" "~/Documents/Notes/roam" "~/Documents/Notes/roam/daily"))
;; (add-to-list 'org-agenda-files org-journal-dir)

(add-to-list 'org-modules 'org-habit)
(defun cc/toggle-org-habit-show-habits-only-for-today (arg)
  "toggle org-habit-show-habits-only-for-today"
  (interactive "P")
  (setq org-habit-show-habits-only-for-today (not org-habit-show-habits-only-for-today)))
(map! :leader
      :desc "show habits only for today"
      "t h" #'cc/toggle-org-habit-show-habits-only-for-today)
;; (global-set-key (kbd "<SPC> t h") (lambda () (setq org-habit-show-habits-only-for-today (not org-habit-show-habits-only-for-today))))
;; Automatically adds the current and all future journal entries to the agenda.
;; It's a performance compromise
;; (setq org-journal-enable-agenda-integration t)

(setq org-journal-date-prefix "#+TITLE: "
      org-journal-time-prefix "* "
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-file-format "%Y-%m-%d.org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(setq
 projectile-project-search-path '("~/Developer"))


(setq org-roam-directory "~/Documents/Notes/roam")

;;; -----------------------------------------------------------------------
;;; customize org-roam title in agenda view (instead of showing file name)
;;; reference: https://d12frosted.io/posts/2020-06-24-task-management-with-roam-vol2.html
;;; -----------------------------------------------------------------------
(setq org-agenda-prefix-format
      '((agenda . " %i %(vulpea-agenda-category 12)%?-12t% s")
        (todo . " %i %(vulpea-agenda-category 12) ")
        (tags . " %i %(vulpea-agenda-category 12) ")
        (search . " %i %(vaulpea-agenda-category 12) ")))

(defun vulpea-agenda-category (&optional len)
  "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (vulpea-buffer-prop-get "title"))
         (category (org-get-category))
         (result
          (or (if (and
                   title
                   (string-equal category file-name))
                  title
                category)
              "")))
    (if (numberp len)
        (s-truncate len (s-pad-right len " " result))
      result)))

(defun vulpea-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                             (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

;;; -----------------------------------------------------------------------
;;; Python
;;; -----------------------------------------------------------------------
(setq dap-python-debugger 'debugpy
      dap-python-executable "python3")

;; (add-hook! python-mode 'anaconda-mode)
;; (add-hook! vterm-mode 'anaconda-mode)
;; (add-hook! python-mode 'anaconda-eldoc-mode)


;; (setq +evil-collection-disabled-list (remove 'anaconda-mode +evil-collection-disabled-list))

;; HACK: fix python f-strings + smartparens
(after! smartparens
  (sp-local-pair '(python-mode) "f\"" "\"")
  (sp-local-pair '(python-mode) "f'" "'"))

(defun pyvenv-autoload ()
          (interactive)
          "auto activate venv directory if exists"
          (f-traverse-upwards (lambda (path)
              (let ((venv-path (f-expand "venv" path)))
              (when (f-exists? venv-path)
              (pyvenv-activate venv-path))))))

(add-hook! python-mode 'pyvenv-autoload)

(setq consult-imenu-config '(
        (emacs-lisp-mode :toplevel "Functions" :types
                  ((102 "Functions" font-lock-function-name-face)
                   (109 "Macros" font-lock-function-name-face)
                   (112 "Packages" font-lock-constant-face)
                   (116 "Types" font-lock-type-face)
                   (118 "Variables" font-lock-variable-name-face)))
        (python-mode :toplevel "Packages" :types
                  ((102 "Functions" font-lock-function-name-face)
                   (?s "Sections" font-lock-function-name-face)
                   (109 "Macros" font-lock-function-name-face)
                   (112 "Packages" font-lock-constant-face)
                   (116 "Types" font-lock-type-face)
                   (118 "Variables" font-lock-variable-name-face)))
        )
      )

;; (defun my-merge-imenu ()
;;   (interactive)
;;   (let ((mode-imenu (lsp--imenu-create-index))
;;         (custom-imenu (imenu--generic-function imenu-generic-expression)))
;;     (append mode-imenu custom-imenu)))


;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (add-to-list
;;              'imenu-generic-expression
;;              '("Sections" "^def \\(.*\\)" 1))
;;              ;; '("Sections" "^#### \\[ \\(.*\\) \\]$" 1))
;;             ;; (imenu-add-to-menubar "Position")
;;             (setq lsp--imenu-create-index 'my-merge-imenu)
;;             (setq lsp-imenu-create-uncategorized-index 'my-merge-imenu)))

;; (defun my-merge-imenu ()
;;   (interactive)
;;   (let ((mode-imenu (imenu-default-create-index-function))
;;         (custom-imenu (imenu--generic-function imenu-generic-expression)))
;;     (append mode-imenu custom-imenu)))


;; (add-hook! python-mode
;;           (lambda ()
;;             (add-to-list
;;              'imenu-generic-expression
;;              '("Section" "^def \\(.*\\)" 1))
;;              ;; '("Sections" "^#### \\[ \\(.*\\) \\]$" 1))
;;             ;; (imenu-add-to-menubar "Position")
;;             (setq imenu-create-index-function 'my-merge-imenu)))


;; (setq python-consult
;; `(:name "Projectile projects"
;;         :narrow   ?P
;;         :category project
;;         :action   ,#'projectile-switch-project-by-name
;;         :items    ,projectile-known-projects))
;; (add-to-list 'consult-buffer-sources my-consult-source-projectile-projects 'append)


;;; -----------------------------------------------------------------------
;;; Terraform
;;; -----------------------------------------------------------------------

(setq lsp-terraform-server '("terraform-ls" "serve"))
;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection '("terraform-ls" "serve"))
;;                   :major-modes '(terraform-mode)
;;                   :server-id 'terraform-ls))

;; (add-hook 'terraform-mode-hook #'lspd)

;;; -----------------------------------------------------------------------
;;; Magit
;;; -----------------------------------------------------------------------
(setq magit-wip-mode t)

;;; -----------------------------------------------------------------------
;;; Keybindings
;;; -----------------------------------------------------------------------

;; by default doom has those 2 keybindings swaped (better in n"norman" keyboards)
(map! :map evil-normal-state-map "C-+" 'text-scale-increase)
(map! :map evil-normal-state-map "C-=" 'doom/reset-font-size)

;;; -----------------------------------------------------------------------
;;; git gutter
;;; -----------------------------------------------------------------------
(setq +vc-gutter-default-style nil)

;;; -----------------------------------------------------------------------
;;; ophints: evil-goggles
;;; -----------------------------------------------------------------------
(after! evil-goggles
        (evil-goggles-use-diff-refine-faces))

;;; -----------------------------------------------------------------------
;;; Unity
;;; -----------------------------------------------------------------------
(setenv "FrameworkPathOverride" "/usr/local/lib/mono/4.8-api")

;;; -----------------------------------------------------------------------
;;; Misc
;;; -----------------------------------------------------------------------
;; (after! terraform (set-company-backend! 'company-files))
(add-hook! terraform-mode (set-company-backend! 'company-files))

(setq +file-templates-dir (concat doom-private-dir "templates/"))

;; treat '_' as part of the word (see: https://evil.readthedocs.io/en/latest/faq.html#underscore-is-not-a-word-character)
(defalias 'forward-evil-word 'forward-evil-symbol)


(map! :map evil-normal-state-map "z e" 'hs-hide-level)

;; Add some Leader Keybindings
(map! :leader "w a" 'ace-window)
(map! :leader "w f" 'ace-swap-window)

;; Swap leader-: with leader-;
(map! :leader ":" 'pp-eval-expression
      :leader ";" 'execute-extended-command)
