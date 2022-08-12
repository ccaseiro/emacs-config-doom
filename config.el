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

;;; -----------------------------------
;;; Window
;;; -----------------------------------
(defun cc/window-top-2 ()
  "Move the cursor to center/top window"
  (interactive)
  (evil-window-top-left)
  (windmove-right))

(defun cc/middle-bottom-window ()
  "Move the cursor to center/top window"
  (interactive)
  (cc/window-top-2)
  (windmove-down))

(defun cc/window-top-3 ()
  "Move the cursor to center/top window"
  (interactive)
  (evil-window-top-left)
  (windmove-right)
  (windmove-right))

(defun cc/window-top-4 ()
  "Move the cursor to center/top window"
  (interactive)
  (evil-window-top-left)
  (windmove-right)
  (windmove-right)
  (windmove-right))

(defun cc/left-bottom-window ()
  "Move the cursor to center/top window"
  (interactive)
  (evil-window-top-left)
  (windmove-down))

(map! :leader "wN" 'evil-window-new)

(map! :leader "wn" 'evil-window-top-left)
(map! :leader "we" 'cc/window-top-2)
(map! :leader "wi" 'cc/window-top-3)
(map! :leader "wy" 'cc/window-top-4)
;; (map! :leader "w/" 'cc/left-bottom-window)
(map! :leader "w," 'cc/middle-bottom-window)
(map! :leader "w." 'evil-window-bottom-right)

(map! :leader "w4" 'evil-window-top-left)
(map! :leader "w5" 'cc/window-top-2)
(map! :leader "w6" 'cc/window-top-3)
(map! :leader "w1" 'cc/left-bottom-window)
(map! :leader "w2" 'cc/middle-bottom-window)
(map! :leader "w3" 'evil-window-bottom-right)
;;; -----------------------------------
;;; Org-mode
;;; -----------------------------------
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/Notes")

(after! org
  ;;   (add-to-list 'org-capture-templates
  ;;                '("t" "Personal Task" entry (file +org-capture-todo-file) "* TODO %?\n%i" :empty-lines 1)
  ;;                )
  ;;
  (setq org-capture-templates
        '(
          ("t" "Personal Task"  entry
           (file +org-capture-todo-file)
           "* TODO %?\n%i" :empty-lines 1)
          ("r" "Personal Task (with reference)"  entry
           (file +org-capture-todo-file)
           "* TODO %?\n%i\n%a" :empty-lines 1)
          ("z" "Original Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* [ ] %?\n%i\n%a" :prepend t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)
          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t)
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
          ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
          ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t))
        )
  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE" )
          (sequence "IN-PROGRESS(p)" "WAITING(w)" "|" "DONE" "CANCELED(c)")))
  ;; (define-key org-agenda-mode-map "<TAB>" 'org-agenda-show)
  )

(map! :after evil-org-agenda
      :map evil-org-agenda-mode-map
      :m "o" 'org-agenda-show)

(map! :after evil-org-agenda
      :map evil-org-agenda-mode-map
      :m "S-SPC" 'evil-switch-to-windows-last-buffer)

(defun cc/switch-agenda-buffer ()
    "Switch to agenda buffer, or open default agenda."
    (interactive)
    (if (get-buffer "*Org Agenda*")
        (switch-to-buffer "*Org Agenda*")
      (org-agenda nil "d"))
    )

(define-key evil-normal-state-map (kbd "S-SPC") 'cc/switch-agenda-buffer)

;; (setq org-agenda-files '("~/Documents/Notes"))
(setq org-agenda-files '("~/Documents/Notes" "~/Documents/Notes/journal" "~/Documents/Notes/roam" "~/Documents/Notes/roam/daily"))
;; (add-to-list 'org-agenda-files org-journal-dir)

(setq org-priority-highest 65)
(setq org-priority-default 67)
(setq org-priority-lowest 69)
(setq org-priority-start-cycle-with-default nil)
(setq org-priority-faces
      '(
        (65 . error)
        (66 . warning)
        (67 . success)
        (68 . success)
        (69 . success)
        ))


(defun cc/last-day-of-month-p (date)
  (let* ((month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (last-month-day (calendar-last-day-of-month month year))
         (month-day (cadr date)))
    (eq month-day last-month-day)))


(defun cc/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun cc/org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(setq org-agenda-custom-commands
      '(
        ("n" "Agenda and all TODOs"
         ((agenda "")
          (alltodo "")))
        ("d" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-span 3)(org-agenda-start-day "-0d")))
          (alltodo ""
                   ((org-agenda-skip-function
                     '(or (cc/org-skip-subtree-if-priority ?A)
                          (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))
                   ))
         ;; ((org-agenda-compact-blocks t))
         )))


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
(setq projectile-project-search-path '("~/Developer"))


(setq org-roam-directory "~/Documents/Notes/roam")
(setq +org-roam-open-buffer-on-find-file nil)

(setq org-roam-capture-templates '(("d" "default" plain "%?"
                                    :if-new
                                    (file+head "${slug}.org"
                                               "#+title: ${title}\n#+date: %u\n#+lastmod: \n\n")
                                    :immediate-finish t)
                                   ("o" "old" plain "%?" :target
                                    (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                    :unnarrowed t))
      time-stamp-start "#\\+lastmod: [\t]*")

(setq deft-directory org-directory
      deft-recursive t
      deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
      deft-use-filename-as-title t)

(defun cc/search-notes ()
  "Conduct a text search in files under `user-emacs-directory'."
  (interactive)
  (let ((default-directory org-directory))
    (call-interactively
     (cond ((featurep! :completion ivy)     #'+ivy/project-search-from-cwd)
           ((featurep! :completion helm)    #'+helm/project-search-from-cwd)
           ((featurep! :completion vertico) #'+vertico/project-search-from-cwd)
           (#'rgrep)))))

(map! :leader "sn" 'cc/search-notes)

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
(setq dap-python-debugger 'debugpy)
;; (setq dap-python-executable "python3")

;; (add-hook! python-mode 'anaconda-mode)
;; (add-hook! vterm-mode 'anaconda-mode)
;; (add-hook! python-mode 'anaconda-eldoc-mode)

(dap-mode 1)
(defun dap-python-johmue-populate-test-at-point (conf)
  "Populate CONF with the required arguments."
  (if-let ((test (test-cockpit--python--test-function-path)))
      (plist-put conf :program test)
    (user-error "`dap-python': no test at point"))
  (plist-put conf :cwd (lsp-workspace-root))
  (dap-python--populate-start-file-args conf))

(dap-register-debug-provider "python-test-at-point" 'dap-python-johmue-populate-test-at-point)
(dap-register-debug-template "Python :: Run pytest (at point!!!)"
                             (list :type "python-test-at-point"
                                   :args ""
                                   :program nil
                                   :module "pytest"
                                   :request "launch"
                                   :name "Python :: Run pytest (at point!!!)"))


;; (setq +evil-collection-disabled-list (remove 'anaconda-mode +evil-collection-disabled-list))

;; HACK: fix python f-strings + smartparens
(after! smartparens
  (sp-local-pair '(python-mode) "f\"" "\"")
  (sp-local-pair '(python-mode) "f'" "'"))

(defun pyvenv-autoload ()
          (interactive)
          "auto activate venv directory if exists"
          (f-traverse-upwards (lambda (path)
              (let ((venv-path (f-expand ".venv" path)))
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

;; (use-package! lsp-sonarlint)

;; (use-package! lsp-sonarlint-python)
;; (setq lsp-sonarlint-python-enabled t)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.venv.*\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]__pycache__\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.pytest_cache\\'"))


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
(map! :map evil-normal-state-map "C-0" 'doom/reset-font-size)

(map! :map evil-normal-state-map "s-+" 'text-scale-increase)
(map! :map evil-normal-state-map "s--" 'text-scale-decrease)
(map! :map evil-normal-state-map "s-0" 'doom/reset-font-size)
(map! :map evil-normal-state-map "s-=" 'doom/reset-font-size)
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
;;; Yasnippet
;;; -----------------------------------------------------------------------
(setq +lsp-company-backends '(:separate company-yasnippet company-capf))

(add-hook! company-mode
  (map! :map company-active-map "C-n" nil)
  (map! :i "C-n" 'yas-expand)
  )

;;; -----------------------------------------------------------------------
;;; Misc
;;; -----------------------------------------------------------------------
;; (after! terraform (set-company-backend! 'company-files))
(add-hook! terraform-mode (set-company-backend! 'company-files))

(setq +file-templates-dir (concat doom-private-dir "templates/"))

;; treat '_' as part of the word (see: https://evil.readthedocs.io/en/latest/faq.html#underscore-is-not-a-word-character)
(defalias 'forward-evil-word 'forward-evil-symbol)


(map! :map evil-normal-state-map "z e" 'hs-hide-level
      :map evil-normal-state-map "z i" 'hs-hide-all
      :map evil-normal-state-map "z s" 'hs-show-all)

;; Add some Leader Keybindings
(map! :leader "w a" 'ace-window)
(map! :leader "w f" 'ace-swap-window)

;; Swap leader-: with leader-;
(map! :leader ":" 'pp-eval-expression
      :leader ";" 'execute-extended-command)

(setq zoneinfo-style-world-list '(
                                  ("America/Los_Angeles" "Los Angeles")
                                  ("America/New_York" "New York")
                                  ("America/Sao_Paulo" "Sao Paulo")
                                  ("Europe/Lisbon" "Lisbon")
                                  ("Europe/Paris" "Paris")
                                  ("Asia/Tokyo" "Tokyo")
                                  ))
;;; -----------------------------------------------------------------------
;;; Disable emacs contamining the system clipboard
;;; -----------------------------------------------------------------------
;; (setq select-enable-clipboard nil)
;; (map! "s-v" #'clipboard-yank)
;; (map! "s-c" #'clipboard-kill-ring-save)
