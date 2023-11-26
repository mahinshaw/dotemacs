;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

;;; Commentary:

;;; Configuration for Emacs.

;;; Code:

;;; Early birds
;; Setting gc cons to a higher value can help with up front churn
;; (setq gc-cons-threshold 100000000)
(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq-default user-init-file (or load-file-name buffer-file-name)
                user-emacs-directory (file-name-directory user-init-file))

  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  ;; (package-initialize)
  (setq inhibit-startup-buffer-menu t
        inhibit-startup-screen t
        inhibit-startup-echo-area-message "mhinshaw"
        initial-buffer-choice t
        initial-scratch-message ""
        load-prefer-newer t)
  ;; bells and flash no more
  (setq ring-bell-function 'ignore)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(push (concat user-emacs-directory "lisp") load-path)

(when (native-comp-available-p)
    (defvar native-comp-deferred-compilation-deny-list nil)
    ;; disable native comp warnings for now
    (setq native-comp-async-report-warnings-errors nil))

;; gfind is faster than find on a mac. woop
(setq straight-find-executable "/opt/homebrew/bin/gfind")
;; using this mode removes 2 seconds of startup time in bootstrap!
(setq straight-check-for-modifications 'live)
;; Check cost of boostrap
;; (benchmark 1 '(unwind-protect (straight--cache-package-modifications) (straight--uncache-package-modifications)))

;;; mah-straight.el - get the package manager going.
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

(message "Loading bootstrap...done (%.3fs)"
         (float-time (time-subtract (current-time)
                                    before-user-init-time)))
(require 'straight)
(setq straight-use-package-by-default t
      straight-vc-git-default-clone-depth 1)

(progn ;    `use-package'
  (straight-use-package 'use-package)
  (require  'use-package)
  (setq use-package-verbose t
        use-package-always-defer t))

(require 'mah-macros)
(require 'mah-defs)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (when (file-exists-p custom-file)
;;   (load custom-file))

(use-package diminish
  :demand t)

(use-package general
  :demand t
  :config
  (progn
    (general-evil-setup)
    ;; (setq general-override-states '(normal visual motion))
    (general-override-mode)))

(use-package no-littering
  :demand t
  :config
  (no-littering-theme-backups)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-feature server
  :demand t
  :config (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Long tail
(use-package esup
  :config
  (setq esup-depth 0))

(use-package exec-path-from-shell
  ;; Loading from script now to capture shell env on mac
  :disabled
  :demand t
  :init
  (setq exec-path-from-shell-arguments '()
        exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "JAVA_HOME"))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; Direnv is my friend
(use-package direnv
  :demand t
  :config
  (direnv-mode))

;;; epa-file
(use-feature epa-file
  :init
  (when mah-is-mac
    (setq epa-pinentry-mode 'loopback)))

;;; auth-source and pass
(use-feature auth-source-pass
  :demand t
  :config
  (auth-source-pass-enable))

(use-feature auth-source
  :init
  (setq auth-sources '(password-store)) )

(use-package pass)

;;; Bindings without a prefix for normal and motion
(general-create-definer mah-no-pref
  :states '(normal motion)
  :keymaps 'override)

;; leader map
(general-create-definer mah-leader
  :states '(normal motion)
  :keymaps 'override
  :prefix mah-leader)

(general-create-definer mah-local-leader
  :states '(normal motion)
  :prefix mah-local-leader)
(mah-local-leader
  :keymaps 'override
  "" nil)

;;; themes

(use-package doom-themes
  :init
  (progn
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)
    (load-theme 'doom-oceanic-next t)
    (doom-themes-org-config)))

(use-package poet-theme
  ;; :init
  ;; (add-hook 'text-mode-hook (lambda () (variable-pitch-mode 1)))
  ;; (load-theme 'poet t)
  )

(set-face-attribute 'default nil :font "Source Code Pro Semibold" :height 140)

;;; General emacs

(when mah-is-mac
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta
        ns-use-native-fullscreen nil
        ;; gnu ls has --dired option
        insert-directory-program (executable-find "gls")
        dired-use-ls-dired t
        ))

;; Emacs variable defaults

;; auto save every 500 words typed
(setq auto-save-interval 500
      ;; don't recenter the point
      scroll-conservatively 10000
      ;; scroll one line at a time
      scroll-step 1
      ;; scroll 8 lines from top and bottom
      scroll-margin 8
      ;; scroll horizontally 1 column at a time.
      hscroll-step 1
      ;; scroll horizontally 4 lines left and right
      hscroll-margin 4
      ;; update read buffers for subprocess to allow more efficient passing of data
      ;; https://twitter.com/yonchovski/status/1208476565715202048
      ;; https://github.com/emacs-mirror/emacs/commit/cc78faee7d23dd0433ba537818a68cbd20fa52a3
      read-process-output-max (* 1024 1024)
      )
(setq-default truncate-lines t
              ;; tabs should be 4 spaces
              tab-width 4
              ;; don't want no stinking tabs
              indent-tabs-mode nil)

(general-def
  :keymaps 'override
  "C-M-<return>" 'toggle-frame-fullscreen
  "M-`" 'other-frame
  )

(mah-leader
  ;; Buffers
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bd" 'kill-this-buffer
  "bR" 'mah-refresh-buffer

  ;; Files
  "fs" 'save-buffer
  "fed" 'mah-find-init-file

  ;; help
  "hdc" 'describe-char
  "hdf" 'describe-function
  "hdF" 'describe-face
  "hdk" 'describe-key
  "hdm" 'describe-mode
  "hdv" 'describe-variable

  "u" 'universal-argument

  ;; windows
  "w=" 'balance-windows
  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wl" 'evil-window-right

  "TF" 'toggle-frame-fullscreen
  "Tl" 'display-line-numbers-mode
  )

;; insert mode helpers
(general-def
  :states 'insert
  :keymaps 'override
  "C-l" 'mah-insert-lambda-arrow-for-major-mode)

;; window movements
(general-def
  :states 'normal
  :keymaps 'override
  "C-h" 'evil-window-left
  "C-j" 'evil-window-down
  "C-k" 'evil-window-up
  "C-l" 'evil-window-right
  )

(general-nmap "v" (general-key-dispatch 'evil-visual-char
                    :timeout 0.2
                   "v" 'split-window-horizontally))
(general-nmap "s" (general-key-dispatch 'evil-substitute
                    :timeout 0.2
                    "s" 'split-window-vertically))

(use-feature display-line-numbers
  :init
  ;; (setq display-line-numbers-type 'relative)
  (when (version<= "26.0.50" emacs-version )
    ;; (global-display-line-numbers-mode)
    (add-hook 'prog-mode-hook #'display-line-numbers-mode)))


;;; Evil mode and related
(use-package evil
  :demand t
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil
        evil-normal-state-cursor '("DarkGoldenrod2" box)
        evil-insert-state-cursor '("chartreuse3" (bar . 2))
        evil-emacs-state-cursor '("SkyBlue3" box)
        evil-replace-state-cursor '("chocolate" (hbar . 2))
        evil-visual-state-cursor '("gray" (hbar . 2))
        evil-motion-state-cursor '("plum3" box)
        )
  :config
  (evil-mode 1))

(evil-define-motion evil-first-non-blank-or-digit-argument ()
  "Move the cursor to the first non-blank row of the current line.
This function passes its command to `digit-argument' (usually a 0)
if it is not the first event."
  :type exclusive
  (cond
   (current-prefix-arg
    (setq this-command #'digit-argument)
    (call-interactively #'digit-argument))
   (t
    (setq this-command #'evil-first-non-blank)
    (call-interactively #'evil-first-non-blank))))

(general-mmap
  "0" 'evil-first-non-blank-or-digit-argument
  "^" 'evil-beginning-of-line)

(use-package evil-collection
  :demand t
  :config
  (setq evil-collection-setup-minibuffer nil) ;; TODO this messes with helm bindings.
  (evil-collection-init)
  ;; from evil-magit
  (general-nmap '(magit-mode-map)
    "q" 'magit-mode-bury-buffer))

(use-package evil-commentary
  :demand t
  :diminish 'evil-commentary-mode
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :demand t
  :config
  (global-evil-surround-mode 1))

(use-package evil-multiedit
  :init
  (setq evil-multiedit-store-in-search-history t)
  (general-vmap
    "R" 'evil-multiedit-match-all
    "M-d" 'evil-multiedit-match-and-next
    "M-D" 'evil-multiedit-match-and-prev
    "C-M-D" 'evil-multiedit-restore)
  (general-nmap
    "M-d" 'evil-multiedit-match-and-next
    "M-D" 'evil-multiedit-match-and-prev)
  (general-mmap
    "RET" 'evil-multiedit-toggle-or-restrict-region)
  (general-def 'evil-multiedit-state-map
    "RET" 'evil-multiedit-toggle-or-restrict-region
    "C-n" 'evil-multiedit-next
    "C-p" 'evil-multiedit-prev)
  (general-def 'evil-multiedit-insert-state-map
    "C-n" 'evil-multiedit-next
    "C-p" 'evil-multiedit-prev)
  (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match))

;;; Company

(use-package company
  :diminish (company-mode . " C")
  :init
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match nil
        company-tooltip-align-annotations t
        company-tooltip-minimum-width 60)
  (general-def company-active-map
   "C-j" 'company-select-next-or-abort
   "C-k" 'company-select-previous-or-abort
   "C-l" 'company-complete-selection)
  (progn
    (global-company-mode)))

(mah-leader
  "SPC" 'execute-extended-command
  "ff" 'find-file
  "FF" 'make-frame
  "Fo" 'other-frame
  "jd" 'find-dired)

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode)
  (mah-leader
    "rl" #'vertico-repeat)
  (general-def vertico-map
    "<escape>"  #'abort-recursive-edit
    "C-f" 'vertico-next-group
    "C-b" 'vertico-previous-group
    "C-j" 'vertico-next
    "C-k" 'vertico-previous
    "C-l" 'vertico-insert
    "C-h" 'vertico-directory-delete-word)
  :config
  ;; Gathered from https://github.com/minad/vertico/wiki#using-prescientel-filtering-and-sorting
  (setq completion-styles '(prescient basic))
  ;; Use `prescient-completion-sort' after filtering.
  (setq vertico-sort-function #'prescient-completion-sort)

  (defun vertico-prescient-remember ()
    "Remember the chosen candidate with Prescient."
    (when (>= vertico--index 0)
      (prescient-remember
       (substring-no-properties
        (nth vertico--index vertico--candidates)))))
  (advice-add #'vertico-insert :after #'vertico-prescient-remember))

(use-package ctrlf
  :demand t
  :config
  (ctrlf-mode +1))

(use-package prescient
  :demand t
  :config
  (prescient-persist-mode +1))

(use-package company-prescient)

(use-package consult
  :demand t
  :init
  (mah-leader
    "bb" 'consult-buffer
    "fr" 'consult-recent-file
    "ha" 'consult-apropos
    "pi" 'consult-project-imenu
    "sp" 'consult-ripgrep
    "ss" 'consult-line
    "ry" 'consult-yank-pop)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle' to
  ;; switch between the annotators.
  ;; (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  )

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim))       ;; good alternative: M-.
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  (mah-leader
    "hdB" 'embark-bindings)  ;; alternative for `describe-bindings'
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Magit et al.
(use-package magit
  :commands (magit-status)
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :init
  (mah-leader
    "gb" 'magit-blame
    "gs" 'magit-status)
  :config
  (progn
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules
                            'magit-insert-stashes
                            'append)
    (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
    (mah-local-leader
      :keymaps 'with-editor-mode-map
      "c" 'with-editor-finish
      "k" 'with-editor-cancel
      )))

;; (use-package forge
;;   :demand t
;;   :after magit)

;;; General tooling
(use-feature abbrev
  :diminish 'abbrev-mode)

(use-package ace-window
  :config
  (progn
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (mah-leader
      "ws" 'ace-swap-window
      "ww" 'ace-window)))

(use-feature ansi-color
  :init
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))

;; (use-package anzu
;;   :config
;;   (progn
;;     (global-anzu-mode +1)
;;     (general-def
;;       :keymaps 'override
;;       [remap query-replace] 'anzu-query-replace
;;       [remap query-replace-regexp] 'anzu-query-replace-regexp
;;       )))

;; (use-package evil-anzu)

(use-feature autorevert
  :diminish 'auto-revert-mode)

;; (use-package avy
;;   :config
;;   (progn
;;     (mah-leader
;;       "jc" 'avy-goto-char-timer
;;       "jl" 'avy-goto-line
;;       "jw" 'avy-goto-word-1
;;       )))

(use-package dash
  :config (dash-enable-font-lock))

(use-package diff-hl
  :demand t
  :init
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)
  :config
  ;; (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (diff-hl-margin-mode))

(use-feature dired
  :config
  (progn
    (general-nmap
     "-" 'dired-jump)
    (general-def 'dired-mode-map
      "-" 'dired-up-directory)
    (setq dired-listing-switches "-alh")))

(use-feature ediff
  :init
  (setq-default
   ediff-window-setup-function 'ediff-setup-windows-plain
   ediff-split-window-function 'split-window-horizontally))

(use-feature eldoc
  :when (version< "25" emacs-version)
  :config (global-eldoc-mode))

(use-package eyebrowse
  :demand t
  :config
  (setq eyebrowse-mode-line-separator " | "
        eyebrowse-new-workspace t
        eyebrowse-wrap-around t)
  (mah-leader
    "lc" 'eyebrowse-create-window-config
    "ln" 'eyebrowse-next-window-config
    "lo" 'eyebrowse-last-window-config
    "lp" 'eyebrowse-prev-window-config
    "lx" 'eyebrowse-close-window-config
    "lR" 'eyebrowse-rename-window-config

    "ll" 'eyebrowse-switch-to-window-config
    "l0" 'eyebrowse-switch-to-window-config-0
    "l1" 'eyebrowse-switch-to-window-config-1
    "l2" 'eyebrowse-switch-to-window-config-2
    "l3" 'eyebrowse-switch-to-window-config-3
    "l4" 'eyebrowse-switch-to-window-config-4
    "l5" 'eyebrowse-switch-to-window-config-5
    "l6" 'eyebrowse-switch-to-window-config-6
    "l7" 'eyebrowse-switch-to-window-config-7
    "l8" 'eyebrowse-switch-to-window-config-8
    "l9" 'eyebrowse-switch-to-window-config-9
    )
  (eyebrowse-mode t))

(use-feature files
  :config
  (setq backup-by-copying t
        delete-old-versions t
        kept-new-versions 10
        kept-old-versions 0))

(use-package flycheck
  :diminish (flycheck-mode ." FlyC")
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (mah-leader
    "tf" 'flycheck-mode
    "eb" 'flycheck-buffer
    "ec" 'flycheck-clear
    "el" 'flycheck-list-errors
    "en" 'flycheck-next-error
    "ep" 'flycheck-previous-error)
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  )

(use-feature help
  :config (temp-buffer-resize-mode))

(use-package helpful
  :init
  (mah-leader
    "hdc" #'helpful-command
    "hdf" #'helpful-callable
    "hdF" #'helpful-function
    "hdv" #'helpful-variable
    "hdk" #'helpful-key
    "hdp" #'helpful-at-point))

(use-feature Info-mode
  :init
  (general-nmap 'Info-mode-map
    "C-n" 'Info-next
    "C-p" 'Info-prev
    ))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-feature lisp-mode
  :diminish (lisp-mode . " l")
  :init
  ;; TODO investigate these modes.
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook #'indent-spaces-mode))

;; TODO: this imports swiper/ivy
;; (use-package lispy
;;   :diminish lispy-mode
;;   :config
;;   (lispy-set-key-theme '(lispy c-digits)))

;; Switch to evil-cleverparens
(use-package lispyville
  :disabled
  :diminish '(lispyville-mode . " ()");; (lispyville-mode-line-string " 🍰" " 🍰"))
  :init
  (general-def '(insert emacs) 'lispyville-mode-map
    "\"" 'lispy-doublequote
    "(" 'lispy-parens
    "[" 'lispy-brackets
    "{" 'lispy-braces)
  (lispyville-set-key-theme '(additional
                              additional-wrap
                              operators
                              slurp/barf-cp
                              ;; wrap
                              )))

(use-package man
  :config (setq Man-width 80))

(use-feature outline
  :diminish 'outline-minor-mode
  :config
   ;; show org ediffs unfolded (from 'outline).
  (add-hook 'ediff-prepare-buffer-hook #'show-all))

(use-package paren
  :config (show-paren-mode))

(use-feature prog-mode
  :config
  ;; (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))

(use-package projectile
  :commands (projectile-switch-project projectile-find-file)
  :init
  (mah-leader
    "pb" 'projectile-switch-to-buffer
    "pf" 'projectile-find-file
    "pl" 'projectile-switch-project
    "pI" 'projectile-invalidate-cache
    "p'" 'projectile-run-vterm
    )
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t
        projectile-completion-system 'default
        projectile-ignored-projects '("~/"))
  (add-to-list 'projectile-globally-ignored-directories "~/Dropbox/Books")
  (add-to-list 'projectile-globally-ignored-directories "~/.emacs.d/var/lsp-java/workspace")

  ;; https://emacs.stackexchange.com/questions/26266/projectile-and-magit-branch-checkout/26272
  (defun run-projectile-invalidate-cache (&rest _args)
    ;; We ignore the args to `magit-checkout'.
    (projectile-invalidate-cache nil))
  (advice-add 'magit-checkout
              :after #'run-projectile-invalidate-cache)
  (advice-add 'magit-branch-and-checkout ; This is `b c'.
              :after #'run-projectile-invalidate-cache))

(use-package recentf
  :demand t
  :config
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
  (recentf-mode +1))

(use-feature reveal
  :diminish 'reveal-mode)

;; (use-package rmsbolt)

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :when (version< "25" emacs-version)
  :config (save-place-mode))

(use-feature simple
  :config (column-number-mode))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook #'indicate-buffer-boundaries-left))

(use-feature tramp
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(use-package undo-tree
  :diminish 'undo-tree-mode)

(use-package wgrep)

(use-package rg
  :init
  (mah-leader
    "srr" #'rg
    "srp" 'rg-project
    ))

(use-package deadgrep
  :init
  (mah-leader
    "swd" #'deadgrep))

(use-package which-key
  :demand t
  :diminish 'which-key-mode
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
    (concat mah-leader " b") "buffers"
    (concat mah-leader " e") "flycheck"
    (concat mah-leader " f") "files"
    (concat mah-leader " F") "frames"
    (concat mah-leader " g") "git"
    (concat mah-leader " h") "help"
    (concat mah-leader " p") "projects"
    (concat mah-leader " r") "resume"
    (concat mah-leader " s") "search"
    (concat mah-leader " t") "toggle"))

(use-feature winner-mode
  :init
  (winner-mode t)
  ;; restore window layout when done.
  (add-hook 'ediff-quit-hook #'winner-undo))

(use-package with-editor
  :diminish with-editor-mode)

;;; mode line
;; (use-package minions
;;   :demand t
;;   :config (minions-mode 1))

(display-time-mode 1)
(setq display-time-string-forms '(dayname " " 12-hours ":" minutes " " am-pm))

;; (setq mode-line-format
;;       (list
;;        "%e"
;;        mode-line-front-space
;;        mode-line-mule-info
;;        mode-line-client
;;        mode-line-modified
;;        mode-line-remote
;;        mode-line-frame-identification
;;        mode-line-buffer-identification
;;        "   "
;;        mode-line-position
;;        evil-mode-line-tag
;;        (vc-mode vc-mode)
;;        "  "
;;        mode-line-modes
;;        mode-line-misc-info
;;        mode-line-end-spaces
;;        ))
;; (use-package moody
;;   :demand t
;;   :config
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode))

;; if you see Japanese/Chinese icons, run `all-the-icons-install-fonts`
(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-height 18
        doom-modeline-bar-width 3
        doom-modeline-buffer-file-name-style 'truncate-upto-project ;; file-name
        doom-modeline-lsp t))

(use-package rainbow-mode)

;; TODO - error below
;; Error in post-command-hook (yas--post-command-handler): (wrong-number-of-arguments #<subr signal> 1)
;; (use-package yasnippet
;;   :demand t
;;   :config
;;   (yas-global-mode))

;;; Dev tooling
(use-package vterm
  :disabled
  :config
  (general-nmap vterm-mode-map
    "C-d" #'vterm--self-insert
    "i" #'evil-insert-resume
    "o" #'evil-insert-resume))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :init
  (mah-local-leader 'restclient-mode-map
    "ee" 'restclient-http-send-current
    "eE" 'restclient-http-send-current-raw
    "ew" 'restclient-http-send-current-stay-in-window
    "y" 'restclient-copy-curl-command))

(use-package company-restclient
  :config
  (add-to-list 'company-backends 'company-restclient) )

(use-package tree-sitter
  :demand t)

(use-package tree-sitter-langs
  :demand t
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package just-mode)

;;; language specific
(use-package csv-mode
  :init
  (mah-local-leader 'csv-mode-map
     "ti" 'csv-toggle-invisibility
     "tt" 'csv-transpose
     "tcs" 'csv-set-comment-start
     "u" 'csv-unalign-fields
     "a" 'csv-align-fields
     "yt" 'csv-yank-as-new-table
     "yf" 'csv-yank-fields
     "kf" 'csv-kill-fields
     "td" 'csv-toggle-descending
     ;; "" 'csv-reverse-region
     "tsn" 'csv-sort-numeric-fields
     "tss" 'csv-sort-fields
    ))

(use-package docker)

(use-package dockerfile-mode)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  ;; use prettier to format markdown
  (add-hook 'markdown-mode-hook 'prettier-js-mode)
  (mah-local-leader '(markdown-mode-map gfm-mode-map)
    "il" 'markdown-insert-link
    "iL" 'markdown-insert-reference-link-dwim
    "ih" 'markdown-insert-header
    "=" 'prettier-js
    ">" 'markdown-indent-region
    "<" 'markdown-outdent-region
    ))

(use-package adoc-mode
  :ensure t)

(use-feature elisp-mode
  :init
  (progn
    (mah-company emacs-lisp-mode company-capf)
    (add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
    ;; (add-hook 'emacs-lisp-mode-hook #'lispyville-mode)
    )
  :config
  (mah-local-leader
    :keymaps 'emacs-lisp-mode-map
    "eb" 'eval-buffer
    "ef" 'eval-defun
    "er" 'eval-region
    "es" 'eval-last-sexp
    "hi" 'consult-imenu))

(use-package elisp-slime-nav
  :diminish 'elisp-slime-nav-mode
  :init
  (progn
    (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
      (add-hook hook 'elisp-slime-nav-mode)))
  :config
  (general-nmap emacs-lisp-mode-map
    "gd" 'elisp-slime-nav-find-elisp-thing-at-point
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point))

(use-package clojure-mode
  :disabled
  :init
  (add-hook 'clojure-mode-hook #'lispyville-mode)
  :config
  (progn
    (mah-local-leader 'clojure-mode-map
      "'" 'cider-jack-in
      "\"" 'cider-jack-in-cljs
      "="  'cider-format-buffer

      "db" 'cider-debug-defun-at-point

      "e;" 'cider-eval-defun-to-comment
      "eb" 'cider-eval-buffer
      "ee" 'cider-eval-last-sexp
      "ef" 'cider-eval-defun-at-point
      "em" 'cider-macroexpand-1
      "eM" 'cider-macroexpand-all
      "eP" 'cider-pprint-eval-last-sexp
      "er" 'cider-eval-region
      "ew" 'cider-eval-last-sexp-and-replace

      "gb" 'cider-pop-back
      "gc" 'cider-classpath
      "ge" 'cider-jump-to-compilation-error
      "gn" 'cider-find-ns
      "gr" 'cider-find-resource
      "gs" 'cider-browse-spec
      "gS" 'cider-browse-spec-all

      "ha" 'cider-apropos
      "hc" 'clojure-cheatsheet
      "hg" 'cider-grimoire
      "hh" 'cider-doc
      "hj" 'cider-javadoc
      "hn" 'cider-browse-ns
      "hN" 'cider-browse-ns-all

      "sq" 'cider-quit
      "ss" 'cider-switch-to-repl-buffer
      "sn" 'cider-repl-set-ns

      "tf" 'cider-test-rerun-failed-tests
      "tn" 'cider-test-run-ns-tests
      "tt" 'cider-test-run-test
      "tr" 'cider-test-rerun-test
      "tR" 'cider-test-show-report

      "Te" 'cider-enlighten-mode
      "Tt" 'cider-auto-test-mode
      )
    (general-nmap 'clojure-mode-map
      "gd" 'cider-find-var
      "K" 'cider-doc)))

(use-package cider
  :disabled
  :init
  (add-hook 'cider-repl-mode-hook #'lispyville-mode)
  :config
  (mah-local-leader 'cider-repl-mode-map
    "sC" 'cider-repl-clear-buffer
    "sc" 'cider-repl-clear-output
    "sq" 'cider-quit
    "ss" 'cider-switch-to-last-clojure-buffer
    )
  )

(use-package lsp-mode
  :init
  (setq lsp-inhibit-message t
        lsp-eldoc-render-all nil
        lsp-prefer-flymake nil
        ;; lsp-eslint-server-command `("node"
        ;;                             ;; "/Users/mhinshaw/.vscode/extensions/dbaeumer.vscode-eslint-2.1.13/server/out/eslintServer.js"
        ;;                             ;; "/Users/mhinshaw/workspace/typescript/vscode-eslint/server/out/eslintServer.js"
        ;;                             ,(no-littering-expand-var-file-name "lsp/server/eslint/unzipped/extension/server/out/eslintServer.js")
        ;;                             "--stdio")
        lsp-eslint-package-manager "yarn"
        lsp-headerline-breadcrumb-segments '(project file symbols)
        )
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  (defadvice xref-find-definitions (before add-evil-jump activate) (evil-set-jump))
  :config
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]bin\\")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]build\\")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\].*/build\\")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\].*/node_modules\\")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.log\\")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.ccls-cache\\")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.cache\\")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.vscode\\")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]coverage\\")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\].*/dist\\")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\].*/ksdk/packages/.*/lib\\")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]pkg\\")
  )

(use-package lsp-ui)

(use-package treemacs)

(use-package treemacs-projectile)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(defmacro mah:lsp-default-keys (mode-map)
  "Given a MODE-MAP, assign the default keys for lsp based major modes to local leader."
  `(progn
     (mah-local-leader ,mode-map
       "=" 'lsp-format-buffer
       "as" 'lsp-ui-sideline-apply-code-actions
       "aa" 'lsp-execute-code-action
       "at" 'lsp-treemacs-quick-fix

       "gD" 'lsp-find-declaration
       "gg" 'lsp-find-definition ;; (xref-find-definitions :async true)
       "gG" 'xref-find-definitions-other-window
       "gi" 'lsp-goto-implementation
       "gr" 'lsp-find-references
       "gt" 'lsp-goto-type-definition

       "ha" 'xref-find-apropos
       "hh" 'lsp-describe-thing-at-point
       "hi" 'consult-imenu

       "rn" 'lsp-rename

       "ssd" 'lsp-describe-session
       "sr" 'lsp-workspace-restart
       "sq" 'lsp-shutdown-workspace
       "sfa" 'lsp-workspace-folders-add
       "sfd" 'lsp-workspace-folders-remove

       "Te" 'lsp-treemacs-errors-list
       "Tl" 'lsp-lens-mode
       "Tt" 'treemacs-project
       "Ts" 'lsp-treemacs-symbols

       "ug" 'lsp-ui-peek-find-definitions
       "ui" 'lsp-ui-peek-find-implementation
       "uI" 'lsp-ui-imenu
       "ur" 'lsp-ui-peek-find-references)

     (general-nmap
       :keymaps ,mode-map
       "gd" 'lsp-find-definition
       "gi" 'lsp-goto-implementation
       "K" 'lsp-describe-thing-at-point))
  )

(defmacro mah:dap-default-keys (mode-map)
  "Given a MODE-MAP, assign the default keys for dap based major modes to local leader."
  `(progn
     (mah-local-leader ,mode-map
       "dbb" 'dap-breakpoint-toggle
       "dba" 'dap-breakpoint-add
       "dbr" 'dap-breakpoint-delete
       "dbR" 'dap-breakpoint-delete-all
       "ddd" 'dap-debug
       "ddl" 'dap-debug-last
       "ddr" 'dap-debug-recent
       "ddo" 'dap-go-to-output-buffer
       "ddt" 'dap-debug-edit-template
       "dh" 'dap-hydra
       "dl" 'dap-ui-list-sessions
       "dq" 'dap-disconnect
       "dn" 'dap-next
       "di" 'dap-step-in
       "dc" 'dap-continue
       "do" 'dap-step-out
       "dss" 'dap-switch-session
       "dst" 'dap-switch-thread
       "dsf" 'dap-switch-stack-frame
       "dub" 'dap-ui-breakpoints
       "dui" 'dap-ui-inspect
       "dul" 'dap-ui-locals
       "dur" 'dap-uid-repl
       "dus" 'dap-ui-sessions
       "duu" 'dap-ui-inspect-thing-at-point)))

;;; java
(use-package keystore-mode)

(use-package lsp-java
  :init
  ;; TODO - remove?
  ;; (setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/1.12.0/jdt-language-server-1.12.0-202206011637.tar.gz")
  (mah:lsp-default-keys 'java-mode-map)
  (mah:dap-default-keys 'java-mode-map)
  (mah-local-leader 'java-mode-map
    "="  'google-java-format-buffer
    "an" 'lsp-java-actionable-notifications

    "bp" 'lsp-java-build-project
    "bu" 'lsp-java-update-project-configuration
    "bU" 'lsp-java-update-user-settings

    "dtc" 'dap-java-debug-test-class
    "dtm" 'dap-java-debug-test-method

    "ee" 'dap-eval
    "er" 'dap-eval-region
    "es" 'dap-eval-dwim

    "hc" 'lsp-java-classpath-browse

    "rec" 'lsp-java-extract-to-constant
    "rel" 'lsp-java-extract-to-local-variable
    "rem" 'lsp-java-extract-method

    "rf" 'lsp-java-create-field
    "rl" 'lsp-java-create-local
    "ri" 'lsp-java-add-import
    "rI" 'lsp-java-organize-imports
    "rp" 'lsp-java-create-parameter
    "rcs" 'lsp-java-convert-to-static-import

    "tc" 'dap-java-run-test-class
    "tm" 'dap-java-run-test-method)
  (setq c-basic-offset 2
        ;; lsp-java-format-enabled nil
        lsp-java-format-on-type-enabled nil
        lsp-java-save-actions-organize-imports nil
        lsp-java-workspace-dir (no-littering-expand-var-file-name "lsp-java/workspace/")
        lsp-java-workspace-cache-dir (no-littering-expand-var-file-name "lsp-java/workspace/.cache/")
        lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        lsp-java-format-settings-profile "GoogleStyle")
  (add-hook 'java-mode-hook #'lsp))

(use-package dap-mode
  :init
  (defun mah-dap-java-hook ()
    (progn
      (dap-mode t)
      (dap-ui-mode t)
      (require 'dap-java)))
  (add-hook 'java-mode-hook #'mah-dap-java-hook)
  :config
  (setq dap-java-test-runner (no-littering-expand-var-file-name "lsp-java/eclipse.jdt.ls/test-runner/junit-platform-console-standalone.jar")))

(require 'google-java-format)
(setq google-java-format-executable (executable-find "google-java-format"))
(defun google-java-hook ()
  "Add a hook to to before-save which run google-java-format."
  (add-hook 'before-save-hook #'google-java-format-buffer nil 'local))
;; (add-hook 'java-mode-hook #'google-java-hook)

(use-package kotlin-mode)

(use-package groovy-mode
  :init
  (setq groovy-indent-offset 2))

(use-package gradle-mode
  :diminish 'gradle-mode
  :init
  (add-hook 'java-mode-hook 'gradle-mode)
  (mah-local-leader '(java-mode-map groovy-mode-map)
    "bgb" 'gradle-build
    "bgB" 'gradle-build--daemon
    "bge" 'gradle-execute
    "bgE" 'gradle-execute--daemon
    "bgt" 'gradle-test
    "bgT" 'gradle-test--daemon))

;; Python
;; (use-package pyenv-mode-auto
;;   :demand t)

(use-feature python
  :init
  (mah:lsp-default-keys 'python-mode-map))

;; (use-package pipenv
;;   :init
;;   (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(use-package swift-mode
  :hook (swift-mode . (lambda () (lsp)))
  :init
  (mah:lsp-default-keys 'swift-mode-map)
  :config
  ;; Needed for lsp to know about the source kit executable.
  (require 'lsp-sourcekit))

(use-package lsp-sourcekit
  :after lsp
  :init
  (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))

;; golang
(use-package go-mode
  :hook (go-mode . lsp)
  :init
  (setq gofmt-command "goimports")
  ;; (mah-local-leader '(go-mode-map)
  ;;   "ga" 'go-goto-arguments
  ;;   "gD" 'go-goto-docstring
  ;;   "gf" 'go-goto-function
  ;;   "gg" 'godef-jump
  ;;   "gG" 'godef-jump-other-window
  ;;   "gn" 'go-goto-function-name
  ;;   "gr" 'go-goto-return-values
  ;;   "gm" 'go-goto-method-receiver

  ;;   "hh" 'godef-describe)
  ;; (general-nmap '(go-mode-map)
  ;;   "gd" 'godef-jump
  ;;   "K" 'godef-describe)
  (mah:lsp-default-keys 'go-mode-map))

;; C/C++
(use-package cmake-mode)

(use-feature c-mode
  :hook (c-mode . lsp)
  :init
  (mah:lsp-default-keys 'c-mode-map))

(use-feature c++-mode
  :hook (c++-mode . lsp)
  :init
  (mah:lsp-default-keys 'c++-mode-map))

(use-feature objc-mode
  :hook (objc-mode . lsp)
  :init
  (mah:lsp-default-keys 'objc-mode-map))

(use-package web-mode
  :init
  ;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'lsp)
  (mah:lsp-default-keys 'web-mode-map))

;; Javascript|Typescript
(use-package nvm
  :demand t
  ;; TODO - Failing at startup.
  ;; :config
  ;; (nvm-use "14")
  )

(use-feature js-mode
  :init
  (setq-default js-indent-level 2))

(use-package rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.cjs\\'" . rjsx-mode))
  (setq-default js-indent-level 2)
  (mah:lsp-default-keys 'rjsx-mode-map)
  (mah-local-leader 'rjsx-mode-map
    "icc" 'js-doc-insert-function-doc
    "icf" 'js-doc-insert-file-doc
    "ict" 'js-doc-insert-tag)
  (add-hook 'rjsx-mode-hook 'lsp))

(use-package typescript-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  (setq-default typescript-indent-level 2)
  (mah:lsp-default-keys 'typescript-mode-map)
  (mah-local-leader 'typescript-mode-map
    "icc" 'js-doc-insert-function-doc
    "icf" 'js-doc-insert-file-doc
    "ict" 'js-doc-insert-tag)
  (add-hook 'typescript-mode-hook 'lsp))

(use-package js-doc
  :init
  (setq js-doc-mail-address "mahinshaw@gmail.com"
        js-doc-author (format "Mark Hinshaw <%s>" js-doc-mail-address)))

(use-package prettier-js
  :init
  ;; (setq prettier-js-args '("--single-quote"))
  ;; (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  ;; (add-hook 'typescript-mode-hook 'prettier-js-mode)
  )

(use-package eslintd-fix
  :init
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode))

;; json
(use-package json-reformat)
(use-package json-snatcher)
(use-package json-navigator)

(use-package graphql-mode)

(defun mah/json-reformat-buffer ()
  "Format from `point-min' to `point-max'."
  (interactive)
  (save-excursion (json-reformat-region (point-min) (point-max))))

(use-package json-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.avsc$" . json-mode))
  (add-hook 'json-mode-hook 'prettier-js-mode)
  (setq js-indent-level 2
        json-reformat:pretty-string? t)
  (mah-local-leader 'json-mode-map
    "=" 'mah/json-reformat-buffer
    "p" 'jsons-print-path
    "jq" 'jsons-print-path-jq)
  (general-vmap 'json-mode-map
    ",=" 'json-reformat-region))

;; SQL (Postgres)
(require 'pg-formatter)
(use-feature sql
  :init
  (mah-local-leader 'sql-mode-map
    "=" 'pg-format))

;; cql - cassandra
(use-package cql-mode)

;; ocaml
;; (use-package tuareg)

;; protobuf
(use-package protobuf-mode
  :straight (protobuf-mode :type git
                           :host github
                           :repo "emacsmirror/protobuf-mode"
                           :files ("protobuf-mode.el")
                           ))

(defun mah/format-xml ()
  "Format the current xml buffer."
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))

(use-feature nxml-mode
  :commands (lsp)
  :init
  (setq lsp-xml-jar-file (substitute-in-file-name "$HOME/.vscode/extensions/redhat.vscode-xml-0.11.0/server/org.eclipse.lemminx-0.11.1-uber.jar"))
  (mah:lsp-default-keys 'nxml-mode-map)
  (general-nmap 'nxml-mode-map
    ",=" #'mah/format-xml))

;; Rust
(use-package rustic
  :init
  (setq
   ;; Disable proc macro because it's annoying and sometimes broken.
   ;; https://github.com/rust-analyzer/rust-analyzer/issues/6835
   lsp-rust-analyzer-diagnostics-disabled ["unresolved-proc-macro"]
   )
  (mah:lsp-default-keys 'rustic-mode-map)
  (mah-local-leader 'rustic-mode-map
    "bb" 'rustic-cargo-build
    "br" 'rustic-cargo-run
    "bl" 'rustic-cargo-clippy
    "tt" 'rustic-cargo-test)
  (add-hook 'rustic-mode-hook 'lsp))

;; Yaml
(use-package yaml-mode
  ;; (add-to-list 'auto-mode-alist '("\\(group_vars/.+\\|host_vars/.+\\)" . yaml-mode))
  )

;; TODO - org load order is a pain
;; (require 'mah-org)

;; Kubernetes
;; Not in use
;; (use-package kubernetes
;;   :demand t
;;   :commands (kubernetes-overview))

;; (use-package kubernetes-evil
;;   :demand t
;;   :after kubernetes)

;; epub
;; (use-package nov
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;;   ;; (add-hook 'nov-mode-hook (lambda () (evil-set-initial-state 'nov-mode 'emacs)))
;;   (general-nmap 'nov-mode-map
;;     ;; "" 'nov-render-document
;;     "v" 'nov-view-source
;;     "V" 'nov-view-content-source
;;     "m" 'nov-display-metadata
;;     "J" 'nov-next-document
;;     "]" 'nov-next-document
;;     "K" 'nov-previous-document
;;     "[" 'nov-previous-document
;;     "t" 'nov-goto-toc
;;     "RET" 'nov-browse-url
;;     ;; "<follow-link>" 'mouse-face
;;     ;; "<mouse-2>" 'nov-browse-url
;;     "TAB" 'shr-next-link
;;     "M-TAB" 'shr-previous-link
;;     "<backtab>" 'shr-previous-link
;;     "SPC" 'nov-scroll-up
;;     "S-SPC" 'nov-scroll-down
;;     "DEL" 'nov-scroll-down
;;     ;; "<home>" 'beginning-of-buffer ;; implemented with `gg'
;;     ;; "<end>" 'end-of-buffer ;; implemented with `G'
;;     )
;;   )

;;; End packages
(progn                                  ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(progn ;     personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

(when (file-exists-p custom-file)
  (load custom-file))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
