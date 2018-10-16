;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

;;; Commentary:

;;; Configuration for Emacs.

;;; Code:

;;; Early birds
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
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(push (concat user-emacs-directory "lisp") load-path)

;;; mah-straight.el - get the package manager going.
(defvar bootstrap-version)
(message "user-emacs-dir is %s" user-emacs-directory)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil nil))

(require 'straight)
(setq straight-use-package-by-default t)

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
(use-package exec-path-from-shell
  :demand t
  :init
  (setq exec-path-from-shell-arguments '())
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "JAVA_HOME")
    (exec-path-from-shell-copy-env "GOROOT")
    (exec-path-from-shell-copy-env "GOPATH")))

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
  :demand t
  :init
  (progn
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)
    (load-theme 'doom-spacegrey t)
    (doom-themes-org-config)))

(set-face-attribute 'default nil :font "Source Code Pro Semibold" :height 140)

;;; General emacs

(when mah-is-mac
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta
        ns-use-native-fullscreen nil))

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
      hscroll-margin 4)
(setq-default truncate-lines t
              ;; tabs should be 4 spaces
              tab-width 4
              ;; don't want no stinking tabs
              indent-tabs-mode nil)

;; (general-def :keymaps 'override "M-RET" 'toggle-frame-fullscreen)

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
  :demand t
  :init (setq display-line-numbers-type 'relative)
  :config
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
  :custom (evil-collection-setup-minibuffer nil) ;; TODO this messes with helm bindings.
  :init
  (evil-collection-init))

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

;;; Ivy et al.
(use-package ivy
  :diminish (ivy-mode . " I")
  :init
  (progn
    (setq ivy-use-virtual-buffers t
          ivy-use-selectable-prompt t
          ivy-height 15
          ivy-count-format "(%d/%d) ")
    (mah-leader
      "SPC" 'counsel-M-x
      "bb" 'ivy-switch-buffer
      "ff" 'counsel-find-file
      "fr" 'counsel-recentf
      "ha" 'counsel-apropos
      "rl" 'ivy-resume
      "ry" 'counsel-yank-pop
      "sa" 'counsel-ag
      "ss" 'swiper)
    (general-def ivy-minibuffer-map
      "C-f" 'ivy-scroll-down-command
      "C-b" 'ivy-scroll-up-command
      "C-j" 'ivy-next-line
      "C-k" 'ivy-previous-line
      "C-l" 'ivy-alt-done
      "C-h" 'ivy-backward-delete-char))
  :config
  (ivy-mode 1))

(use-package ivy-hydra)

(use-package amx
  :config
  (amx-mode t))

(use-package ivy-xref
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;;; Magit et al.
(use-package magit
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :init
  (mah-leader
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

(use-package evil-magit
  :demand t
  :after magit)

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

(use-package anzu
  :config
  (progn
    (global-anzu-mode +1)
    (general-def
      :keymaps 'override
      [remap query-replace] 'anzu-query-replace
      [remap query-replace-regexp] 'anzu-query-replace-regexp
      )))

(use-package evil-anzu)

(use-feature autorevert
  :diminish 'auto-revert-mode)

(use-package avy
  :config
  (progn
    (mah-leader
      "jc" 'avy-goto-char-timer
      "jd" 'counsel-dired-jump
      "jl" 'avy-goto-line
      "jw" 'avy-goto-word-1
      )))

(use-package dash
  :config (dash-enable-font-lock))

(use-package diff-hl
  :config
  ;; (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (diff-hl-dired-mode)
  (diff-hl-margin-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote))

(use-feature dired
  :demand t
  :config
  (progn
    (general-nmap
     "-" 'dired-jump)
    (general-def 'dired-mode-map
      "-" 'dired-up-directory)
    (setq dired-listing-switches "-alh")))

(use-feature ediff
  :demand t
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
    "ep" 'flycheck-previous-error
    )
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))


(use-feature help
  :config (temp-buffer-resize-mode))

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
  :config
  ;; TODO investigate these modes.
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook #'indent-spaces-mode))

(use-package lispy
  :diminish lispy-mode
  :config
  (lispy-set-key-theme '(lispy paredit c-digits)))

(use-package lispyville
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
                              ))
  (add-hook 'lispy-mode-hook #'lispyville-mode))

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
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))

(use-package projectile
  :init
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-directories "~/Dropbox/Books")
  (add-to-list 'projectile-globally-ignored-directories "~/.emacs.d/var/lsp-java/workspace")
  (mah-leader
    "pl" 'projectile-switch-project
    "pI" 'projectile-invalidate-cache
    "p'" 'projectile-run-eshell
    )
  (projectile-mode 1)
  )

(use-package counsel-projectile
  :init
  (mah-leader
    "pb" 'counsel-projectile-switch-to-buffer
    "pl" 'counsel-projectile-switch-project
    "pf" 'counsel-projectile-find-file
    "sp" 'counsel-projectile-ag
    )
  :config
  (counsel-projectile-mode))

(use-package recentf
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-feature reveal
  :diminish 'reveal-mode)

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :when (version< "25" emacs-version)
  :config (save-place-mode))

(use-feature simple
  :config (column-number-mode))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook #'indicate-buffer-boundaries-left))

(use-package tramp
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(use-package undo-tree
  :diminish 'undo-tree-mode)

(use-package wgrep)

(use-package which-key
  :demand t
  :diminish 'which-key-mode
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
    (concat mah-leader " b") "buffers"
    (concat mah-leader " e") "flycheck"
    (concat mah-leader " f") "files"
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

(setq mode-line-format
      ;; ("%e"
      ;;  mode-line-front-space
      ;;  mode-line-mule-info
      ;;  mode-line-client
      ;;  mode-line-modified
      ;;  mode-line-remote
      ;;  mode-line-frame-identification
      ;;  mode-line-buffer-identification
      ;;  "   "
      ;;  mode-line-position
      ;;  evil-mode-line-tag
      ;;  (vc-mode vc-mode)
      ;;  "  "
      ;;  mode-line-modes
      ;;  mode-line-misc-info
      ;;  mode-line-end-spaces)
      (list
       "%e"
       mode-line-front-space
       mode-line-mule-info
       mode-line-client
       mode-line-modified
       mode-line-remote
       mode-line-frame-identification
       mode-line-buffer-identification
       "   "
       mode-line-position
       evil-mode-line-tag
       (vc-mode vc-mode)
       "  "
       mode-line-modes
       mode-line-misc-info
       mode-line-end-spaces
       ))
;; (use-package moody
;;   :demand t
;;   :config
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode))

(use-package yasnippet
  :demand t
  :config
  (yas-global-mode 1))

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

(use-feature elisp-mode
  :init
  (progn
    (mah-company emacs-lisp-mode company-capf)
    (add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
    (add-hook 'emacs-lisp-mode-hook #'lispyville-mode))
  :config
  (mah-local-leader
    :keymaps 'emacs-lisp-mode-map
    "eb" 'eval-buffer
    "ef" 'eval-defun
    "er" 'eval-region
    "es" 'eval-last-sexp

    "hi" 'counsel-imenu))

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
  :config
  (progn
    (mah-local-leader 'clojure-mode-map
      "'" 'cider-jack-in
      "\"" 'cider-jack-in-cljs
      "="  'cider-format-buffer

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

      "tn" 'cider-test-run-ns-tests
      "tt" 'cider-test-run-test
      "tR" 'cider-test-show-report

      "Te" 'cider-enlighten-mode
      "Tt" 'cider-auto-test-mode
      )

    (mah-local-leader 'cider-repl-mode-map
      "sc" 'cider-repl-clear-buffer
      "sc" 'cider-repl-clear-output
      "sq" 'cider-quit
      "ss" 'cider-switch-to-last-clojure-buffer
      )

    (general-nmap 'clojure-mode-map
      "gd" 'cider-find-var
      "K" 'cider-doc)))

(use-package cider
  :config
  (progn
    (mah-local-leader 'cider-mode-mode)))

(use-package lsp-mode
  :demand t
  :init
  (setq lsp-inhibit-message t
        lsp-eldoc-render-all nil
        lsp-highlight-symbol-at-point nil)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  (require 'lsp-imenu)
  (defadvice xref-find-definitions (before add-evil-jump activate) (evil-set-jump))
  )

(use-package lsp-ui
  :demand t
  :init
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point)
  (lsp-ui-doc-enable nil)
  )

(use-package company-lsp
  :after company
  :demand t
  :init
  (add-hook 'java-mode-hook (lambda () (push 'company-lsp company-backends)))
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t ;; nil
        ;; company-transformers nil
        company-lsp-async t
        ))

;;; java
(use-package lsp-java
  :demand t
  :requires (lsp-ui-flycheck lsp-ui-sideline)
  :init
  (progn
    (mah-local-leader 'java-mode-map
      "="  'google-java-format-buffer
      "as" 'lsp-ui-sideline-apply-code-actions
      "aa" 'lsp-execute-code-action
      "an" 'lsp-java-actionable-notifications

      "bp" 'lsp-java-build-project
      "bu" 'lsp-java-update-project-configuration
      "bU" 'lsp-java-update-user-settings

      "db" 'dap-toggle-breakpoint
      "dda" 'dap-java-attach
      "ddj" 'dap-java-debug
      "ddr" 'dap-java-run
      "ddl" 'dap-debug-last-configuration
      "dl" 'dap-ui-list-sessions
      "dq" 'dap-disconnect
      "dn" 'dap-next
      "di" 'dap-step-in
      "dc" 'dap-continue
      "db" 'dap-toggle-breakpoint
      "do" 'dap-step-out
      "dss" 'dap-switch-session
      "dst" 'dap-switch-thread
      "dsf" 'dap-switch-stack-frame

      "ee" 'dap-eval
      "er" 'dap-eval-region
      "es" 'dap-eval-dwim

      "gg" '(xref-find-definitions :async true)
      "gG" 'xref-find-definitions-other-window
      "gi" 'lsp-goto-implementation
      "gr" 'xref-find-references
      "gt" 'lsp-goto-type-definition

      "ha" 'xref-find-apropos
      "hh" 'lsp-describe-thing-at-point
      "hi" 'counsel-imenu

      "rec" 'lsp-java-extract-to-constant
      "rel" 'lsp-java-extract-to-local-variable
      "rem" 'lsp-java-extract-method

      "rf" 'lsp-java-create-field
      "rl" 'lsp-java-create-local
      "ri" 'lsp-java-add-import
      "rI" 'lsp-java-organize-imports
      "rn" 'lsp-rename
      "rp" 'lsp-java-create-parameter

      "sr" 'lsp-restart-workspace

      "ug" 'lsp-ui-peek-find-definitions
      "ui" 'lsp-ui-peek-find-implementation
      "uI" 'lsp-ui-imenu
      "ur" 'lsp-ui-peek-find-references
      )

    (general-nmap 'java-mode-map
      "gd" '(xref-find-definitions :async true)
      "K" 'lsp-describe-thing-at-point
      )
    :config
    (add-hook 'java-mode-hook
              (lambda ()
                (progn
                  (setq c-basic-offset 2
                        lsp-java-format-enabled nil
                        lsp-java-save-action-organize-imports nil)

                  (flycheck-mode t)
                  (lsp-ui-flycheck-enable t)
                  (lsp-ui-sideline-mode t)
                  (lsp-ui-doc-enable nil)
                  (lsp-java-enable))))
    (setq lsp-java-workspace-dir (no-littering-expand-var-file-name "lsp-java/workspace/")
          lsp-java-workspace-cache-dir (no-littering-expand-var-file-name "lsp-java/workspace/.cache/")
          lsp-java-server-install-dir (no-littering-expand-var-file-name "lsp-java/server")
          lsp-java--workspace-folders (list "/Users/mhinshaw/workspace/kollective/kollective_connect/"
                                            "/Users/mhinshaw/workspace/kollective/merge-db-streams/"
                                            "/Users/mhinshaw/workspace/kollective/db-source-merge/"
                                            "/Users/mhinshaw/workspace/kollective/delivery-state/"
                                            "/Users/mhinshaw/workspace/kollective/delivery-ktable/"
                                            "/Users/mhinshaw/workspace/kollective/prod3-history-fix/"
                                            "/Users/mhinshaw/workspace/kollective/kafka-dash/"
                                            "/Users/mhinshaw/workspace/kollective/redshift-loader/"
                                            "/Users/mhinshaw/workspace/java/streams/"
                                            "/Users/mhinshaw/workspace/java/kafka-connect-storage-cloud/"
                                            "/Users/mhinshaw/workspace/java/kafka-connect-jdbc/"
                                            ))))

(use-package dap-mode
  :straight (dap-mode :type git
                      :host github
                      :repo "yyoncho/dap-mode")
  :config
  (add-hook 'java-mode-hook (lambda ()
                              (progn
                                (dap-mode t)
                                (dap-ui-mode t)))))

(require 'google-java-format)
(setq google-java-format-executable "/usr/local/bin/google-java-format")
(add-hook 'java-mode-hook
	  (lambda ()
	    (add-hook 'before-save-hook #'google-java-format-buffer nil 'local)))

(use-package groovy-mode)

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
(use-package lsp-python
  :demand t
  :init
  (mah-local-leader '(python-mode-map)
    "=" 'lsp-format-buffer
    "as" 'lsp-ui-sideline-apply-code-actions
    "aa" 'lsp-execute-code-action

    "gg" '(xref-find-definitions :async true)
    "gG" 'xref-find-definitions-other-window
    "gi" 'lsp-goto-implementation
    "gr" 'xref-find-references
    "gt" 'lsp-goto-type-definition

    "ha" 'xref-find-apropos
    "hh" 'lsp-describe-thing-at-point

    "rn" 'lsp-rename

    "sr" 'lsp-restart-workspace
    "sR" 'cquery-freshen-index

    "ug" 'lsp-ui-peek-find-definitions
    "ui" 'lsp-ui-peek-find-implementation
    "uI" 'lsp-ui-imenu
    "ur" 'lsp-ui-peek-find-references)
  :config
  (add-hook 'python-mode-hook #'lsp-python-enable))

;; golang
(use-package go-mode
  :init
  (setq gofmt-command "goimports")
  (mah-local-leader '(go-mode-map)
    "ga" 'go-goto-arguments
    "gD" 'go-goto-docstring
    "gf" 'go-goto-function
    "gg" 'godef-jump
    "gG" 'godef-jump-other-window
    "gn" 'go-goto-function-name
    "gr" 'go-goto-return-values
    "gm" 'go-goto-method-receiver

    "hh" 'godef-describe)
  (general-nmap '(go-mode-map)
    "gd" 'godef-jump
    "K" 'godef-describe)
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets/yasnippet-go"
                                                   user-emacs-directory)))

;; (use-package company-go
;;   :init
;;   (add-to-list 'company-backends 'company-go))

;; (use-package go-eldoc
;;   :init
;;  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package lsp-go
  :demand t
  :requires (lsp-ui-flycheck lsp-ui-sideline)
  :init
  (mah-local-leader '(go-mode-map)
    "=" 'lsp-format-buffer
    "as" 'lsp-ui-sideline-apply-code-actions
    "aa" 'lsp-execute-code-action

    "gg" '(xref-find-definitions :async true)
    "gG" 'xref-find-definitions-other-window
    "gi" 'lsp-goto-implementation
    "gr" 'xref-find-references
    "gt" 'lsp-goto-type-definition

    "ha" 'xref-find-apropos
    "hh" 'lsp-describe-thing-at-point

    "rn" 'lsp-rename

    "sr" 'lsp-restart-workspace
    "sR" 'cquery-freshen-index

    "ug" 'lsp-ui-peek-find-definitions
    "ui" 'lsp-ui-peek-find-implementation
    "uI" 'lsp-ui-imenu
    "ur" 'lsp-ui-peek-find-references)
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (progn
                              (lsp-go-enable)
                              (flycheck-mode t)
                              (lsp-ui-flycheck-enable t)
                              (lsp-sideline-mode t)))
            ))

;; C/C++
(defun cquery//enable ()
  "Enable cquery or throw an error."
  (condition-case nil
      (lsp-cquery-enable)
    (user-error nil)))

(use-package cquery
  :commands lsp-cquery-enable
  :init
  (setq cquery-executable (executable-find "cquery")
        cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack" :completion (:detailedLabel t))
        ;; alternatively, (setq cquery-sem-highlight-method 'overlay)
        cquery-sem-highlight-method 'font-lock)

  (mah-local-leader
    :keymaps '(c++-mode-map c-mode-map)
    "=" 'lsp-format-buffer
    "as" 'lsp-ui-sideline-apply-code-actions
    "aa" 'lsp-execute-code-action

    "gb" '(cquery-xref-find-custom "$cquery/base")
    "gc" '(cquery-xref-find-custom "$cquery/callers")
    "gg" '(xref-find-definitions :async true)
    "gG" 'xref-find-definitions-other-window
    "gi" 'lsp-goto-implementation
    "gr" 'xref-find-references
    "gt" 'lsp-goto-type-definition
    "gv" '(cquery-xref-find-custom "$cquery/vars")

    "ha" 'xref-find-apropos
    "hh" 'lsp-describe-thing-at-point
    "hH" 'cquery-member-hierarchy
    "hi" 'counsel-imenu

    "rn" 'lsp-rename

    "sr" 'lsp-restart-workspace
    "sR" 'cquery-freshen-index

    "ub" '(lsp-ui-peek-find-custom 'base "$cquery/base")
    "uc" '(lsp-ui-peek-find-custom 'callers "$cquery/callers")
    "ug" 'lsp-ui-peek-find-definitions
    "ui" 'lsp-ui-peek-find-implementation
    "uI" 'lsp-ui-imenu
    "ur" 'lsp-ui-peek-find-references
    "uv" '(lsp-ui-peek-find-custom 'random "$cquery/random") ;; jump to a random declaration
    )
  (general-nmap
    :keymaps '(c++-mode-map c-mode-map)
    "gd" '(xref-find-definitions :async true)
    "K" 'lsp-describe-thing-at-point)

  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json"
                    ".cquery")
                  projectile-project-root-files-top-down-recurring)))

  (defun mah/cquery-hook ()
    (progn
      (cquery//enable)
      (flycheck-mode t)
      (lsp-ui-flycheck-enable t)
      (lsp-sideline-mode t)))
  (add-hook 'c-mode-hook #'mah/cquery-hook)
  (add-hook 'c++-mode-hook #'mah/cquery-hook)
  :config
  ;; For rainbow semantic highlighting
  (cquery-use-default-rainbow-sem-highlight))

(use-package json-reformat)
(use-package json-snatcher)
(use-package json-navigator)

(defun mah/json-reformat-buffer ()
  "Format from `point-min' to `point-max'."
  (interactive)
  (save-excursion (json-reformat-region (point-min) (point-max))))

(use-package json-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.avsc$" . json-mode))
  (setq json-reformat:pretty-string? t)
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

;; Ruby
(use-package chruby
  :demand t
  :config
  ;; run chruby at startup for tooling.
  (progn (chruby-use-corresponding)))

;; Terraform
(use-package terraform-mode
  :config (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package company-terraform
  :config (add-hook 'terraform-mode-hook #'company-terraform-init))

;; Ansible
(use-package jinja2-mode
  ;; :mode ("\\.j2\\'" .jinja2-mode)
  :init )

(use-package yaml-mode
  ;; (add-to-list 'auto-mode-alist '("\\(group_vars/.+\\|host_vars/.+\\)" . yaml-mode))
  )

(use-package ansible)

(use-package ansible-doc
  :init
  (add-hook 'ansible-mode-hook #'ansible-doc-mode))

(use-package company-ansible
  :init
  (add-to-list 'company-backends 'company-ansible))

(require 'mah-org)

;; epub
(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  ;; (add-hook 'nov-mode-hook (lambda () (evil-set-initial-state 'nov-mode 'emacs)))
  (general-nmap 'nov-mode-map
    ;; "" 'nov-render-document
    "v" 'nov-view-source
    "V" 'nov-view-content-source
    "m" 'nov-display-metadata
    "J" 'nov-next-document
    "]" 'nov-next-document
    "K" 'nov-previous-document
    "[" 'nov-previous-document
    "t" 'nov-goto-toc
    "RET" 'nov-browse-url
    ;; "<follow-link>" 'mouse-face
    ;; "<mouse-2>" 'nov-browse-url
    "TAB" 'shr-next-link
    "M-TAB" 'shr-previous-link
    "<backtab>" 'shr-previous-link
    "SPC" 'nov-scroll-up
    "S-SPC" 'nov-scroll-down
    "DEL" 'nov-scroll-down
    ;; "<home>" 'beginning-of-buffer ;; implemented with `gg'
    ;; "<end>" 'end-of-buffer ;; implemented with `G'
    )
  )

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
