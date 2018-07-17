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
  (setq user-init-file (or load-file-name buffer-file-name)
        user-emacs-directory (file-name-directory user-init-file))

  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  ;; (package-initialize)
  (setq inhibit-startup-buffer-menu t
        inhibit-startup-screen t
        inhibit-startup-echo-area-message "locutus"
        initial-buffer-choice t
        initial-scratch-message ""
        load-prefer-newer t)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(push (concat user-emacs-directory "lisp") load-path)
(push (concat user-emacs-directory "lib/borg") load-path)
(require  'borg)
(borg-initialize)

(require 'mah-macros)
(require 'mah-defs)

(progn ;    `use-package'
  (require  'use-package)
  (setq use-package-verbose t))


(use-package auto-compile
  :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package general
  :config
  (progn
    (general-evil-setup)
    ;; (setq general-override-states '(normal visual motion))
    (general-override-mode)))

(use-package no-littering)

(use-package server
  :config (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Long tail

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

(require 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(load-theme 'doom-spacegrey t)

(set-face-attribute 'default nil :font "Source Code Pro Semibold" :height 140)

;;; General emacs

(when mah-is-mac
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta
        ns-use-native-fullscreen nil))

(general-def :keymaps 'override "M-RET" 'toggle-frame-fullscreen)

(mah-leader
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bd" 'kill-this-buffer
  "fs" 'save-buffer
  "fed" 'mah-find-init-file

  "hdc" 'describe-char
  "hdf" 'describe-function
  "hdF" 'describe-face
  "hdk" 'describe-key
  "hdm" 'describe-mode
  "hdv" 'describe-variable
  "u" 'universal-argument)

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



;;; Evil mode and related
(use-package evil
  :init
  (setq evil-want-C-u-scroll t
        evil-want-integration nil
        evil-normal-state-cursor '("DarkGoldenrod2" box)
        evil-insert-state-cursor '("chartreuse3" (bar . 2))
        evil-emacs-state-cursor '("SkyBlue3" box)
        evil-replace-state-cursor '("chocolate" (hbar . 2))
        evil-visual-state-cursor '("gray" (hbar . 2))
        evil-motion-state-cursor '("plum3" box)
        )
  :config
  (evil-mode))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer nil) ;; TODO this messes with helm bindings.
  :init
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;;; Company

(use-package company
  :defer t
  :diminish (company-mode . " C")
  :init
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match nil
        company-tooltip-align-annotations t
        company-tooltip-minimum-width 60)
  :config
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
      "C-h" 'ivy-backward-delete-char
      )
    (with-eval-after-load 'projectile
      (setq projectile-completion-system 'ivy))
    )
  :config
  (ivy-mode 1))

;;; Magit et al.
(use-package magit
  :defer t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :init
  (mah-leader
    "gs" 'magit-status)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package evil-magit
  :after magit)

;;; General tooling
(use-package ace-window
  :defer t
  :config
  (progn
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (mah-leader
      "ws" 'ace-swap-window
      "ww" 'ace-window)))

(use-package anzu
  :defer t
  :config
  (progn
    (global-anzu-mode +1)
    (general-def
      :keymaps 'override
      [remap query-replace] 'anzu-query-replace
      [remap query-replace-regexp] 'anzu-query-replace-regexp
      )))

(use-package avy
  :defer t
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
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote))

(use-package dired
  :defer t
  :config
  (progn
    (mah-no-pref
     "-" 'dired-jump)
    (general-def 'dired-mode-map
      "-" 'dired-up-directory)
    (setq dired-listing-switches "-alh")))

(use-package eldoc
  :when (version< "25" emacs-version)
  :config (global-eldoc-mode))

(use-package files
  :config
  (setq backup-by-copying t
        delete-old-versions t
        kept-new-versions 10
        kept-old-versions 0))
(use-package flycheck :diminish (flycheck-mode ."f")
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


(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package lisp-mode
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
  :defer t
  :diminish '(lispyville-mode . "()")
  :init
  (lispyville-set-key-theme '(operators
                              slurp/barf-cp
                              wrap))
  (add-hook 'lispy-mode-hook #'lispyville-mode))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package paren
  :config (show-paren-mode))

(use-package prog-mode
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))

(use-package projectile
  :config
  (setq projectile-enable-caching t)
  (mah-leader
    "pl" 'projectile-switch-project
    "pI" 'projectile-invalidate-cache
    )
  (projectile-mode 1)
  )

(use-package counsel-projectile
  :init
  (mah-leader
    "pb" 'counsel-projectile-switch-buffer
    "pl" 'counsel-projectile-switch-project
    "pf" 'counsel-projectile-find-file
    "sp" 'counsel-projectil-ag
    )
  :config
  (counsel-projectile-mode))

(use-package recentf
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :when (version< "25" emacs-version)
  :config (save-place-mode))

(use-package simple
  :config (column-number-mode))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook #'indicate-buffer-boundaries-left))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(use-package which-key
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

(use-package with-editor
  :defer t
  :diminish with-editor-mode)

;;; language specific

(use-package elisp-mode
  :defer t
  :diminish '(emacs-lisp-mode . "Elisp")
  :init
  (progn
    (mah-local-leader
      :keymaps 'emacs-lisp-mode-map
      "es" 'eval-last-sexp
      "ef" 'eval-defun
      "eb" 'eval-buffer)
    (mah-company emacs-lisp-mode company-capf)
    (add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
    (add-hook 'emacs-lisp-mode-hook #'lispy-mode)))

(use-package elisp-slime-nav
  :diminish 'elisp-slime-nav-mode
  :init
  (progn
    (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
      (add-hook hook 'elisp-slime-nav-mode))
    (general-nmap emacs-lisp-mode-map
      "gd" 'elisp-slime-nav-find-elisp-thing-at-point
      "K" 'elisp-slime-nav-describe-elisp-thing-at-point)
    ))

(use-package clojure-mode
  :defer t
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

      )
    (general-nmap 'clojure-mode-map
      "gd" 'cider-find-var
      "K" 'cider-doc)
    ))

(use-package cider
  :defer t
  :config
  (progn
    (mah-local-leader 'cider-mode-mode)))

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

(load custom-file)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
