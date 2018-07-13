;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

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
    (setq general-override-states '(normal visual motion))
    (general-override-mode)))

(use-package no-littering)

(use-package server
  :config (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Long tail

;; leader map
(general-create-definer mah-leader
  :states '(normal motion)
  :keymaps 'override
  :prefix "SPC")

(general-create-definer mah-local-leader
  :states '(normal motion)
  :prefix ",")
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
  "u" 'universal-argument
  )

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
        evil-want-integration nil)
  :config
  (evil-mode))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer nil) ;; TODO this messes with helm bindings.
  :init
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

;;; Company

(use-package company
  :defer t
  :diminish (company-mode . "C")
  :init
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match nil
        company-tooltip-alight-annotations t
        company-tooltip-minimum-width 60)
  :config
  (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)
  (progn
    (global-company-mode)
    ))

;;; Ivy et al.
(use-package ivy
  :init
  (progn
    (setq ivy-use-virtual-buffers t
          ivy-count-format "(%d/%d) ")
    (mah-leader
      "SPC" 'counsel-M-x
      "bb" 'ivy-switch-buffer
      "ff" 'counsel-find-file
      "fr" 'counsel-recentf
      "ha" 'counsel-apropos
      "rl" 'ivy-resume
      "sa" 'counsel-ag
      "ss" 'swiper)
    (general-def ivy-minibuffer-map
      "C-j" 'ivy-next-line
      "C-k" 'ivy-previous-line
         ))
  :config
  (ivy-mode 1))

;;; General tooling
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
  :config (setq dired-listing-switches "-alh"))

(use-package eldoc
  :when (version< "25" emacs-version)
  :config (global-eldoc-mode))

(use-package flycheck
  :diminish (flycheck-mode ."f")
  :init
  (mah-leader
    "tf" 'flycheck-mode
    "el" 'flycheck-list-errors
    "eb" 'flycheck-buffer
    "ec" 'flycheck-clear))

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook #'indent-spaces-mode))

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
                          'append))

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
  :config (which-key-mode))

;;; language specific

(use-package elisp-mode
  :defer t
  :init
  (progn
    (mah-local-leader 
      :keymaps 'emacs-lisp-mode-map
      "es" 'eval-last-sexp
      "ef" 'eval-defun
      "eb" 'eval-buffer)
    (mah-company emacs-lisp-mode company-capf)))

(use-package elisp-slime-nav
  :init
  (progn
    (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
      (add-hook hook 'elisp-slime-nav-mode))
    (general-nmap emacs-lisp-mode-map
      "gd" 'elisp-slime-nav-find-elisp-thing-at-point
      "K" 'elisp-slime-nav-describe-elisp-thing-at-point)
    ))

(progn ;     startup
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
