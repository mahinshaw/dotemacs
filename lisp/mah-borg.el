;;; pacakge --- mah-borg -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(push (concat user-emacs-directory "lib/borg") load-path)
(require  'borg)
(borg-initialize)

(progn ;    `use-package'
  (require  'use-package)
  (setq use-package-verbose t
        use-package-always-defer t))

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
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(provide 'mah-borg)
;;; mah-borg.el ends here
