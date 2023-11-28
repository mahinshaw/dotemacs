;;; package --- early-init.el -*- lexical-binding: t -*-
;;; Commentary:
;; * Kill package.el
;;; Code:
;; Must use early init file as of Emacs 27
;; improves startup speed when using an alternative package manager
(setq package-enable-at-startup nil
      ;; this is no longer necessary
      ;; package--init-file-ensured t
      )

;; bells and flash no more
(setq ring-bell-function 'ignore)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
;; can check later that `package--initialized' is nil
;;; early-init.el ends here
