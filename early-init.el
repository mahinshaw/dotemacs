;;; package --- early-init.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Early init configs

;;; Startup docs https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html#Startup-Summary

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

;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html
(add-to-list 'default-frame-alist
             '(font . "SauceCodePro Nerd Font-14"))

;; can check later that `package--initialized' is nil
;;; early-init.el ends here
