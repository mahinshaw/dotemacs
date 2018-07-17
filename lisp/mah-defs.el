;;; package --- mah-defs.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defvar mah-is-mac (equal system-type 'darwin))
(defvar mah-is-windows (equal system-type 'windows-nt))
(defvar mah-is-linux (equal system-type 'gnu/linux))

(defvar mah-leader "SPC")
(defvar mah-local-leader ",")

(defun mah-find-init-file ()
  "Find the init file."
  (interactive)
  (find-file user-init-file))

(provide 'mah-defs)
;;; mah-defs.el ends here
