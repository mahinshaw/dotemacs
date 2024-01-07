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

(defun mah-insert-lambda-arrow-for-major-mode ()
  "Insert arrow for major mode.
eg -> or =>"
  (interactive)
  (let ((arrow (pcase major-mode
                 ('csharp-mode "=> ")
                 ('ruby-mode "=> ")
                 ((or 'js-mode 'js-ts-mode 'js2-mode 'rjsx-mode 'typescript-mode 'typescript-ts-mode 'web-mode 'js-ts-mode) "=> ")
		         (_ "-> ")
		         )))
    (insert arrow)))

(defun mah-refresh-buffer-optional-auto ()
  "Refresh buffer, optionally from autosave."
  (interactive)
  (revert-buffer nil :noconfirm))

(defun mah-refresh-buffer ()
  "Revert buffer to to state on disk."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(provide 'mah-defs)
;;; mah-defs.el ends here
