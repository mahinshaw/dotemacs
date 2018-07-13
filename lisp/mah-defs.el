(defvar mah-is-mac (equal system-type 'darwin))
(defvar mah-is-windows (equal system-type 'windows-nt))
(defvar mah-is-linux (equal system-type 'gnu/linux))

(defun mah-find-init-file ()
  "Find the init file."
  (interactive)
  (find-file user-init-file))

(provide 'mah-defs)
