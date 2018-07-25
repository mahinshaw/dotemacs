;;; package --- mah-macros.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; straight.el/use-package
(defmacro use-feature (pkg &rest args)
  "A macro to make sure that straight will not import the PKG.
This should be used for Emacs features/built-ins.
ARGS will be passed to `use-package'."
  (declare (indent 1))
  `(use-package ,pkg
     :straight nil
     ,@args))
;;; Company

(defvar mah-company-global-backends nil
  "List of backends to enable everywhere.")

(defmacro mah-company (mode &rest backends)
  "Run `company-mode' in MODE with BACKENDS."
  (declare (indent 1))
  (let ((funcname (intern (format "mah-company-%s" mode)))
        (hookname (intern (format "%s-hook" mode))))
    `(progn
       (defun ,funcname ()
         (company-mode)
         (setq-local company-backends
                     (list (append ',backends mah-company-global-backends))))
       (add-hook ',hookname ',funcname))))

(provide 'mah-macros)
;;; mah-macros.el ends here
