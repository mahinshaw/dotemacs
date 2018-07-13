;; Company

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
