;;; package --- mah-lsp.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'use-package)

(use-package lsp-mode
  :defer t)

(use-package lsp-ui
  :defer t
  :init
  (add-hook 'lsp-mode-hook #'lsp-ui-mode))

(use-package company-lsp
  :defer t
  :init
  (setq company-transformers nil
	company-lsp-async t
	company-lsp-cache-candidates nil))

(defun mah--java-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (require 'lsp-java)
        (setq lsp-java-format-enabled nil
              lsp-java-save-action-organize-imports nil)
        (lsp-java-enable))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dofile.")))

(use-package lsp-java
    :defer t
    :init (progn
            (add-hook 'java-mode-hook #'mah--java-lsetup-lsp)
            (require 'lsp-imenu)
            (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

            ;; (spacemacs/declare-prefix-for-mode 'java-mode "ma" "action")
            ;; (spacemacs/declare-prefix-for-mode 'java-mode "mb" "build")
            ;; (spacemacs/declare-prefix-for-mode 'java-mode "md" "debug")
            ;; (spacemacs/declare-prefix-for-mode 'java-mode "mdd" "process")
            ;; (spacemacs/declare-prefix-for-mode 'java-mode "mds" "switch")
            ;; (spacemacs/declare-prefix-for-mode 'java-mode "mg" "goto")
            ;; (spacemacs/declare-prefix-for-mode 'java-mode "mh" "documentation")
            ;; (spacemacs/declare-prefix-for-mode 'java-mode "mr" "refactor")
            ;; (spacemacs/declare-prefix-for-mode 'java-mode "mre" "extract")
            ;; (spacemacs/declare-prefix-for-mode 'java-mode "ms" "server")
            ;; (spacemacs/declare-prefix-for-mode 'java-mode "mu" "lsp-ui")

            (mah-local-leader 'java-mode-map
              ;; "gg" 'xref-find-definitions
              "="  'google-java-format-buffer
              ;; "as" 'lsp-action-retrieve-and-run
              "as" 'lsp-ui-sideline-apply-code-actions
              "aa" 'lsp-execute-code-action
              "an" 'lsp-java-actionable-notifications

              "bp" 'lsp-java-build-project
              "bu" 'lsp-java-update-project-configuration
              "bU" 'lsp-java-update-user-settings

              "db" 'dap-toggle-breakpoint
              "dda" 'dap-java-attach
              "ddj" 'dap-java-debug
              "ddr" 'dap-java-run
              "ddl" 'dap-debug-last-configuration
              "dl" 'dap-ui-list-sessions
              "dq" 'dap-disconnect
              "dn" 'dap-next
              "di" 'dap-step-in
              "dc" 'dap-continue
              "db" 'dap-toggle-breakpoint
              "do" 'dap-step-out
              "dss" 'dap-switch-session
              "dst" 'dap-switch-thread
              "dsf" 'dap-switch-stack-frame

              "ee" 'dap-eval
              "er" 'dap-eval-region
              "es" 'dap-eval-dwim

	      "gg" '(xref-find-definitions :async true)
              "gG" 'xref-find-definitions-other-window
              "gi" 'helm-imenu
              "gr" 'xref-find-references

              "ha" 'xref-find-apropos
              "hh" 'lsp-describe-thing-at-point

              "rec" 'lsp-java-extract-to-constant
              "rel" 'lsp-java-extract-to-local-variable
              "rem" 'lsp-java-extract-method

              "rf" 'lsp-java-create-field
              "rl" 'lsp-java-create-local
              "ri" 'lsp-java-add-import
              "rI" 'lsp-java-organize-imports
              "rn" 'lsp-rename
              "rp" 'lsp-java-create-parameter

              "sr" 'lsp-restart-workspace

              "ug" 'lsp-ui-peek-find-definitions
              "ui" 'lsp-ui-peek-find-implementation
              "ui" 'lsp-ui-imenu
              "ur" 'lsp-ui-peek-find-references
              )
            ;; (add-to-list 'spacemacs-jump-handlers-java-mode 'xref-find-definitions)
            ;; (defadvice xref-find-definitions (before add-evil-jump activate) (evil-set-jump))
            )
    :commands lsp-java-enable
    )

(use-package google-java-format
  :init
  (progn
    (add-hook 'java-mode-hook
	      (lambda ()
		(add-hook 'before-save-hook #'google-java-format-buffer nil 'local))))
  :config
  (progn
    ;; (setq google-java-format-executable (executable-find "google-java-format"))
    (setq google-java-format-executable "/usr/local/bin/google-java-format")))

(provide 'mah-lsp)
;;; mah-lsp.el ends here
