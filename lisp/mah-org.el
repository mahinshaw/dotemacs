;;; mah-org.el --- org and friends  -*- lexical-binding: t -*-

;;; Commentary:

;;; Org mode needs fancy things when using straight.el

;;; Code:

(require 'subr-x)
(require 'use-package)
(require 'general)
(use-package git)

(defun org-git-version ()
  "The Git version of `org-mode'.
Inserted by installing `org-mode' or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of `org-mode'.
Inserted by installing `org-mode' or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

(use-package org ; or org-plus-contrib if desired
  :init
  (setq org-todo-keywords '((sequence "TODO" "DOING" "BLOCKED" "REVIEW" "|" "DONE" "CANCELLED")))
  ;; TODO - test out 'note some time. It may be useful
  (setq org-log-done 'time)
  (mah-local-leader 'org-mode-map
    "tt" 'org-todo
    "ih" 'org-insert-heading-after-current
    "iH" 'org-insert-heading
    )

  (with-eval-after-load 'org-capture

    (general-define-key
     :definer 'minor-mode
     :states 'normal
     :keymaps 'org-capture-mode
      ",c" 'org-capture-finalize
      ",w" 'org-capture-refile
      ",k" 'org-capture-kill
     )

    ;; Taken General.el README
    (general-define-key
     :keymaps 'org-capture-mode-map
     [remap evil-save-and-close]          'org-capture-finalize
     [remap evil-save-modified-and-close] 'org-capture-finalize
     [remap evil-quit]                    'org-capture-kill)))

(use-package org-bullets
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-projectile
  :init
  (mah-leader
    "oao" 'org-agenda-open-link
    "opt" 'org-projectile-project-todo-completing-read
    "oc" 'org-capture)

  :config
  (progn
    (setq org-projectile-projects-file "~/workspace/org/projects.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)))

(provide 'mah-org)
;;; mah-org.el ends here
