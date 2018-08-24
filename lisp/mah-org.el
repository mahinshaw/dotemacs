;;; mah-org.el --- org and friends  -*- lexical-binding: t -*-

;;; Commentary:

;;; Org mode needs fancy things when using straight.el

;;; Code:

(require 'subr-x)
(require 'use-package)
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

(use-package org) ; or org-plus-contrib if desired

(use-package org-bullets
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide 'mah-org)
;;; mah-org.el ends here
