;;; pg-formatter.el --- emacs access to pg_formatter

;; Copywrite (C) 2018 Mark Hinshaw

;; Authors: Mark Hinshaw <mahinshaw@gmail.com>
;; Maintainers: Mark Hinshaw <mahinshaw@gmail.com>
;; Version: $Id: pg-formatter.el,v 0.1 2018/09/20

;; Keyworkds: sql, postgresql
;; URL: https://github.com/mahinshaw/pg-formatter.el

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Format sql files for Postgresql

;;; Code:

(defgroup pg-formatter nil
  "Minor mode to format JS code on file save"
  :group 'languages
  :prefix "pg-formatter"
  :link '(url-link :tag "Repository" "https://github.com/darold/pgFormatter"))

(defcustom pg-formatter-command "pg_format"
  "The 'pg_format' command."
  :type 'string
  :group 'pg-formatter)

(defcustom pg-formatter-args '("-u" "1")
  "List of args to send to pg_format command."
  :type '(repeat string)
  :group 'pg-formatter)

(defun pg-format ()
  "Format sql files intended for postgresql."
  (interactive)
  (let* ((ext (file-name-extension buffer-file-name t))
	 (bufferfile (make-temp-file "pg_format" nil ext))
	 (coding-system-for-read 'utf-8)
	 (coding-system-for-write 'utf-8))
    (unwind-protect
	(save-restriction
	  (widen)
	  (write-region nil nil bufferfile)
	  (if (zerop (apply 'call-process
			    pg-formatter-command
			    bufferfile
			    (list (list :file buffer-file-name))
			    nil
			    pg-formatter-args
			    ))
	      (progn
		(revert-buffer :ignore-auto :no-confirm)
		(message "Applied pg_format with args `%s'" pg-formatter-args))
	    (message "Could not apply pg_format")))
      (delete-file bufferfile))))

;;;###autoload
(define-minor-mode pg-formatter-mode
  "Execute pg_formatter on file save when this mode is turned on."
  :lighter " pgfmt"
  :global nil
  (if pg-formatter-mode
      (add-hook 'before-save-hook 'pg-format nil 'local)
    (remove-hook 'before-save-hook 'pg-format 'local)))

(provide 'pg-formatter)

;;; pg-formatter.el ends here
