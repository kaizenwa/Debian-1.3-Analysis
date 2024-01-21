;;;
;;; tm-mh-e3.el --- tm-mh-e module for mh-e 3.* to emulate mh-e 4.*.
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1993 .. 1996 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;;	    (This module was imported from mh-e 4.1)
;;; Created: 1994/7/10
;;; Version:
;;;	$Id: tm-mh-e3.el,v 7.2 1996/07/22 18:24:08 morioka Exp $
;;; Keywords: mail, MH, mh-e 3, MIME, multimedia, encoded-word, multilingual
;;;
;;; This file is part of tm (Tools for MIME).
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with This program.  If not, write to the Free Software
;;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Code:

(require 'mh-e)

(defvar mh-send-prog "send"
  "Name of the MH send program.
Some sites need to change this because of a name conflict.")

(defvar mail-citation-hook nil
  "*Hook for modifying a citation just inserted in the mail buffer.
Each hook function can find the citation between point and mark.
And each hook function should leave point and mark around the citation
text as modified.")


;;; Ensure new buffers won't get this mode if default-major-mode is nil.
(put 'mh-show-mode 'mode-class 'special)

(defun mh-show-mode ()
  "Major mode for showing messages in mh-e.
The value of mh-show-mode-hook is called when a new message is displayed."
  (kill-all-local-variables)
  (setq major-mode 'mh-show-mode)
  (mh-set-mode-name "MH-Show")
  (run-hooks 'mh-show-mode-hook))

(defun mh-start-of-uncleaned-message ()
  ;; position uninteresting headers off the top of the window
  (let ((case-fold-search t))
    (re-search-forward
     "^To:\\|^From:\\|^Subject:\\|^Date:" nil t)
    (beginning-of-line)
    (mh-recenter 0)))

(fset 'mh-show-msg (symbol-function 'mh-show))


;;; @ mh-comp definitions
;;;

(provide 'mh-comp)

(defun mh-read-address (prompt)
  ;; Read a To: or Cc: address, prompting in the minibuffer with PROMPT.
  ;; May someday do completion on aliases.
  (read-string prompt))

(defvar mh-forward-subject-format "%s: %s"
  "*Format to generate the Subject: line contents for a forwarded message.
The two string arguments to the format are the sender of the original
message and the original subject line.")

(defun mh-forwarded-letter-subject (from subject)
  ;; Return a Subject suitable for a forwarded message.
  ;; Original message has headers FROM and SUBJECT.
  (let ((addr-start (string-match "<" from))
	(comment (string-match "(" from)))
    (cond ((and addr-start (> addr-start 0))
	   ;; Full Name <luser@host>
	   (setq from (substring from 0 (1- addr-start))))
	  (comment
	   ;; luser@host (Full Name)
	   (setq from (substring from (1+ comment) (1- (length from)))))))
  (format mh-forward-subject-format from subject))


;;; @ end
;;;

(provide 'tm-mh-e3)

;;; tm-mh-e3.el ends here
