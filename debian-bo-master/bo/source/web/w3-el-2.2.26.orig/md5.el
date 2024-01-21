;;; md5.el,v --- MD5 functionality for emacsen without it builtin
;; Author: wmperry
;; Created: 1995/04/14 23:47:48
;; Version: 1.2
;; Keywords: mail, news, tools, hypermedia
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994, 1995 by William M. Perry (wmperry@spry.com)
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1995 by William M. Perry (wmperry@spry.com)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This code is ripped off from vm-pop.el
;; A short-term

(defvar md5-program "md5"
  "*Program that reads a message on its standard input and writes an
MD5 digest on its output.")

(defun md5 (object &optional start end)
  (let ((buffer nil))
    (unwind-protect
	(save-excursion
	  (setq buffer (generate-new-buffer "*md5-work*"))
	  (set-buffer buffer)
	  (cond
	   ((bufferp object)
	    (insert-buffer-substring object start end))
	   ((stringp object)
	    (insert (if (or start end)
			(substring object start end)
		      object)))
	   (t nil))
	  (call-process-region (point-min) (point-max)
			       (or shell-file-name "/bin/sh")
			       t buffer nil
			       "-c" md5-program)
	  ;; MD5 digest is 32 chars long
	  ;; mddriver adds a newline to make neaten output for tty
	  ;; viewing, make sure we leave it behind.
	  (buffer-substring (point-min) (+ (point-min) 32)))
      (and buffer (kill-buffer buffer)))))

(provide 'md5)
