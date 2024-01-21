;;; w3-mac.el,v --- Macintosh specific functions for emacs-w3
;; Author: wmperry
;; Created: 1995/05/27 02:50:00
;; Version: 1.15
;; Keywords: faces, help, mouse, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994, 1995 by William M. Perry (wmperry@spry.com)
;;;
;;; This file is part of GNU Emacs.
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
;;; Macintosh emacs specifics... these could kill your emacs
;;; Resistance is futile, you will be quickdrawn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar w3-main-menu-id nil "*Mac ID of the main WWW menu.")
(defvar w3-help-menu-id nil "*Mac ID of the WWW help menu.")
(defvar w3-navigate-menu-id nil "*Mac ID of the WWW navigation menu.")

(defun w3-install-menu-lucid-style (menudesc)
  "Install a lucid style menu on the mac version of emacs 18"
  (let ((mnu (NewMenu (get-unique-menu-ID) (car menudesc)))
	tmp)
    (setq menudesc (cdr menudesc))
    (while menudesc
      (setq tmp (car menudesc)
	    menudesc (cdr menudesc))
      (cond
       ((vectorp tmp)			; Menu description
	(AppendMenu mnu (aref tmp 0) 'w3-generic-mac-menu-callback))
       ((stringp tmp)			; Separator
	(AppendMenu mnu "(-" nil))
       ((listp tmp)			; Submenu
	nil)				; Ignore for now
       ((null tmp)			; Null??
	(AppendMenu mnu "(-" nil))
       (t (message "Bad menu descriptor %s" tmp))))
    (InsertMenu mnu buffers-menu)
    (DrawMenuBar)
    mnu))

(defun w3-find-action-by-name (item menu)
  (let (retval)
    (while (and menu (not retval))
      (if (equal (aref (car menu) 0) item)
	  (setq retval (aref (car menu) 1)))
      (setq menu (cdr menu)))
    retval))

(defun w3-generic-mac-menu-callback (menu item)
  (let ((s (make-string 256 0)) act)
    (GetItem menu item s)
    (setq s (mapconcat
	     (function
	      (lambda (x)
		(if (= 0 x) "" (char-to-string x)))) (substring s 1 nil) ""))
    (cond
     ((= menu w3-main-menu-id)
      )
     ((= menu w3-help-menu-id)
      )
     ((= menu w3-navigate-menu-id)
      ))
    (cond
     ((null act) (message "%s not found." s))
     ((symbolp act) (funcall act))
     ((listp act) (eval act))
     ((stringp act) (message "%s" act))
     (t (message "ACK! %s not found." s)))))

(provide 'w3-mac)
