;;; w3-emacs.el,v --- Emacs 18.xx specific functions for emacs-w3
;; Author: wmperry
;; Created: 1995/06/14 23:09:05
;; Version: 1.19
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
;;; Normal Emacs Specific Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun face-list () nil)

(defun w3-create-faces ()
  "Create faces, the dumb emacs 18 way"
  nil)

(defun w3-find-specific-link (link)
  "Find LINK in the current document"
  (let* ((thezones w3-zones-list))
    (while (and thezones
		(not
		 (equal link
			(car-safe (cdr (car (cdr (cdr (car thezones)))))))))
      (setq thezones (cdr thezones)))
    (if thezones (goto-char (car (car thezones)))
      (message "Link %s was not found." link))))

(fset 'w3-zone-eq 'eq)
(defun w3-zone-hidden-p (start end)
  "Return t iff the region from start to end is invisible."
  nil)

(defun w3-unhide-zone (start end)
  "Make a region from START TO END visible. (emacs18-unfunctional)"
  nil)

(defun w3-hide-zone (start end)
  "Make a region from START to END invisible. (emacs18-nonfunctional)"
  nil)

(defun w3-add-zone (start end style data &optional highlight)
  "Add a zone (normal emacs)"
  (if (memq (car data) '(w3 w3form w3graphic))
      (cond
       ((or (null w3-zones-list)
	    (< start (car (car w3-zones-list))))
	(setq w3-zones-list (cons (list (copy-marker start)
					(copy-marker end)
					data) w3-zones-list)))
       (t
	(let ((zones w3-zones-list))
	  (while (and (cdr zones)
		      (< (car (car (cdr zones))) start))
	    (setq zones (cdr zones)))
	  (setcdr zones (cons (list
			       (copy-marker start)
			       (copy-marker end)
			       data)
			      (cdr zones))))))))

(defun w3-extend-zone (zone new-end)
  (set-marker (nth 1 (car zone)) new-end))

(defun w3-all-zones ()
  "Return all the zones in this buffer."
  w3-zones-list)

(defun w3-zones-list ()
  "Return a list of zones for this buffer"
  w3-zones-list)

(defun w3-zone-at (pt)
  "Return zone (if any) at buffer position PT"
  (let ((zones w3-zones-list))
    (while (and zones
		(not (and
		      (>= (car (cdr (car zones))) pt)
		      (<= (car (car zones)) pt))))
      (setq zones (cdr zones)))
    (if zones (car zones)
      nil)))

(defun w3-delete-zone (zone)
  "Delete zone ZONE in this buffer."
  (let ((tmp w3-zones-list)
	(val '()))
    (while tmp
      (if (not (eq (car tmp) zone))
	  (setq val (append val (list (car tmp)))))
      (setq tmp (cdr tmp)))
    (setq w3-zones-list val)))

(defun w3-zone-end (zone)
  "Return the ending position of zone ZONE"
  (marker-position (car (cdr zone))))

(defun w3-zone-start (zone)
  "Return the starting position of zone ZONE"
  (marker-position (car zone)))

(defun w3-fix-extent-endpoints ()
  "Not done yet"
  (let ((x (w3-all-zones))
        st nd ch st-marker nd-marker)
    (while x
      (setq st-marker (car (car x))
            nd-marker (car (cdr (car x)))
            st (marker-position st-marker)
            nd (marker-position nd-marker))
      (while (memq (char-after (1- nd)) '(9 13 10 32))
        (setq nd (1- nd)
              ch t))
      (if ch (set-marker nd-marker nd))
      (setq ch nil)
      (while (memq (char-after st) '(9 13 10 32))
	(setq st (1+ st)
	      ch t))
      (if ch (set-marker st-marker st))
      (setq x (cdr x)))))

(defun w3-next-zone (zone)
  "Return zone (if any) after ZONE"
  (let ((zones w3-zones-list))
    (while (and zones
		(not (and (equal (car (cdr (car zones)))
				 (car (cdr zone)))
			  (equal (car (car zones))
				 (car zone)))))
      (setq zones (cdr zones)))
    (while (eq (car (nth 2 (car zones))) 'w3graphic)
      (setq zones (cdr zones)))
    (if (cdr zones)
	(car (cdr zones))
      nil)))

(defun w3-previous-zone (zone)
  "Return zone (if any) before ZONE"
  (let ((zones w3-zones-list)
	(last nil))
    (while (not (eql (car zones) zone))	; Get to current zone
      (if (eq (car (nth 2 (car zones))) 'w3graphic)
	  nil				; Don't keep track of graphic zones
	(setq last (car zones)))
      (setq zones (cdr zones)))
    (if zones
	last
      nil)))

(defun w3-zone-data (zone)
  "Return the data segment from zone ZONE"
  (car (cdr (cdr zone))))

(defvar w3-ignore-links '(w3graphic)
  "*List of link types to skip when w3-forward-link or
w3-backward-link is called.")

(defun w3-forward-link (p)
  "Go forward 1 link"
  (interactive "P")
  (setq p (or p 1))
  (if (< p 0)
      (w3-back-link (- p))
    (if (/= 1 p)
	(w3-forward-link (1- p)))
    (let ((zones w3-zones-list))
      (while (and zones
		  (or (<= (car (car zones)) (point))
		      (memq (car (w3-zone-data (car zones)))
			    w3-ignore-links)))
	(setq zones (cdr zones)))
      (if zones
	  (progn
	    (goto-char (car (car zones)))
	    (while (looking-at "[ \t\n]+") (forward-char 1)))
	(error "No more links.")))))

(defun w3-back-link (p)
  "Go back 1 link"
  (interactive "P")
  (setq p (or p 1))
  (if (< p 0)
      (w3-forward-link (- p))
    (if (/= 1 p)
	(w3-back-link (1- p)))
    (cond
     ((null w3-zones-list)
      (error "No links in this document."))
     ((> (car (cdr (car w3-zones-list))) (point))
      (error "No previous link"))
     (t
      (let* ((zones w3-zones-list)
	     (last-zone (if (memq (car (w3-zone-data (car zones)))
				       w3-ignore-links)
			    nil
			  zones)))
	(while (and (cdr zones)
		    (< (car (cdr (car (cdr zones)))) (point)))
	  (if (not (memq (car (w3-zone-data (car (cdr zones))))
			 w3-ignore-links))
	      (setq last-zone (cdr zones)))
	  (setq zones (cdr zones)))
	(if (null last-zone)
	    (error "No previous link"))
	(goto-char (car (car last-zone)))
	(while (looking-at "[ \t\n]+") (forward-char 1)))))))

(defun w3-follow-inlined-image ()
  "Follow an inlined image, regardless of whether its a link or not."
  (interactive)
  (let* ((zn (w3-zone-at (point))))
    (cond
     ((null zn) (error "Not on a link!"))
     ((eq (car zn) 'w3graphic) (url-maybe-relative (nth 1 zn)))
     (t (error "No inlined image at point.")))))

(defvar w3-old-mouse-function-cm nil "Old Ctrl-middle mouse binding.")
(defvar w3-old-mouse-function-m nil "Old Ctrl-middle mouse binding.")

(defun w3-follow-inlined-image-mouse (arg)
  "Follow a mouse over an inlined image.  If buffer is not in w3-mode, then
call function 'w3-fold-mouse-function-cm"
  (x-mouse-set-point arg)
  (if (eq major-mode 'w3-mode)
      (w3-follow-inlined-image)
    (funcall w3-old-mouse-function-cm arg)))

(defun w3-follow-mouse (arg)
  "Follow a mouse key in emacs 18, if buffer is not in W3-mode, then
call function 'w3-old-mouse-function-m"
  (x-mouse-set-point arg)
  (if (eq major-mode 'w3-mode)
      (w3-follow-link)
    (funcall w3-old-mouse-function-m arg)))

(defun w3-setup-version-specifics ()
  "Set up routine for emacs 18/NeXTemacs"
  (fset 'w3-insert 'insert-before-markers)
  (cond
   ((and (fboundp 'define-mouse)
	 (eq system-type 'next-mach))
    (require 'w3-next))
   ((eq system-type 'Apple-Macintosh) (require 'w3-mac))
   ((eq (device-type) 'x)		; Xwindows specific stuff
    (and (fboundp 'x-popup-menu)
	 (fset 'w3-x-popup-menu 'x-popup-menu))
    (require 'x-mouse)
    (fset 'w3-old-mouse-function-m (lookup-key mouse-map x-button-middle))
    (fset 'w3-old-mouse-function-cm (lookup-key mouse-map x-button-c-middle))
    (define-key mouse-map x-button-middle 'w3-follow-mouse)
    (define-key mouse-map x-button-c-middle 'w3-follow-inlined-image-mouse)
    )
   ((eq (device-type) 'intuition)	; Amiga specific stuff
    ;; Need anything here?
    )
   (t nil)))

(defun w3-store-in-x-clipboard (str)
  "Store string STR in the window systems cut buffer"
  (cond
   ((and (eq (device-type) 'x) (fboundp 'x-store-cut-buffer))
    (x-store-cut-buffer str))
   ((eq (device-type) 'intuition)
    )
   (t
    )))

(defun w3-mode-version-specifics ()
  "Emacs 18 specific stuff for w3-mode"
  nil)

(defun w3-map-links (function &optional buffer from to maparg)
  "Map FUNCTION over the hypertext links which overlap region in BUFFER,
starting at FROM and ending at TO.  FUNCTION is called with the arguments
linkdata, START, END, and MAPARG.
The arguments FROM, TO, MAPARG, and BUFFER default to the beginning of
BUFFER, the end of BUFFER, nil, and (current-buffer), respectively.

In emacs17, FROM, TO, and BUFFER are ignored.... working on it."
  (mapcar (function (lambda (x)
		      (if (eq (car (w3-zone-data x)) 'w3)
			  (funcall function (w3-zone-data x)
				   (w3-zone-start x)
				   (w3-zone-end x)
				   maparg))
		      nil)) (w3-all-zones))
  nil)

(provide 'w3-emacs)
