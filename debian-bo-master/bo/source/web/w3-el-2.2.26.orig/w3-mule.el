;;; w3-mule.el,v --- MULE 18/19 specific functions for emacs-w3
;; Author: wmperry
;; Created: 1995/07/01 17:14:23
;; Version: 1.13
;; Keywords: faces, help, i18n, mouse, hypermedia

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
;;; Printing a mule buffer as postscript.  Requires m2ps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-m2ps-buffer (&optional buffer)
  "Print a buffer by passing it through m2ps and lpr."
  (or buffer (setq buffer (current-buffer)))
  (let ((x (save-excursion (set-buffer buffer) tab-width)))
    (save-excursion
      (set-buffer (get-buffer-create " *mule-print*"))
      (erase-buffer)
      (insert-buffer buffer)
      (if (/= x tab-width)
	  (progn
	    (setq tab-width x)
	    (message "Converting tabs")
	    (untabify (point-min) (point-max))))
      (setq file-coding-system *internal*)
      (shell-command-on-region (point-min) (point-max)
			       "m2ps | lpr" t))))
	    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multi-Lingual Emacs (MULE) Specific Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if w3-running-FSF19
    (progn
      (fset 'w3-create-faces-origunal (symbol-function 'w3-create-faces))
      (defun w3-create-faces ()
	"Create faces, the no-so-dumb MULE 2.x way"
	(w3-create-faces-origunal)
	(setq w3-delimit-emphasis nil)
	(setq w3-delimit-links nil)))
  (defun w3-create-faces ()
    "Create faces, the no-quite-so-dumb MULE 1.x way"
    (setq w3-delimit-links nil))
  )

(defvar attributed-region nil
  "Bogus definition to get rid of compile-time warnings.")

(defvar w3-mule-marker-attribute-alist 
  '(("SUBMIT"   . 'reverse)
    ("RESET"    . 'reverse)
    ("PASSWORD" . 'underline)
    ("OPTION"   . 'underline)
    (""         . 'underline))
  "Pairs of anchors' TYPEs and their display ATTRIBUTEs. For emacs18.")

(defun w3-mule-overlay-attribute (ovl)
  "Return a type of attribute for OVL. If OVL is not for
links/forms/images/headers, nil is returned. For emacs19."
  (let (ovp)
    (cond ((and (setq ovp (overlay-get ovl 'w3))
		(nth 1 ovp)) ;; has a HREF
	   'underline)
	  ((overlay-get ovl 'w3form) 'underline)
	  ((overlay-get ovl 'w3header) 'bold)
	  (t nil)
	  )))

(defun w3-mule-attribute-on-region (attr start end)
  "Turn on ATTR for the characters within the region, but white spaces."
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char start)
      (skip-chars-forward " \t")
      (setq start (point))
      (end-of-line)
      (while (< (point) end)
	(attribute-on-region attr start (point))
	(forward-line 1)
	(skip-chars-forward " \t")
	(setq start (point))
	(if (= (point) end) t (end-of-line)) )
      (attribute-on-region attr start end))))

(defun w3-mule-attribute-zones-19 (&optional zones)
  "Trace overlays in the current buffer and turn on an appropriate
attribute, if necessary. For Mule based on emacs19."
  (let ((pos (point-min)) (pmax (point-max)) l)
    (while (< pos pmax)
      (if (setq l (overlays-at pos))
	  (let ((c 0) ovl at)
	    (while (setq ovl (nth c l))
	      (if (setq at (w3-mule-overlay-attribute ovl))
		  (w3-mule-attribute-on-region
		   at (overlay-start ovl) (overlay-end ovl)))
	      (setq c (1+ c)))))
      (setq pos (next-overlay-change pos)))))

(defun w3-mule-attribute-zones-18 (zones)
  "Turn on an appropriate attribute for each marker in ZONES.
For Mule based on emacs18."
  (save-excursion
    (let ((c 0) l z type at beg end)
      (while (setq z (nth c zones))
	(setq type (nth 2 (nth 2 z)))
	(if (null type) t
	  (setq at (or (cdr (assoc type w3-mule-marker-attribute-alist)) 
		       w3-mule-attribute))
	  (w3-mule-attribute-on-region at (nth 0 z) (nth 1 z)))
	(setq c (1+ c))))))

(fset 'w3-mule-attribute-zones 
      (if w3-running-FSF19
	  'w3-mule-attribute-zones-19 
	'w3-mule-attribute-zones-18))

(defun w3-inhibit-code-conversion (proc buf)
  "Inhibit Mule's subprocess PROC from code converting in BUF."
  (save-excursion
    (set-buffer buf)
    (setq mc-flag nil))
  (set-process-coding-system proc *noconv* *noconv*))

(defconst w3-mime-list-for-code-conversion
  '("text/plain" "text/html")
  "List of MIME types that require Mules' code conversion.")

(defun w3-convert-code-for-mule (mmtype)
  "Convert current data into the appropriate coding system"
  (and (or (not mmtype) (member mmtype w3-mime-list-for-code-conversion))
       (let* ((c (code-detect-region (point-min) (point-max)))
	      (code (or (and (listp c) (car c)) c)))
	 (setq mc-flag t)
	 (code-convert-region (point-min) (point-max) code *internal*)
	 (set-file-coding-system code))))

(or (fboundp 'attribute-on-region)
    (defun attribute-on-region (attr from to)
      (let ((face (cond ((eq attr 'inverse) 'region)
			(t attr))))
	(add-text-properties from to (list 'face face)))))

(provide 'w3-mule)
