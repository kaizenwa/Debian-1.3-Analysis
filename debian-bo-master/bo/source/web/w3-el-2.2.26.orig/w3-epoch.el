;;; w3-epoch.el,v --- Epoch 4.x specific functions for emacs-w3
;; Author: wmperry
;; Created: 1995/08/25 01:05:49
;; Version: 1.33
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
;;; Epoch Enhancements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is necessary for buffer-styles to work correctly
(setq w3-persistent-variables (cons 'buffer-style w3-persistent-variables))

(defvar w3-style-cache nil "Mapping of face names to style objects")

(defun find-face (name)
  (assq name w3-style-cache))

(defun make-face (new-name &optional def-fn def-fg def-bg def-ulp)
  (or (find-face new-name)
      (let* ((face (make-style))
	     (name (symbol-name new-name))
	     (fn   (or (w3-get-resource (concat name ".attributeFont")
					"Face.AttributeFont") def-fn))
	     (fg   (or (w3-get-resource (concat name ".attributeForeground")
					"Face.AttributeForeground") def-fg))
	     (bg   (or (w3-get-resource (concat name ".attributeBackground")
					"Face.AttributeBackground") def-bg))
	     (ulp  (or (w3-get-resource (concat name ".attributeUnderline")
					"Face.AttributeUnderline") def-ulp)))
	(if fn
	    (condition-case ()
		(set-style-font face fn)
	      (error (w3-warn 'faces
			      (format "Font `%s' not found for face `%s'"
				      fn name)))))
	(if fg
	    (condition-case ()
		(set-style-foreground face fg)
	      (error (w3-warn 'faces "Color `%s' not allocated for face `%s'"
			      fg name))))
	(if bg
	    (condition-case ()
		(set-style-background face bg)
	      (error (w3-warn 'faces "Color `%s' not allocated for face `%s'"
			      bg name))))
	(if (and
	     (stringp ulp)
	     (member (downcase ulp) '("true" "on" "yes" "t")))
	    (set-style-underline face "white"))
	(setq w3-style-cache (cons (cons new-name face) w3-style-cache)))))

(defun face-list ()
  (mapcar 'car w3-style-cache))

(defun face-instance (face)
  (cdr-safe (assq face w3-style-cache)))

(defun set-face-font (face font)
  (let ((style (cdr-safe (assq face w3-style-cache))))
    (and style (set-style-font font))))

(defun set-face-foreground (face color)
  (let ((style (cdr-safe (assq face w3-style-cache))))
    (and style (set-style-foreground style color))))

(defun set-face-background (face color)
  (let ((style (cdr-safe (assq face w3-style-cache))))
    (and style (set-style-background style color))))

(defun w3-create-faces ()
  "Create the faces, the Epoch way"
  (make-face 'bold nil "green" nil nil)
  (make-face 'italic nil "pink" nil nil)
  (make-face 'bold-italic nil "springgreen" nil nil)
  (make-face 'w3-node-style nil "yellow" nil t)
  (make-face 'w3-default-style nil nil nil nil)
  (make-face 'w3-visited-node-style nil "red" nil nil))

(defvar w3-mouse-map (create-mouse-map))
(define-key w3-mode-map "i" 'w3-load-delayed-images)

(defun w3-find-specific-link (link)
  "Find LINK in the current document"
  (let* ((thezones (epoch::zones-in-region (point-min) (point-max))))
    (while (and thezones
		(not (equal link
			    (car-safe
			     (cdr (epoch::zone-data (car thezones)))))))
      (setq thezones (cdr thezones)))
    (if thezones
	(goto-char (zone-start (car thezones)))
      (message "Link %s was not found." link))))

(fset 'w3-delete-zone 'epoch::delete-zone)
(fset 'w3-zone-data 'epoch::zone-data)
(fset 'w3-zone-start 'epoch::zone-start)
(fset 'w3-zone-end 'epoch::zone-end)
(fset 'w3-zone-eq 'eq)
(fset 'w3-zone-at 'epoch::zone-at)

(defun w3-extend-zone (zone pt)
  (epoch::move-zone zone (zone-start zone) pt))

(defun w3-zone-hidden-p (start end)
  "Return t iff the region from start to end is invisible."
  nil)

(defun w3-unhide-zone (start end)
  "Make a region from START TO END visible. (epoch-unfunctional)"
  nil)

(defun w3-hide-zone (start end)
  "Make a region from START to END invisible. (epoch-nonfunctional)"
  nil)

(defun w3-all-zones ()
  "Return all the zones in this buffer."
  (epoch::zones-in-region (point-min) (point-max)))

(defun w3-forward-link (p)
  "Go forward 1 link"
  (interactive "P")
  (setq p (or p 1))
  (if (< p 0)
      (w3-back-link (- p))
    (if (/= 1 p)
	(w3-forward-link (1- p)))
    (let* ((thezones (epoch::zones-in-region 
		      (if (epoch::zone-at (point))
			  (1+ (epoch::zone-end (epoch::zone-at (point))))
			(point)) (point-max))))
      (while (and thezones
		  (not (memq (car (epoch::zone-data (car thezones)))
			     '(w3 w3form))))
	(setq thezones (cdr thezones)))
      (if (car thezones)
	  (goto-char (epoch::zone-start (car thezones)))))))

(defun w3-back-link (p)
  "Go back 1 link"
  (interactive "P")
  (setq p (or p 1))
  (if (< p 0)
      (w3-forward-link (- p))
    (if (/= 1 p)
	(w3-back-link (1- p)))
    (let* ((thezones (epoch::zones-in-region
		      (point-min)
		      (if (epoch::zone-at (point))
			  (1- (epoch::zone-start (epoch::zone-at (point))))
			(point)))))
      (while (and thezones
		  (and
		   (equal (car-safe (epoch::zone-data (car (last thezones))))
			  'w3)
		   (memq (cdr-safe (epoch::zone-data (car (last thezones))))
			 '(style address header))))
	(setq thezones (butlast thezones 1)))
      (if (car thezones)
	  (goto-char (epoch::zone-start (car (last thezones))))))))

(defun w3-follow-mouse (mouse-data)
  "Follow the link under the mouse cursor"
  (interactive)
  (mouse::set-point mouse-data)
  (w3-follow-link))

(defun w3-fix-extent-endpoints ()
  "Make sure no extents have whitespace at the end of them."
  (let ((x (epoch::zones-in-region (point-min) (point-max))))
    (while x
      (let ((st (epoch::zone-start (car x)))
	    (nd (epoch::zone-end (car x))))
	(while (memq (char-after (1- nd)) '(?\t ?\r ?\n ?\ ))
	  (setq nd (1- nd)))
	(while (memq (char-after st) '(?\t ?\r ?\n ?\ ))
	  (setq st (1+ st)))
	(epoch::move-zone (car x) st nd))
      (setq x (cdr x)))))

(defun w3-follow-link ()
  "Attempt to follow link under cursor"
  (interactive)
  (let ((x (zones-in-region (point) (if (= (point) (point-max)) (point-max)
				      (1+ (point))) t))
	(data nil))
    (if x
	(progn
	  (while x
	    (setq data (epoch::zone-data (car x)))
	    (if (eq (car-safe data) 'w3form)
		(w3-do-form-entry data (car x))
	      (if (and (equal (car-safe data) 'w3)
		       (not (memq (car (cdr data))
				  '(address header style pic))))
		  (url-maybe-relative (car (cdr (cdr data))))))
	    (setq x (cdr x))))
      (progn
	(setq x (zone-at (point)))
	(if x
	    (progn
	      (setq data (zone-data x))
	      (if (eq (car-safe data) 'w3form) (w3-do-form-entry data x)
		(if (and (equal (car-safe data) 'w3)
			 (not (memq (car (cdr data))
				    '(address header style pic))))
		    (url-maybe-relative (car (cdr (cdr data)))))))
	  (message "Not on a link!"))))))

(defun w3-add-zone (start end style data &optional highlight)
  "Add highlighting (epoch)"
  (cond
   ((stylep style) nil)
   ((symbolp style)
    (setq style (cdr-safe (assq style w3-style-cache)))) 
   (t (setq style nil)))
  (let ((zone (add-zone start end style)))
    (epoch::set-zone-data zone data)
    zone))

(define-mouse w3-mouse-map mouse-middle mouse-down 'w3-follow-mouse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graphics handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (and (fboundp 'add-graphic-zone)
	 (fboundp 'epoch::read-pixmap-file))
    (defun w3-insert-graphic (name pt align alt)
      "Insert the graphic pointed to by the URL NAME, at buffer position POINT,
with alignment specified by ALIGN (one of 'center 'top or 'bottom).  If the
conversion of the picture fails for any reason, use ALT as the alternative
text.  If the reading of the pixmap is successful, the url and a pointer to
the pixmap are stored in w3-graphics-list for possible re-use later.

  If I can ever figure out how to get the color _NAME_ from epoch, I will
change this to grok bitmaps/pixmaps and change their background color to
that of the emacs screen.  Will look better that way.

  If epoch was not compiled with graphics zone support, this function
does nothing."
      (goto-char pt)
      (insert "^")
      (let ((bit nil)
	    (converter nil)
	    (add-to-list nil)
	    (lnk (cdr name))
	    (url-request-method "GET")
	    (url-be-asynchronous nil)
	    (url-request-data nil)
	    (url-mime-accept-string nil)
	    (w3-source t)
	    (url-request-extra-headers nil)
	    (fname (url-generate-unique-filename)))
	(setq name (car name)
	      url-mime-accept-string
	      (substring
	       (mapconcat
		(function
		 (lambda (x)
		   (if x (concat (car x) ",") ""))) w3-graphic-converter-alist
		   "")
	       0 -1))
	(save-excursion
	  (let ((w3-working-buffer " *W3GRAPH*"))
	    (if (assoc name w3-graphics-list)
		(progn
		  (message "Reusing image...")
		  (setq bit (cdr (assoc name w3-graphics-list))))
	      (progn
		(url-retrieve name)
		(setq add-to-list t)
		(w3-convert-graphic-to-useable-format w3-working-buffer
						      fname
						      nil)
		(message "Reading image %s..." url-current-file)
		(condition-case ()
		    (setq bit (epoch::read-pixmap-file fname))
		  (error nil))
		(condition-case ()
		    (delete-file fname)
		  (error nil))))))
	(and add-to-list
	     (setq w3-graphics-list
		   (cons (cons name bit) w3-graphics-list)))
	(if bit
	    (add-graphic-zone bit pt (1+ pt)
			      (cond
			       ((eq align 'top) 0)
			       ((eq align 'center) 50)
			       ((eq align 'bottom) 100)
			       (t 50))
			      '(w3 pic) (current-buffer))
	  (progn
	    (goto-char pt)
	    (delete-region pt (1+ pt))
	    (insert alt)
	    (w3-add-zone pt (point) nil (list 'w3graphic name) t))))))

(defun w3-create-hrule ()
  "Create a pixmap that is the width of the current buffer.  This
could use some work - not extremely pretty right now, but it works.

  If epoch was not compiled with graphics zone support, this function
returns nil, causing the function which calls it (w3-fix-horizontal-rules)
to draw a line with dashes."
  (if (not (fboundp 'read-pixmap-file)) nil
  (let ((width (- (window-pixwidth) 10))
	x bit f)
    (setq x (concat "/* XPM */\nstatic char * scratch [] = {\n"
		    (format "\"%d 4 2 1\",\n" width)
		    (format "\"       c %s\",\n" "gray80") 
		    (format "\".      c %s\",\n" "black")
		    (format "\"%s\",\n" (make-string width 32))
		    (format "\"%s\",\n" (make-string width ?.))
		    (format "\"%s\",\n" (make-string width ?.))
		    (format "\"%s\"};\n" (make-string width 32)))
	  f (url-generate-unique-filename)
	  bit (progn
		(save-excursion
		  (set-buffer (get-buffer-create " *tmp*"))
		  (erase-buffer)
		  (insert x)
		  (write-region (point-min) (point-max) f nil 5)
		  (kill-buffer " *tmp*")
		  (read-pixmap-file f))))
    bit)))

(defun w3-insert (&rest args)
  (let ((start (point))
	(zones (zones-at (point))))
    (prog1
	(apply 'insert-before-markers args)
      (mapcar (function (lambda (zone)
			  (if (equal (zone-start zone) start)
			      (move-zone zone (point) (zone-end zone)))))
	      zones))))

(defun w3-setup-version-specifics ()
  "Set up routine for Lucid emacs 19.9"
  nil)

(fset 'w3-store-in-x-clipboard 'epoch::store-cut-buffer)

(defun w3-map-links (function &optional buffer from to maparg)
  "Map FUNCTION over the hypertext links which overlap region in BUFFER,
starting at FROM and ending at TO.  FUNCTION is called with the arguments
linkdata, START, END, and MAPARG.
The arguments FROM, TO, MAPARG, and BUFFER default to the beginning of
BUFFER, the end of BUFFER, nil, and (current-buffer), respectively."
  (mapcar (function
	   (lambda (x)
	     (if (eq (car (w3-zone-data x)) 'w3)
		 (funcall function (w3-zone-data x)
			  (w3-zone-start x)
			  (w3-zone-end x)
			  maparg))
	     nil)) (epoch::zones-in-region (or from (point-min))
					   (or to (point-max))))
  nil)

(defun w3-mode-version-specifics ()
  "Epoch specific stuff for w3-mode"
  (use-local-mouse-map w3-mouse-map))

(provide 'w3-epoch)
