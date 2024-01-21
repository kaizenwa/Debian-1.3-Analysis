;;; font.el,v --- New font model
;; Author: wmperry
;; Created: 1995/10/28 02:09:08
;; Version: 1.19
;; Keywords: faces

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1995 by William M. Perry (wmperry@spry.com)
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
;;; The emacsen compatibility package - load it up before anything else
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (load-library "w3-sysdp"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lots of variables / keywords for use later in the program
;;; Not much should need to be modified
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defkeyword (keyword &optional docstring)
  (list 'defconst keyword (list 'quote keyword)
	(or docstring "A keyword")))

(defconst font-window-system-mappings
  '((x        . x-font-create-name)
    (ns       . ns-font-create-name)
    (tty      . nil))
  "An assoc list mapping device types to the function used to create
a font name from a font structure.")

(defconst ns-font-weight-mappings
  '((:extra-light . "extralight")
    (:light       . "light")
    (:demi-light  . "demilight")
    (:medium      . "medium")
    (:normal      . "normal")
    (:demi-bold   . "demibold")
    (:bold        . "bold")
    (:extra-bold  . "extrabold"))
  "An assoc list mapping keywords to actual NeXTstep specific
information to use")

(defconst x-font-weight-mappings
  '((:extra-light . "extralight")
    (:light       . "light")
    (:demi-light  . "demilight")
    (:medium      . "medium")
    (:normal      . "normal")
    (:demi-bold   . "demibold")
    (:bold        . "bold")
    (:extra-bold  . "extrabold"))
  "An assoc list mapping keywords to actual Xwindow specific strings
for use in the 'weight' field of an X font string.")

(defconst font-possible-weights
  (mapcar 'car x-font-weight-mappings))

(defvar font-rgb-file nil
  "Where the RGB file was found.")

(defvar font-maximum-slippage "1pt"
  "How much a font is allowed to vary from the desired size.")

(defvar font-family-mappings
  '(
    ("serif"        . ("garamond" "palatino" "times new roman" "baskerville"
		       "bookman" "bodoni" "computer modern" "rockwell"))
    ("sans-serif"   . ("gills-sans" "avant-garde" "univers" "helvetica"
		       "optima"))
    ("proportional" . ("courier" "lucidatypewriter" "fixed"))
    ("cursive"      . ("sirene" "zapf chancery"))
    )
  "A list of font family mappings.")

(defkeyword :family "Keyword specifying the font family of a FONTOBJ.")

(defkeyword :weight "Keyword specifying the font weight of a FONTOBJ.")
 (defkeyword :extra-light)
 (defkeyword :light)
 (defkeyword :demi-light)
 (defkeyword :medium)
 (defkeyword :normal)
 (defkeyword :demi-bold)
 (defkeyword :bold)
 (defkeyword :extra-bold)

(defkeyword :style "Keyword specifying the font style of a FONTOBJ.")
 (defkeyword :italic)
 (defkeyword :italics)
 (defkeyword :small-caps)
 (defkeyword :big-caps)

(defkeyword :size)

(defkeyword :registry "Keyword specifying the registry of a FONTOBJ.")
(defkeyword :encoding "Keyword specifying the encoding of a FONTOBJ.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun unique (list)
  (let ((retval)
	(cur))
    (while list
      (setq cur (car list)
	    list (cdr list))
      (if (member cur retval)
	  nil
	(setq retval (cons cur retval))))
    (nreverse retval)))

(defun font-higher-weight (w1 w2)
  (let ((index1 (length (memq w1 font-possible-weights)))
	(index2 (length (memq w2 font-possible-weights))))
    (cond
     ((<= index1 index2)
      (or w1 w2))
     ((not w2)
      w1)
     (t
      w2))))

(defun font-spatial-to-canonical (spec &optional device)
  "Convert SPEC (in inches, millimeters, points, or picas) into pixels"
  ;; 1 in = 25.4 mm = 72 pt = 6 pa
  (if (numberp spec)
      spec
    (let ((num nil)
	  (type nil)
	  ;; If for any reason we get null for any of this, default
	  ;; to 1024x768 resolution on a 17" screen
	  (pix-width (float (or (device-pixel-width device) 1024)))
	  (mm-width (float (or (device-mm-width device) 293)))
	  (retval nil))
      (if (string-match "[^0-9.]+$" spec)
	  (setq type (substring spec (match-beginning 0))
		spec (substring spec 0 (match-beginning 0)))
	(setq type "px"
	      spec spec))
      (setq num (string-to-number spec))
      (cond
       ((member type '("pixel" "px" "pix"))
	(setq retval num
	      num nil))
       ((member type '("point" "pt"))
	(setq retval (+ (* (/ pix-width mm-width)
			   (/ 25.4 72.0)
			   num))))
       ((member type '("pica" "pa"))
	(setq retval (* (/ pix-width mm-width)
			(/ 25.4 6.0)
			num)))
       ((member type '("inch" "in"))
	(setq retval (* (/ pix-width mm-width)
			(/ 25.4 1.0)
			num)))
       ((string= type "mm")
	(setq retval (* (/ pix-width mm-width)
			num)))
       ((string= type "cm")
	(setq retval (* (/ pix-width mm-width)
			10
			num)))
       (t (setq retval num))
       )
      retval)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The main interface routines - constructors and accessor functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-font (&rest args)
  (vector :family
	    (nth 1 (memq :family args))
	    :weight
	    (nth 1 (memq :weight args))
	    :style
	    (nth 1 (memq :style args))
	    :size
	    (nth 1 (memq :size args))
	    :registry
	    (nth 1 (memq :registry args))
	    :encoding
	    (nth 1 (memq :encoding args))))

(defsubst set-font-family (fontobj family)
  (aset fontobj 1 family))

(defsubst set-font-weight (fontobj weight)
  (aset fontobj 3 weight))

(defsubst set-font-style (fontobj style)
  (aset fontobj 5 style))

(defsubst set-font-size (fontobj size)
  (aset fontobj 7 size))

(defsubst set-font-registry (fontobj reg)
  (aset fontobj 9 reg))

(defsubst set-font-encoding (fontobj enc)
  (aset fontobj 11 enc))

(defsubst font-family (fontobj)
  (aref fontobj 1))

(defsubst font-weight (fontobj)
  (aref fontobj 3))

(defsubst font-style (fontobj)
  (aref fontobj 5))

(defsubst font-size (fontobj)
  (aref fontobj 7))

(defsubst font-registry (fontobj)
  (aref fontobj 9))

(defsubst font-encoding (fontobj)
  (aref fontobj 11))

(defun font-create-name (fontobj &optional device)
  (let* ((type (device-type device))
	 (func (cdr-safe (assq type font-window-system-mappings))))
    (and func (fboundp func) (funcall func fontobj device))))

(defun font-combine-fonts (fontobj-1 fontobj-2)
  (let ((retval (make-font))
	(size-1 (font-spatial-to-canonical (font-size fontobj-1)))
	(size-2 (font-spatial-to-canonical (font-size fontobj-2))))
    (set-font-weight retval (font-higher-weight (font-weight fontobj-1)
						(font-weight fontobj-2)))
    (set-font-family retval (font-family fontobj-1))
    (set-font-style retval (unique (append (font-style fontobj-1)
					   (font-style fontobj-2))))
    (set-font-registry retval (font-registry fontobj-1))
    (set-font-encoding retval (font-encoding fontobj-1))
    (set-font-size retval (if (>= size-2 size-1)
			      (font-size fontobj-2)
			    (font-size fontobj-1)))
    retval))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The window-system dependent code (X-style)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun x-font-families-for-device (&optional device no-resetp)
  (condition-case ()
      (require 'x-font-menu)
    (error nil))
  (or device (setq device (selected-device)))
  (if (boundp 'device-fonts-cache)
      (let ((menu (or (cdr-safe (assq device device-fonts-cache)))))
	(if (and (not menu) (not no-resetp))
	    (progn
	      (reset-device-font-menus device)
	      (x-font-families-for-device device t))
	  (let ((scaled (mapcar (function (lambda (x) (if x (aref x 0))))
				(aref menu 0)))
		(normal (mapcar (function (lambda (x) (if x (aref x 0))))
				(aref menu 1))))
	    (sort (unique (nconc scaled normal)) 'string-lessp))))
    (mapcar 'car font-family-mappings)))

(defun x-font-create-name (fontobj &optional device)
  (if (and (not (or (font-family fontobj)
		    (font-weight fontobj)
		    (font-size fontobj)
		    (font-registry fontobj)
		    (font-encoding fontobj)))
	   (not (memq :bold (font-style fontobj)))
	   (not (memq :italic (font-style fontobj)))
	   (not (memq :italics (font-style fontobj))))
      (face-font 'default)
    (let ((family (or (font-family fontobj)
		      (x-font-families-for-device device)))
	  (weight (or (font-weight fontobj) :medium))
	  (style (or (font-style fontobj) (list :normal)))
	  (size (font-size fontobj))
	  (registry (or (font-registry fontobj) "*"))
	  (encoding (or (font-encoding fontobj) "*")))
      (if (stringp family)
	  (setq family (list family)))
      (if (symbolp style)
	  (setq style (list style)))
      (setq weight (font-higher-weight weight (car-safe (memq :bold style))))
      (if (stringp size)
	  (setq size (round (font-spatial-to-canonical size device))))
      (setq weight (or (cdr-safe (assq weight x-font-weight-mappings))
		       "medium"))
      (let ((done nil)			; Did we find a good font yet?
	    (font-name nil)		; font name we are currently checking
	    (cur-family nil)		; current family we are checking
	    )
	(while (and family (not done))
	  (setq cur-family (car family)
		family (cdr family))
	  (if (assoc cur-family font-family-mappings)
	      ;; If the family name is an alias as defined by
	      ;; font-family-mappings, then append those families
	      ;; to the front of 'family' and continue in the loop.
	      (setq family (append
			    (cdr-safe (assoc cur-family
					     font-family-mappings))
			    family))
	    ;; Not an alias for a list of fonts, so we just check it.
	    ;; First, convert all '-' to spaces so that we don't screw up
	    ;; the oh-so wonderful X font model.  Wheee.
	    (let ((x (length cur-family)))
	      (while (> x 0)
		(if (= ?- (aref cur-family (1- x)))
		    (aset cur-family (1- x) ? ))
		(setq x (1- x))))
	    (setq font-name (format "-*-%s-%s-%s-*-*-%s-*-*-*-*-*-%s-%s"
				    cur-family weight
				    (if (or (memq :italic style)
					    (memq :italics style))
					"i"
				      "r")
				    (if size (int-to-string size) "*")
				    registry
				    encoding
				    )
		  done (try-font-name font-name device))))
	(if done font-name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The window-system dependent code (NS-style)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ns-font-families-for-device (&optional device no-resetp)
  ;; For right now, assume we are going to have the same storage for
  ;; device fonts for NS as we do for X.  Is this a valid assumption?
  (or device (setq device (selected-device)))
  (let ((menu (or (cdr-safe (assq device device-fonts-cache)))))
    (if (and (not menu) (not no-resetp))
	(progn
	  (reset-device-font-menus device)
	  (ns-font-families-for-device device t))
      (let ((scaled (mapcar (function (lambda (x) (if x (aref x 0))))
			    (aref menu 0)))
	    (normal (mapcar (function (lambda (x) (if x (aref x 0))))
			    (aref menu 1))))
	(sort (unique (nconc scaled normal)) 'string-lessp)))))

(defun ns-font-create-name (fontobj &optional device)
  (let ((family (or (font-family fontobj)
		    (ns-font-families-for-device device)))
	(weight (or (font-weight fontobj) :medium))
	(style (or (font-style fontobj) (list :normal)))
	(size (font-size fontobj))
	(registry (or (font-registry fontobj) "*"))
	(encoding (or (font-encoding fontobj) "*")))
    ;; Create a font, wow!
    (if (stringp family)
	(setq family (list family)))
    (if (symbolp style)
	(setq style (list style)))
    (setq weight (font-higher-weight weight (car-safe (memq :bold style))))
    (if (stringp size)
	(setq size (font-spatial-to-canonical size device)))
    (setq weight (or (cdr-safe (assq weight ns-font-weight-mappings))
		     "medium"))
    (let ((done nil)			; Did we find a good font yet?
	  (font-name nil)		; font name we are currently checking
	  (cur-family nil)		; current family we are checking
	  )
      (while (and family (not done))
	(setq cur-family (car family)
	      family (cdr family))
	(if (assoc cur-family font-family-mappings)
	    ;; If the family name is an alias as defined by
	    ;; font-family-mappings, then append those families
	    ;; to the front of 'family' and continue in the loop.
	    (setq family (append
			  (cdr-safe (assoc cur-family
					   font-family-mappings))
			  family))
	  ;; CARL: Need help here - I am not familiar with the NS font
	  ;; model
	  (setq font-name "UNKNOWN FORMULA GOES HERE"
		done (try-font-name font-name device))))
      (if done font-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now overwrite the original copy of set-face-font with our own copy that
;;; can deal with either syntax.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun font-set-face-font (face font &rest args)
  (cond
   ((vectorp font)
    (let ((font-name (font-create-name font)))
      (and (symbolp face)
	   (put face 'font-specification font))
      (if (null font-name)
	  nil
	(apply 'font-original-set-face-font face font-name args))))
   (t					; Let the original set-face-font
    (and (symbolp face)
	 (put face 'font-specification nil)) ; signal an error if bad data
    (apply 'font-original-set-face-font face font args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now for emacsen specific stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun font-update-device-fonts (device)
  ;; Update all faces that were created with the 'font' package
  ;; to appear correctly on the new device.  This should be in the
  ;; create-device-hook.  This is XEmacs 19.12+ specific
  (let ((faces (face-list 2))
	(cur nil)
	(font nil)
	(font-spec nil))
    (while faces
      (setq cur (car faces)
	    faces (cdr faces)
	    font-spec (get cur 'font-specification)
	    font (and font-spec (font-create-name font-spec device)))
      (cond
       (font
	(font-original-set-face-font cur font device))
       (font-spec
	(warn "Could not determine font for %s\n" cur))
       (t nil)))))

(defun font-update-one-face (face &optional device-list)
  ;; Update FACE on all devices in DEVICE-LIST
  ;; DEVICE_LIST defaults to a list of all active devices
  (setq device-list (or device-list (device-list)))
  (let* ((cur-device nil)
	 (font-spec (get face 'font-specification))
	 (font nil))
    (if (not font-spec)
	;; Hey!  Don't mess with fonts we didn't create in the
	;; first place.
	nil
      (while device-list
	(setq cur-device (car device-list)
	      device-list (cdr device-list))
	(if (not (device-live-p cur-device))
	    ;; Whoah!
	    nil
	  (setq font (and font-spec (font-create-name font-spec cur-device)))
	  (cond
	   (font
	    (font-original-set-face-font face font cur-device))
	   (font-spec
	    (warn "Could not determine font for %s on %s\n" face
		  (device-name cur-device)))
	   (t nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various color related things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond
 ((fboundp 'display-warning)
  (fset 'font-warn 'display-warning))
 ((fboundp 'w3-warn)
  (fset 'font-warn 'w3-warn))
 ((fboundp 'url-warn)
  (fset 'font-warn 'url-warn))
 ((fboundp 'warn)
  (defun font-warn (class message &optional level)
    (warn "(%s/%s) %s" class (or level 'warning) message)))
 (t
  (defun font-warn (class message &optional level)
    (save-excursion
      (set-buffer (get-buffer-create "*W3-WARNINGS*"))
      (goto-char (point-max))
      (save-excursion
	(insert (format "(%s/%s) %s\n" class (or level 'warning) message)))
      (display-buffer (current-buffer))))))

(defun font-lookup-rgb-components (color)
  "Lookup COLOR (a color name) in rgb.txt and return a list of RGB values.
The list (R G B) is returned, or an error is signaled if the lookup fails."
  (let ((lib-list (if (boundp 'x-library-search-path)
		      x-library-search-path
		    ;; This default is from XEmacs 19.13 - hope it covers
		    ;; everyone.
		    (list "/usr/X11R6/lib/X11/"
			  "/usr/X11R5/lib/X11/"
			  "/usr/lib/X11R6/X11/"
			  "/usr/lib/X11R5/X11/"
			  "/usr/local/X11R6/lib/X11/"
			  "/usr/local/X11R5/lib/X11/"
			  "/usr/local/lib/X11R6/X11/"
			  "/usr/local/lib/X11R5/X11/"
			  "/usr/X11/lib/X11/"
			  "/usr/lib/X11/"
			  "/usr/local/lib/X11/"
			  "/usr/X386/lib/X11/"
			  "/usr/x386/lib/X11/"
			  "/usr/XFree86/lib/X11/"
			  "/usr/unsupported/lib/X11/"
			  "/usr/athena/lib/X11/"
			  "/usr/local/x11r5/lib/X11/"
			  "/usr/lpp/Xamples/lib/X11/"
			  "/usr/openwin/lib/X11/"
			  "/usr/openwin/share/lib/X11/")))
	(file font-rgb-file)
	r g b)
    (if (not file)
	(while lib-list
	  (setq file (expand-file-name "rgb.txt" (car lib-list)))
	  (if (file-readable-p file)
	      (setq lib-list nil
		    font-rgb-file file)
	    (setq lib-list (cdr lib-list)
		  file nil))))
    (if (null file)
	(error "font-lookup-rgb-components: Can't find rgb.txt file.")
      (save-excursion
	(set-buffer (find-file-noselect file))
	(if (not (= (aref (buffer-name) 0) ? ))
	    (rename-buffer (generate-new-buffer-name " *rgb-tmp-buffer*")))
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char (point-min))
	    (if (re-search-forward (format "\t%s$" (regexp-quote color)) nil t)
		(progn
		  (beginning-of-line)
		  (setq r (* (read (current-buffer)) 256)
			g (* (read (current-buffer)) 256)
			b (* (read (current-buffer)) 256)))
	      (message "No such color: %s" color)
	      (font-warn 'html (format "No such color: %s" color))
	      (setq r 0
		    g 0
		    b 0))
	    (list r g b) ))))))

(defun font-hex-string-to-number (string)
  "Convert STRING to an integer by parsing it as a hexadecimal number."
  (let ((conv-list '((?0 . 0) (?a . 10) (?A . 10)
		     (?1 . 1) (?b . 11) (?B . 11)
		     (?2 . 2) (?c . 12) (?C . 12)
		     (?3 . 3) (?d . 13) (?D . 13)
		     (?4 . 4) (?e . 14) (?E . 14)
		     (?5 . 5) (?f . 15) (?F . 15)
		     (?6 . 6) 
		     (?7 . 7)
		     (?8 . 8)
		     (?9 . 9)))
	(n 0)
	(i 0)
	(lim (length string)))
    (while (< i lim)
      (setq n (+ (* n 16) (or (cdr (assq (aref string i) conv-list)) 0))
	    i (1+ i)))
    n ))

(defun font-parse-rgb-components (color)
  "Parse RGB color specification and return a list of integers (R G B).
#FEFEFE and rgb:fe/fe/fe style specifications are parsed."
  (let ((case-fold-search t)
	r g b str)
  (cond ((string-match "^#[0-9a-f]+$" color)
	 (cond
	  ((= (length color) 4)
	   (setq r (font-hex-string-to-number (substring color 1 2))
		 g (font-hex-string-to-number (substring color 2 3))
		 b (font-hex-string-to-number (substring color 3 4))
		 r (* r 4096)
		 g (* g 4096)
		 b (* b 4096)))
	  ((= (length color) 7)
	   (setq r (font-hex-string-to-number (substring color 1 3))
		 g (font-hex-string-to-number (substring color 3 5))
		 b (font-hex-string-to-number (substring color 5 7))
		 r (* r 256)
		 g (* g 256)
		 b (* b 256)))
	  ((= (length color) 10)
	   (setq r (font-hex-string-to-number (substring color 1 4))
		 g (font-hex-string-to-number (substring color 4 7))
		 b (font-hex-string-to-number (substring color 7 10))
		 r (* r 16)
		 g (* g 16)
		 b (* b 16)))
	  ((= (length color) 13)
	   (setq r (font-hex-string-to-number (substring color 1 5))
		 g (font-hex-string-to-number (substring color 5 9))
		 b (font-hex-string-to-number (substring color 9 13))))
	  (t (error "Invalid RGB color specification: %s" color))))
	((string-match "rgb:\\([0-9a-f]+\\)/\\([0-9a-f]+\\)/\\([0-9a-f]+\\)"
		       color)
	 (if (or (> (- (match-end 1) (match-beginning 1)) 4)
		 (> (- (match-end 2) (match-beginning 2)) 4)
		 (> (- (match-end 3) (match-beginning 3)) 4))
	     (error "Invalid RGB color specification: %s" color)
	   (setq str (match-string 1 color)
		 r (* (font-hex-string-to-number str)
		      (expt 16 (- 4 (length str))))
		 str (match-string 2 color)
		 g (* (font-hex-string-to-number str)
		      (expt 16 (- 4 (length str))))
		 str (match-string 3 color)
		 b (* (font-hex-string-to-number str)
		      (expt 16 (- 4 (length str)))))))
	(t
	 (font-warn 'html (format "Invalid RGB color specification: %s"
				color))
	 (setq r 0
	       g 0
	       b 0)))
  (list r g b) ))

(defun font-color-rgb-components (color)
  "Return the RGB components of COLOR as a list of integers (R G B).
16-bit values are always returned.
#FEFEFE and rgb:fe/fe/fe style color specifications are parsed directly
into their components.
RGB values for color names are looked up in the rgb.txt file.
The variable x-library-search-path is use to locate the rgb.txt file."
  (let ((case-fold-search t))
    (cond
     ((and (listp color) (floatp (car color)))
      (mapcar (function (lambda (x) (* x 255))) color))
     ((listp color)
      color)
     ((or (string-match "^#" color)
	  (string-match "^rgb:" color))
      (font-parse-rgb-components color))
     ((string-match "\\([0-9.]+\\)[ \t]\\([0-9.]+\\)[ \t]\\([0-9.]+\\)"
		    color)
      (let ((r (string-to-number (url-match color 1)))
	    (g (string-to-number (url-match color 2)))
	    (b (string-to-number (url-match color 3))))
	(if (floatp r)
	    (setq r (round (* 255 r))
		  g (round (* 255 g))
		  b (round (* 255 b))))
	(font-parse-rgb-components (format "#%02x%02x%02x" r g b))))
     (t
      (font-lookup-rgb-components color)))))

(defun font-normalize-color (color)
  "Return an RGB tuple, given any form of input.  If an error occurs, black
is returned."
  (apply 'format "#%04x%04x%04x" (font-color-rgb-components color)))

(defun font-set-face-background (face color &rest args)
  (cond
   ((and (vectorp color)		; A vector of rgb components
	 (= (length color) 3))
    (apply 'font-original-set-face-background face
	   (format "%04x04x04x" (aref color 0) (aref color 1) (aref color 2))
	   args))
   ((and (listp color)			; A list of rgb components
	 (= (length color) 3))
    (apply 'font-original-set-face-background face
	   (format "%04x04x04x" (nth 0 color) (nth 1 color) (nth 2 color))
	   args))
   (t
    (apply 'font-original-set-face-background face color args))))

(defun font-set-face-foreground (face color &rest args)
  (cond
   ((and (vectorp color)		; A vector of rgb components
	 (= (length color) 3))
    (apply 'font-original-set-face-foreground face
	   (format "%04x04x04x" (aref color 0) (aref color 1) (aref color 2))
	   args))
   ((and (listp color)			; A list of rgb components
	 (= (length color) 3))
    (apply 'font-original-set-face-foreground face
	   (format "%04x04x04x" (nth 0 color) (nth 1 color) (nth 2 color))
	   args))
   (t
    (apply 'font-original-set-face-foreground face color args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Do the actual overwriting of some functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro font-overwrite-fn (func)
  (` (let ((our-func (intern (format "font-%s" (, func))))
	   (new-func (intern (format "font-original-%s" (, func))))
	   (old-func (and (fboundp (, func)) (symbol-function (, func)))))
       (if (not (fboundp new-func))
	   (progn
	     (if old-func
		 (fset new-func old-func)
	       (fset new-func 'ignore))
	     (fset (, func) our-func))))))

(font-overwrite-fn 'set-face-foreground)
(font-overwrite-fn 'set-face-background)
(font-overwrite-fn 'set-face-font)

(provide 'font)
