;;; w3-draw.el,v --- Emacs-W3 drawing functions for new display engine
;; Author: wmperry
;; Created: 1995/10/28 04:16:16
;; Version: 1.288
;; Keywords: faces, help, hypermedia

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
;;; This function will take a stream of HTML from w3-preparse-buffer
;;; and draw it out
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'w3-vars)
(or (boundp 'MULE) (fset 'string-width 'length))

(defmacro w3-get-state (tag)
  (if (consp tag)
      (if (and (eq 'quote (car tag))
               (consp (cdr tag))
               (null (cdr (cdr tag))))
          (setq tag (car (cdr tag)))))
  (or (symbolp tag)
      (error "Bad argument: %s" tag))
  (let ((index (length (memq tag w3-state-locator-variable))))
    (` (aref w3-state-vector (, index)))))

(defmacro w3-put-state (tag val)
  (if (consp tag)
      (if (and (eq 'quote (car tag))
               (consp (cdr tag))
               (null (cdr (cdr tag))))
          (setq tag (car (cdr tag)))))
  (or (symbolp tag)
      (error "Bad argument: %s" tag))
  (let ((index (length (memq tag w3-state-locator-variable))))
    (` (aset w3-state-vector (, index) (, val)))))

(defmacro w3-get-default-style-info (info)
  (` (and w3-current-stylesheet
          (or
	   ;; Check for tag/class first!
	   (cdr-safe (assq (, info)
			   (cdr-safe
			    (assoc (cdr-safe (assq 'class args))
				   (cdr-safe
				    (assq tag w3-current-stylesheet))))))

	   ;; Then for global stuff with 'class'
	   (cdr-safe (assq (, info)
			   (cdr-safe
			    (assoc (cdr-safe (assq 'class args))
				   (cdr-safe
				    (assq 'doc w3-current-stylesheet))))))
     
	   ;; Fall back on the default styles for just this tag.
	   (cdr-safe (assq (, info)
			   (cdr-safe
			    (assq 'internal
				  (cdr-safe
				   (assq tag w3-current-stylesheet))))))))))

(defun w3-face-for-element ()
  (let ((x (w3-get-default-style-info 'face))
	(y (w3-get-default-style-info 'foreground))
	(z (w3-get-default-style-info 'background)))
    (cons tag (delq nil (list x y z (symbol-value
				     (cdr-safe (assq tag w3-all-faces))))))))

;; Hey, don't blame me!  Apply requires that its last argument be a list.
(defun w3-munge-color-fore (face color &optional locale)
  (cond
   ((valid-color-name-p color)
    (if locale
	(apply 'set-face-foreground face color (list locale))
      (apply 'set-face-foreground face (list color))))
   ((valid-color-name-p (concat "#" color))
    (if locale
	(apply 'set-face-foreground face (concat "#" color) (list locale))
      (apply 'set-face-foreground face (list (concat "#" color)))))
   ((string-match "[ \t\r\n]" color)
    (w3-munge-color-fore
     face
     (mapconcat (function (lambda (x) (if (memq x '(?\t ?\r ?\n ? )) ""
					(char-to-string x)))) color "")
     locale))
   (t 
    (w3-warn 'html (format "Bad color specification: %s" color)))))

;; Hey, don't blame me!  Apply requires that its last argument be a list.
(defun w3-munge-color-back (face color &optional locale)
  (cond
   ((valid-color-name-p color)
    (if locale
	(apply 'set-face-background face color (list locale))
      (apply 'set-face-background face (list color))))
   ((valid-color-name-p (concat "#" color))
    (if locale
	(apply 'set-face-background face (concat "#" color) (list locale))
      (apply 'set-face-background face (list (concat "#" color)))))
   ((string-match "[ \t\r\n]" color)
    (w3-munge-color-back
     face
     (mapconcat (function (lambda (x) (if (memq x '(?\t ?\r ?\n ? )) ""
					(char-to-string x)))) color "")
     locale))
   (t 
    (w3-warn 'html (format "Bad color specification: %s" color)))))

(defun w3-get-resource (name class)
  (cond
   (w3-running-xemacs
    (x-get-resource name class 'string))
   (w3-running-epoch
    (or
     (epoch::get-default (concat "Emacs*" name) class)
     (epoch::get-default (concat "epoch*" name) class)))
   ((fboundp 'get-resource)
    (get-resource name class))
   ((and (eq (device-type) 'x)
	 (fboundp 'x-get-resource))
    (x-get-resource name class))
   (t nil)))

(defun face-would-differ-from-default-p (facename)
  "Return non-nil iff face FACENAME would be different from the default face."
  (let* ((name (if (symbolp facename) (symbol-name facename) facename))
	 (fn  (w3-get-resource (concat name ".attributeFont")
			      "Face.AttributeFont"))
	 (fg  (w3-get-resource (concat name ".attributeForeground")
			      "Face.AttributeForeground"))
	 (bg  (w3-get-resource (concat name ".attributeBackground")
			      "Face.AttributeBackground"))
	 (bgp (w3-get-resource (concat name ".attributeBackgroundPixmap")
			       "Face.AttributeBackgroundPixmap"))
	 (ulp (let ((resource (w3-get-resource
			       (concat name ".attributeUnderline")
			       "Face.AttributeUnderline")))
		(if resource
		    (member (downcase resource) '("on" "true")) nil))))
    (or fn fg bg bgp ulp)))

(if (not (fboundp 'face-differs-from-default-p ))
    (fset 'face-differs-from-default-p 'face-would-differ-from-default-p))

(defun w3-pause ()
  (cond
   (w3-running-FSF19 (sit-for 0))
   (w3-running-xemacs
    (if (and (not (sit-for 0)) (input-pending-p))
	(condition-case ()
	    (dispatch-event (next-command-event))
	  (error nil))))
   (t (sit-for 0))))

(defvar w3-end-tags
  '((/ul   . ul)
    (/lit  . lit)
    (/li   . li)
    (/h1   . h1)
    (/h2   . h2)
    (/h3   . h3)
    (/h4   . h4)
    (/h5   . h5)
    (/h6   . h6)
    (/font0 . font0)
    (/font1 . font1)
    (/font2 . font2)
    (/font3 . font3)
    (/font4 . font4)
    (/font5 . font5)
    (/font6 . font6)
    (/font7 . font7)
    (/ol   . ol)
    (/dl   . dl)
    (/menu . menu)
    (/dir  . dir)
    (/a    . a)))

(defun w3-handle-single-tag (tag &optional args)
  (and (symbolp tag)
       (save-excursion
	 (and w3-draw-buffer (set-buffer w3-draw-buffer))
	 (let ((opos (point)))
	   (goto-char (point-max))
	   (if (and (w3-get-state 'next-break)
		    (not (memq tag
			       '(p h1 h2 h3 h4 h5 h6 ol ul dl menu dir pre))))
	       (w3-handle-p))
	   (w3-put-state 'next-break nil)
	   (setq w3-current-formatter (get tag 'w3-formatter))
	   (let ((data-before nil)
		 (data-after nil))
	     (if (and (not (eq tag 'text)) w3-current-stylesheet)
		 (progn
		   (setq data-before (w3-get-default-style-info
				      'insert.before))
		   (let ((tag (cdr-safe (assq tag w3-end-tags))))
		     (setq data-after (and tag
					   (w3-get-default-style-info
					    'insert.after))))))
	     (if data-before (w3-handle-single-tag 'text data-before))
	     (setq w3-current-formatter (get tag 'w3-formatter))
	     (cond
	      ((eq w3-current-formatter 'ack) nil)
	      ((null w3-current-formatter) (w3-handle-unknown-tag tag args))
	      (t (funcall w3-current-formatter args)))
	     (if data-after (w3-handle-single-tag 'text data-after)))
	   (if (not (eq tag 'text))
	       (setq w3-last-tag tag))
	   (goto-char opos)))))

(defun w3-draw-html (stream)
  (let (
	chunk				; Current 'chunk' of HTML
	tag				; The current HTML tag
	args				; Arguments to the html tag
	formatter			; The formatting function to call

	(len (length stream))		; Length of parsed html
	(ctr 0)				; How much we've parsed
	(fill-column (- (or w3-strict-width (window-width)) w3-right-border))
	)
    (set-buffer (get-buffer-create url-working-buffer))
    (erase-buffer)
    (w3-init-state)
    (setq w3-draw-buffer (current-buffer))
    (switch-to-buffer (current-buffer))
    (setq w3-last-fill-pos (point)
	  fill-prefix "")
    (while stream
      (if (= (% ctr 10) 0)
	  (if w3-do-incremental-display
	      (w3-pause)
	    (url-lazy-message "Drawing... %d%% done."
			      (url-percentage ctr len))))
      (setq w3-last-tag tag
	    ctr (1+ ctr)
	    chunk (car stream)
	    tag (car chunk)
	    args (cdr chunk)
	    stream (cdr stream)
	    formatter (get tag 'w3-formatter)
	    )
      (w3-handle-single-tag tag args))
    (save-excursion
      (goto-char (point-max))
      (w3-handle-paragraph))
    (w3-mode)
    (w3-handle-annotations)
    (w3-handle-headers)
    (if (boundp 'MULE) (w3-mule-attribute-zones w3-zones-list))
    (message "Drawing... done.")
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (let ((window nil)
	  (pop-up-windows nil))
      (switch-to-buffer (current-buffer))
      (display-buffer (current-buffer))
      (if (or w3-running-FSF19 w3-running-xemacs)
	  (setq window (get-buffer-window (current-buffer) t))
	(setq window (get-buffer-window (current-buffer))))
      (select-window window)
      (if (and (fboundp 'select-frame)
	       (fboundp 'window-frame))
	  (select-frame (window-frame window))))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set up basic fonts/stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		      
(defun w3-init-state ()
  ;; Reset the state of an HTML drawing buffer
  (setq w3-state-vector (copy-sequence w3-state-vector))
  (setq w3-current-stylesheet (copy-tree w3-user-stylesheet))
  (setq w3-form-labels nil)
  (if (not (get 'w3-state 'init)) (w3-draw-setup))
  (fillarray w3-state-vector 0)
  (w3-put-state 'bogus nil)		; Make all fake ones return nil
  (w3-put-state 'text-mangler nil)	; Any text mangling routine 
  (w3-put-state 'next-break nil)	; Next item needs a paragraph break
  (w3-put-state 'background nil)	; Netscapism - gag
  (w3-put-state 'table nil)		; Table args
  (w3-put-state 'figdata nil)		; Data for <fig> tag
  (w3-put-state 'figalt nil)		; Alt data for <fig> tag
  (w3-put-state 'pre-start nil)		; Where current <pre> seg starts
  (w3-put-state 'zone nil)		; Zone of current href?
  (w3-put-state 'center nil)		; netscape tag
  (w3-put-state 'select nil)		; Data for current select field
  (w3-put-state 'options nil)		; Options in current select field
  (w3-put-state 'nofill nil)		; non-nil if in pre or xmp
  (w3-put-state 'nowrap nil)		; non-nil if in <p nowrap>
  (w3-put-state 'href nil)		; Current link destination
  (w3-put-state 'name nil)		; Current link ID tag
  (w3-put-state 'image nil)		; Current image destination
  (w3-put-state 'mpeg nil)		; Current mpeg destination
  (w3-put-state 'form nil)		; Current form information
  (w3-put-state 'optarg nil)		; Option arguments
  (w3-put-state 'w3-graphic nil)	; Image stuff for non-xemacs
  (w3-put-state 'lists '())		; Types of list currently in.
  (w3-put-state 'align nil)		; Current alignment of paragraphs
  (w3-put-state 'title nil)		; Whether we can have a title or not
  (w3-put-state 'needspace 'never)	; Spacing info
  (setq w3-active-faces nil)		; Face attributes to use
  )

(defvar w3-rot13-display-table
  (if (fboundp 'make-display-table)
      (let ((table (make-display-table))
	    (i 0))
	(while (< i 26)
	  (aset table (+ i ?a) (vector (+ (% (+ i 13) 26) ?a)))
	  (aset table (+ i ?A) (vector (+ (% (+ i 13) 26) ?A)))
	  (setq i (1+ i)))
	table))
  "Char table for rot 13 display.")

(defun w3-draw-setup ()
  ;; Initialize stuff for drawing HTML.  This takes care of creating
  ;; faces if necessary and scaling fonts, etc.  Also sets up all the
  ;; character-level formatting handling functions from `w3-faces'

  ;; Convert the old style of list chars to our new symbol-based way
  (mapcar
   (function
    (lambda (x)
      (if (stringp (car x))
	  (setcar x (intern (downcase (car x)))))))
   w3-list-chars-assoc)

  ;; Convert the old style of style tags to our new symbol-based way
  (setq w3-style-tags-assoc
	(mapcar '(lambda (x)
		   (cons (intern (downcase (car x)))
			 (cdr x)))
		w3-style-chars-assoc))

  ;; Convert the old style of entities to our new symbol-based way
  (mapcar
   (function
    (lambda (x)
      (put 'w3-entities (read (substring (car x) 1 nil)) (cdr x))))
   w3-html-entities)

  ;; Convert the old style graphic entities to new symbol-base
  (mapcar
   (function
    (lambda (x)
      (put 'w3-entities (read (substring (car x) 1 nil)) (cdr (cdr x)))))
   w3-graphics-entities-alist)

  (if (fboundp 'make-face)
      (let ((faces (face-list)))
	;; Ensure that we have an underlined face (some versions of emacs
	;; do not supply one by default.
	(if (not (memq 'underline faces))
	    (make-face 'underline))
	(if (face-differs-from-default-p 'underline) nil
	  (cond
	   ((fboundp 'set-face-underline-p)
	    (funcall 'set-face-underline-p 'underline t))
	   (w3-running-epoch
	    (if (face-instance 'underline)
		(set-style-underline (face-instance 'underline) "white")))
	   (t (w3-warn 'faces "Could not create an underlined face."))))
	
	;; Create all the faces.
	;; To avoid creating a lot of copies of faces, we use the
	;; new `face-would-differ-from-default-p' function.  If the
	;; face is undefined, just store a pointer to the default face
	;; instead of creating a new face and copying the old one.
	;;
	;; This can lead to lossage under epoch, since we go by the
	;; X resources, not the actual faces (since you can't get to them
	;; directly), so if color or font allocation failed for some face
	;; we may lose.
	(mapcar
	 (function
	  (lambda (x)
	    (let ((varname (intern (format "w3-%s-style" (car x)))))
	      (if (face-would-differ-from-default-p (car x))
		  (progn
		    (set varname (car x))
		    (make-face (car x)))
		(set varname (cdr x)))
	      (make-variable-buffer-local varname)
	      (put varname 'variable-documentation
		   (concat "Face storage for <" (symbol-name (car x))
			   "> tags")))))
	 w3-faces)
	
	(make-face 'rot13)
	(if (fboundp 'set-face-property)
	    (set-face-property 'rot13 'display-table w3-rot13-display-table)
	  (w3-munge-color-fore 'rot13 "white")
	  (w3-munge-color-back 'rot13 "white"))
	
	;; Make sure that wired looks pretty ugly, even if there are
	;; no Xdefaults for it.
	;;
	;; This causes bad things to happen on Mono displays, so only
	;; do it if we are running on a color system.

	(setq w3-wired-style 'wired)
	(make-face 'wired)
	(if (and (not (eq 'mono (device-class)))
		 (not (face-differs-from-default-p 'wired)))
	    (progn
	      (w3-munge-color-fore 'wired "red")
	      (w3-munge-color-back 'wired "yellow")))

	;; Make sure we don't blink a non-w3 face
	(setq-default w3-blink-style 'blink)
	(make-face 'blink)
	(cond
	 ((face-differs-from-default-p 'blink) nil)
	 ((fboundp 'set-face-blinking-p)
	  (set-face-blinking-p 'blink t))
	 ((eq (device-type) 'tty) nil)
	 (t
	  (copy-face 'italic 'blink)))

	(make-face 'w3-graphic-face)
	(cond
	 ((face-differs-from-default-p 'w3-graphic-face) nil)
	 ((eq (device-type) 'tty) nil)
	 (t
	  (copy-face 'bold 'w3-graphic-face)))

	;; Do some fancy scaling of fonts if we can.
	;;
	;; We allow the user preferences in their XDefaults file will
	;; of course override anything we try to do here.
	;;
	(if (and (fboundp 'make-face-larger)
		 (or (not (fboundp 'device-list))
		     (memq 'x (mapcar 'device-type (device-list)))))
	    (let ((faces (face-list))
		  (face nil)
		  (amt nil))
	      (mapcar
	       (function
		(lambda (face-pair)
		  (setq face (car face-pair)
			amt  (cdr face-pair))
		  (if (and (memq face faces)
			   (face-differs-from-default-p face))
		      nil
		    (message "Scaling font for %s, please wait..."
			     (symbol-name face))
		    (make-face face)
		    (eval (list 'setq-default
				(intern
				 (concat "w3-" (symbol-name face) "-style"))
				(list 'quote face)))
		    (if (< amt 0)
			(mapcar (function (lambda (x)
					    (make-face-smaller face)))
				(make-list (abs amt) nil))
		      (mapcar (function (lambda (x) (make-face-larger face)))
			      (make-list (abs amt) nil))))))
	       '((h1    .  3)	 (h2    .  2)
		 (h3    .  0)	 (h4    . -1)
		 (h5    . -2)	 (h6    . -3)
		 (font0 . -3)    (font1 . -2)
		 (font2 . -1)	 (font3 .  0)
		 (font4 .  2)    (font5 .  4)
		 (font6 .  6)    (font7 .  8))))))
    (mapcar
     (function
      (lambda (x)
	(let ((varname (intern (format "w3-%s-style" (car x)))))
	  (set varname (cdr x))
	  (make-variable-buffer-local varname)
	  (put varname 'variable-documentation
	       (concat "Face storage for <" (symbol-name (car x))
		       "> tags (Not functional when not "
		       "compiled w/o a window system)")))))
     w3-faces))
		      
  (mapcar
   (function
    (lambda (x)
      (if (memq (car x) '(h1 h2 h3 h4 h5 h6 q
			     font0 font1 font2 font3 font4 font5 font6 font7))
	  nil
	(let* ((foo (car x))
	       (bar (intern (concat "/" (symbol-name foo)))))
	  (put foo 'w3-formatter 'w3-handle-emphasis)
	  (put bar 'w3-formatter 'w3-handle-emphasis-end)
	  (setq w3-end-tags (cons (cons bar foo) w3-end-tags))))))
   w3-faces)
  (put 'w3-state 'init t)
  (put 'blink 'w3-formatter 'w3-handle-blink)
  (put 'blink 'w3-formatter 'w3-handle-/blink)
  (cond
   ((null w3-do-blinking) (message "Won't do blinking text."))
   ((fboundp 'set-face-blinking-p)
    (set-face-blinking-p 'blink t))
   ((eq (device-type) 'tty)
    (message "Cannot do blinking text."))
   ((featurep 'itimer)
    (let ((timer (get-itimer "w3-blink")))
      (if timer (delete-itimer timer))
      (start-itimer "w3-blink" 'w3-invert-face 1 1)))
   ((or (featurep 'timer)
	(condition-case ()
	    (require 'timer)
	  (error nil)))
    (run-at-time 1 1 'w3-invert-face))
   (t (message "Cannot do blinking text.")))
  (w3-init-state))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mapping HTML tags to functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(put 'lit 'w3-formatter 'w3-handle-pre)
(put '/lit 'w3-formatter 'w3-handle-/pre)
(put 'li 'w3-formatter 'w3-handle-list-item)
(put 'ul 'w3-formatter 'w3-handle-list-opening)
(put 'ol 'w3-formatter 'w3-handle-list-opening)
(put 'dl 'w3-formatter 'w3-handle-list-opening)
(put '/dl 'w3-formatter 'w3-handle-list-ending)
(put '/ul 'w3-formatter 'w3-handle-list-ending)
(put '/ol 'w3-formatter 'w3-handle-list-ending)
(put 'menu 'w3-formatter 'w3-handle-list-opening)
(put '/menu 'w3-formatter 'w3-handle-list-ending)
(put 'dir 'w3-formatter 'w3-handle-list-opening)
(put '/dir 'w3-formatter 'w3-handle-list-ending)
(put 'dt 'w3-formatter 'w3-handle-table-term)
(put 'dd 'w3-formatter 'w3-handle-table-definition)
(put 'a 'w3-formatter 'w3-handle-hyperlink)
(put '/a 'w3-formatter 'w3-handle-hyperlink-end)
(put 'h1 'w3-formatter 'w3-handle-header)
(put 'h2 'w3-formatter 'w3-handle-header)
(put 'h3 'w3-formatter 'w3-handle-header)
(put 'h4 'w3-formatter 'w3-handle-header)
(put 'h5 'w3-formatter 'w3-handle-header)
(put 'h6 'w3-formatter 'w3-handle-header)
(put '/h1 'w3-formatter 'w3-handle-header-end)
(put '/h2 'w3-formatter 'w3-handle-header-end)
(put '/h3 'w3-formatter 'w3-handle-header-end)
(put '/h4 'w3-formatter 'w3-handle-header-end)
(put '/h5 'w3-formatter 'w3-handle-header-end)
(put '/h6 'w3-formatter 'w3-handle-header-end)
(put 'img 'w3-formatter 'w3-handle-image)
(put 'kill_sgml 'w3-formatter 'w3-handle-kill-sgml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The main drawing routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-unknown-tag (tag args)
  ;; A generic formatter for an unkown HTML tag.  This will only be
  ;; called if TAG was not found in the property list of `w3-formatters'.
  ;; If a function named `w3-handle-TAG' is defined, then it will be put
  ;; into the `w3-formatters' property list, so it will be found next time
  ;; the tag is run across.
  (if (numberp tag) (setq tag (intern (int-to-string tag))))
  (if (symbolp tag)
      (let ((handler (intern (concat "w3-handle-" (symbol-name tag)))))
	(if (= (string-to-char (symbol-name tag)) ?/)
	    (setq w3-end-tags (cons (cons tag
					  (intern (substring (symbol-name tag)
							     1)))
				    w3-end-tags)))
	(if (and handler (fboundp handler))
	    (if (w3-get-state 'table)
		(progn
		  (put tag 'w3-formatter 'w3-table-store-data)
		  (funcall 'w3-table-store-data args))
	      (put tag 'w3-formatter handler)
	      (funcall handler args))
	  (put tag 'w3-formatter 'ack)))))

(defun w3-handle-plaintext (&optional args)
  (let ((x (w3-get-state 'nofill)))
    (w3-put-state 'nofill t)
    (w3-handle-text (cdr-safe (assq 'data args)))
    (setq w3-last-fill-pos (point))
    (w3-put-state 'nofill x)))

(defun w3-handle-text (&optional args)
  ;; This is the main workhorse of the display engine.
  ;; It will figure out how a chunk of text should be displayed and
  ;; put all the necessary extents/overlays/regions around it."
  (cond
   ((null args) nil)
   ((string= args "")
    (setq args nil)
    (w3-put-state 'needspace nil))
   (t
    (let ((st (point))
	  (mangler (w3-get-state 'text-mangler))
	  (sym nil)
	  (nuke-leading-spaces
	   (and (= (string-to-char args) ? )
		(or (eq (w3-get-state 'needspace) 'never)
		    (and (bolp)
			 (not (w3-get-state 'nofill)))))))
      (insert args)
      (if nuke-leading-spaces
          (save-excursion
            (goto-char st)
            (delete-region (point)
                           (progn
                             (skip-chars-forward " ")
                             (point)))))
      (and mangler w3-delimit-emphasis
	   (fboundp mangler) (funcall mangler st (point)))
      (let ((faces nil)
	    (todo w3-active-faces)
	    (val nil)
	    (cur nil))
	(while todo
	  (setq cur (car todo)
		sym (cdr-safe (assq cur w3-all-faces))
		val (and sym (boundp sym) (symbol-value sym))
		todo (cdr todo))
	  (cond
	   ((and val (not (memq val faces)))
	    (setq faces (cons (symbol-value sym) faces)))
	   ((symbolp cur)
	    nil)
	   ((listp (cdr-safe cur))
	    (let ((x (cdr cur)))
	      (while x
		(if (not (memq (car x) faces))
		    (setq faces (cons (car x) faces)))
		(setq x (cdr x)))))
	   ((and (consp cur) (not (memq (cdr cur) faces)))
	    (setq faces (cons (cdr cur) faces)))
	   (t nil)))
	(while faces
	  (w3-add-zone st (point) (car faces) (cons 'w3emph (car faces)) nil)
	  (setq faces (cdr faces))))
      (cond
       ((w3-get-state 'href)
	(if (w3-get-state 'zone)
	    (w3-extend-zone (w3-get-state 'zone) (point))
	  (let* ((visitedp (w3-get-state 'seen-this-url))
		 (tag 'a)
		 (args (list (cons 'class (if visitedp "visited" "link"))))
		 (face (or (w3-get-default-style-info 'face)
			   (if visitedp
			       w3-visited-node-style
			     w3-node-style))))
	    (w3-put-state 'zone
			  (w3-add-zone
			   st (point)
			   face
			   (list 'w3
				 (w3-get-state 'name)
				 (w3-get-state 'href)
				 (w3-get-state 'txt)
				 (w3-get-state 'urn)
				 (w3-get-state 'rel)
				 (w3-get-state 'rev)
				 (w3-get-state 'meth)
				 (w3-get-state 'title)) t)))))
       ((w3-get-state 'name)
	(w3-add-zone st (point) nil
		     (cons 'w3 (list (w3-get-state 'name))))))
      (if (w3-get-state 'w3-graphic)
	  (w3-add-zone st (point) nil (list 'w3graphic
					    (w3-get-state 'w3-graphic)) t))
      ;;    (if (and (not (w3-get-state 'nofill))
      ;;	     (>= (current-column) fill-column))
      ;;	(do-auto-fill))
      (if (not (memq (char-after (1- (point))) '(?  ?.)))
	  (w3-put-state 'needspace t))
      ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paragraph breaks, and other things that can cause linebreaks and
;;; alignment changes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro w3-push-alignment ()
  (` (if align
	 (w3-put-state 'align (cons (cons tag align) (w3-get-state 'align))))))

(defmacro w3-pop-alignment ()
  (` (let ((flubber (memq (assq tag (w3-get-state 'align))
			  (w3-get-state 'align))))
       (cond
	((null flubber) nil)
	((cdr flubber)
	 (w3-put-state 'align (cdr flubber)))
	(t (w3-put-state 'align nil))))))

(defmacro w3-current-alignment ()
  (` (cdr-safe (car-safe (w3-get-state 'align)))))

;(defun w3-push-alignment ()
;  (if align
;      (w3-put-state 'align (cons (cons tag align) (w3-get-state 'align)))))

;(defun w3-pop-alignment ()
;  (let ((flubber (memq (assq tag (w3-get-state 'align))
;		       (w3-get-state 'align))))
;    (cond
;     ((null flubber) nil)
;     ((cdr flubber)
;      (w3-put-state 'align (cdr flubber)))
;     (t (w3-put-state 'align nil)))))

;(defun w3-current-alignment ()
;  (cdr-safe (car-safe (w3-get-state 'align))))

(defconst w3-fill-prefixes-vector
  (let ((len 0)
        (prefix-vector (make-vector 80 nil)))
    (while (< len 80)
      (aset prefix-vector len (make-string len ? ))
      (setq len (1+ len)))
    prefix-vector))

(defmacro w3-set-fill-prefix-length (len)
  (` (let ((len (, len)))
       (setq fill-prefix (if (< len 80)
                             (aref w3-fill-prefixes-vector len)
                           (make-string len ? ))))))

(defun w3-handle-header (&optional args)
  ;; Handle the creation of a header (of any level).  Causes a full
  ;; paragraph break. 
  (w3-handle-emphasis args)
  (let ((name (or (cdr-safe (assq 'name args))
		  (cdr-safe (assq 'id args))))
	(align (cdr-safe (assq 'align args)))
	(mangler (nth 2 (cdr-safe (assq tag w3-header-chars-assoc)))))
    (w3-handle-p)
    (if align
	(setq align (intern (downcase align)))
      (setq align (w3-get-default-style-info 'align)))
    (w3-push-alignment)
    (w3-put-state 'text-mangler mangler)
    (if name (w3-put-state 'name name))))

(defun w3-handle-header-end (&optional args)
  ;; Handle the closing of a header (of any level).  Causes a full
  ;; paragraph break.
  (w3-handle-emphasis-end)
  (let ((mangler (w3-get-state 'text-mangler)))
    (and mangler (funcall mangler nil nil t)))
  (w3-put-state 'text-mangler nil)
  (goto-char (point-max))
  (w3-handle-p)
  (let* ((info (car-safe (w3-get-state 'lists)))
	 (type (and info (car-safe info))))
    (if (and type fill-prefix)
	(insert fill-prefix (cond
			     ((memq type '(ol dl)) "    ")
			     (t "  ")))))
  (let ((tag (cdr-safe (assoc tag w3-end-tags))))
    (w3-pop-alignment)))

(defun w3-handle-pre (&optional args)
  ;; Marks the start of a preformatted section of text.  No paragraph
  ;; filling should be done from this point until a matching /pre has
  ;; been encountered.
  (w3-handle-p)
  (w3-put-state 'nofill t)
  (w3-put-state 'pre-start (set-marker (make-marker) (point)))
  )

(defun w3-handle-xmp (&optional args)
  ;; Marks the start of a preformatted section of text.  No paragraph
  ;; filling should be done from this point until a matching /pre has
  ;; been encountered.
  (w3-handle-p)
  (w3-put-state 'nofill t)
  (w3-put-state 'needspace t)
  (w3-put-state 'pre-start (set-marker (make-marker) (point)))
  (if args
      (w3-handle-text (cdr-safe (assq 'data args))))
  )

(defun w3-handle-/pre (&optional args)
  (if (not (w3-get-state 'nofill))
      (w3-handle-p)
    (w3-put-state 'nofill nil)
    (let* ((info (car-safe (w3-get-state 'lists)))
	   (type (and info (car-safe info)))
	   (st (w3-get-state 'pre-start)))
      (if (not (bolp)) (insert "\n"))
      (if (and type fill-prefix st)
	  (progn
	    (save-excursion
	      (goto-char st)
	      (while (re-search-forward "^" nil t)
		(insert fill-prefix (cond
				     ((memq type '(ol dl)) "    ")
				     (t "  ")))))
	    (setq w3-last-fill-pos (point))
	    (insert fill-prefix (cond
				 ((memq type '(ol dl)) "    ")
				 (t "  "))))
	(setq w3-last-fill-pos (point))))
    (let ((tag 'p))
      (w3-handle-p))
    (setq w3-active-faces nil)
    (w3-put-state 'pre-start nil)))

(fset 'w3-handle-/xmp 'w3-handle-/pre)

(defun w3-handle-blockquote (&optional args)
  ;; Start a section of quoted text.  This is done by causing the text
  ;; to be indented from the right and left margins.  Nested
  ;; blockquotes will cause further indentation.
  (let ((align (or (w3-get-default-style-info 'align) 'indent)))
    (w3-handle-p)
    (w3-push-alignment))
  (w3-put-state 'fillcol fill-column)
  (setq fill-column (max (- (or fill-column
				(1- (or w3-strict-width (window-width)))) 8)
			 10)))

(defun w3-handle-/blockquote (&optional args)
  (w3-handle-paragraph)
  (let ((tag (cdr-safe (assoc tag w3-end-tags))))
    (w3-pop-alignment))
  (setq fill-column (or (w3-get-state 'fillcol) (1- (or w3-strict-width
							 (window-width)))))
  (w3-put-state 'fillcol nil))

(defun w3-handle-align (&optional args)
  ;; Cause a single line break (like <BR>) and replace the current
  ;; alignment.
  (let ((align (intern (or (cdr-safe (assq 'role args))
			   (cdr-safe (assq 'align args))
			   (cdr-safe (assq 'style args))))))
    (w3-handle-paragraph)
    (w3-push-alignment)))

(defun w3-handle-/align (&optional args)
  (w3-handle-paragraph)
  (w3-pop-alignment))

(defun w3-handle-hr (&optional args)
  ;; Cause a line break and insert a horizontal rule across the page.
  (w3-handle-paragraph)
  (let* ((perc (or (cdr-safe (assq 'width args))
		   (w3-get-default-style-info 'width)
		   "100%"))
	 (old-align (w3-current-alignment))
	 (talign (intern (downcase
			  (or (cdr-safe (assq 'textalign args))
			      (and old-align (symbol-name old-align))
			      "center"))))
	 (text (cdr-safe (assq 'label args)))
	 (align (cdr-safe (assq 'align args)))
	 (rule nil)
	 (width nil))
    (setq align (if align
		    (intern (downcase align))
		  (or
		   (w3-get-default-style-info 'align)
		   old-align 'center)))
    (w3-push-alignment)

    (setq perc (min (string-to-int perc) 100)
	  width (/ (* (- (or w3-strict-width
			     (window-width))
			 w3-right-border) perc) 100))
    (if text
	(cond
	 ((>= (length text) width)
	  (setq rule (concat "-" text "-")))
	 ((eq talign 'right)
	  (setq rule (concat (make-string (- width 1 (length text))
					  w3-horizontal-rule-char)
			     text "-")))
	 ((eq talign 'center)
	  (let ((half (make-string (/ (- width (length text)) 2)
				   w3-horizontal-rule-char)))
	    (setq rule (concat half text half))))
	 ((eq talign 'left)
	  (setq rule (concat "-" text (make-string (- width 1
						      (length text))
						   w3-horizontal-rule-char)))))
      (setq rule (make-string width w3-horizontal-rule-char)))
    (w3-handle-text rule)
    (condition-case ()
	(w3-handle-paragraph)
      (error nil))
    (w3-pop-alignment)
    (setq w3-last-fill-pos (point))
    (let* ((info (car-safe (w3-get-state 'lists)))
	   (type (and info (car-safe info)))
	   (cur (w3-current-alignment)))
      (cond
       ;;((eq cur 'indent)
       ;;(insert (make-string w3-indent-level ? )))
       ((and type fill-prefix (eq w3-last-tag 'dt))
	(insert fill-prefix))
       ((and type fill-prefix)
	(insert fill-prefix (if (eq type 'ol) "    " "  ")))
       (t nil)))))

(defun w3-handle-/p (&optional args)
  ;; Marks the end of a paragraph.  Only causes a paragraph break if
  ;; it is not followed by another paragraph or similar markup
  ;; (headers, list openings, etc) that will already cause a new
  ;; paragraph to be started.
  (w3-handle-emphasis-end)
  (let ((tag (cdr-safe (assoc tag w3-end-tags))))
    (w3-handle-p)
    (w3-pop-alignment)))

(defun w3-handle-p (&optional args)
  (let ((name (or (cdr-safe (assq 'name args))
		  (cdr-safe (assq 'id args))))
	(align (cdr-safe (assoc 'align args))))
    (w3-handle-emphasis-end)
    (w3-handle-emphasis args)
    (w3-handle-paragraph)
    (w3-put-state 'nowrap (assq 'nowrap args))
    (setq align (if align
		    (intern (downcase align))
		  (w3-get-default-style-info 'align)))
    (and (eq tag 'p) (progn
		       (w3-pop-alignment)
		       (w3-push-alignment)))
    (if (not (bobp))
	(progn
	  (insert (cond
		   ((and (eolp) (bolp)) "\n")
		   ((eolp) "\n\n")
		   (t "\n")))
	  (setq w3-last-fill-pos (point))
	  (cond
	   ((null fill-prefix))
	   ((string= fill-prefix ""))
	   ((eq (car (car (w3-get-state 'lists))) 'ol)
	    (insert fill-prefix "    "))
	   (t (insert fill-prefix "  ")))))
    (if name (w3-put-state 'name name))))

(defun w3-handle-br (&optional args)
  ;; Cause a single line break.
  ;; The alignment will only effect the chunk of text (generally to
  ;; the last <br> or <p> tag) immediately before the <br>.  After
  ;; that, the alignment will revert to the containers alignment.
  (w3-handle-paragraph)
  (let* ((info (car-safe (w3-get-state 'lists)))
	 (type (and info (car-safe info)))
	 (cur (w3-current-alignment)))
    (cond
     ;;((eq cur 'indent)
     ;;(insert (make-string w3-indent-level ? )))
     ((and type fill-prefix (eq w3-last-tag 'dt))
      (insert fill-prefix))
     ((and type fill-prefix)
      (insert fill-prefix (if (eq type 'ol) "    " "  ")))
     (t nil))))

(defmacro w3-fixup-punctuation (char)
  (`
   (let ((x (char-to-string (, char))))
     (goto-char w3-last-fill-pos)
     (while (search-forward x nil t)
       (if (and (equal ?  (char-after (point)))
		(not (equal (, char) (char-after (max (- (point) 2)
						      w3-last-fill-pos)))))
	   (insert " "))))))

(defun w3-handle-paragraph (&optional args)
  (if (not (bobp))
      (let ((align (w3-current-alignment))
            (fill-prefix fill-prefix))
	      (cond
         ((eq align 'indent)
          (w3-set-fill-prefix-length
           (+ (length fill-prefix);; works even if fill-prefix is nil
              w3-indent-level)))
         ((null fill-prefix)
          (setq fill-prefix ""))
         ((string= fill-prefix ""))
	       ((eq (car (car (w3-get-state 'lists))) 'ol)
          (w3-set-fill-prefix-length (+ 4 (length fill-prefix))))
         (t
          (w3-set-fill-prefix-length (+ 2 (length fill-prefix)))))
	(if (eq align 'indent)
	    (progn
	      (goto-char w3-last-fill-pos)
	      (insert fill-prefix)
	      (goto-char (point-max))))
	(if (and (> (current-column) fill-column)
		 (not (w3-get-state 'nowrap))
		 (not (w3-get-state 'nofill)))
	    (fill-region-as-paragraph w3-last-fill-pos (point)
				      (eq align 'justify)))
;	(if (not (w3-get-state 'nofill))
;	    (progn
;	      (w3-fixup-punctuation ?.)
;	      (w3-fixup-punctuation ?!)))
	(if (not w3-last-fill-pos)
	    (setq w3-last-fill-pos (point-min)))
	(goto-char (point-max))
	(skip-chars-backward " \t\n")
	(delete-region (point) (point-max))
	(if (< w3-last-fill-pos (point))
	    (cond
	     ((or (eq align 'center) (w3-get-state 'center))
	      (center-region w3-last-fill-pos (point)))
	     ((eq align 'right)
	      (let ((x (point)))
		(catch 'fill-exit
		  (save-excursion
		    (goto-char w3-last-fill-pos)
		    (while (re-search-forward "$" x t)
		      (if (/= (current-column) fill-column)
			  (let ((buff (- fill-column (current-column))))
			    (beginning-of-line)
			    (setq x (+ x buff))
			    (if (> buff 0)
				(insert (make-string buff ? )))
			    (end-of-line))
			(end-of-line))
		      (if (eobp) (throw 'fill-exit t))
		      (condition-case ()
			  (forward-char 1)
			(error (throw 'fill-exit t))))))))))
	(insert "\n")
	(setq w3-last-fill-pos (point))
	(w3-put-state 'needspace 'never))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; List handling code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-list-ending (&optional args)
  ;; Handles all the list terminators (/ol /ul /dl).
  ;; This just fills the last paragrpah, then reduces the depth in
  ;; `w3-state' and truncates `fill-prefix'"
  (w3-handle-paragraph)
  (w3-put-state 'depth (max 0 (1- (w3-get-state 'depth))))
  (w3-put-state 'next-break t)
  (w3-set-fill-prefix-length (* (w3-get-state 'depth) w3-indent-level))
  (w3-put-state 'lists (cdr (w3-get-state 'lists)))
  (if (/= 0 (length fill-prefix))
      (insert fill-prefix "  ")))

(defun w3-handle-list-opening (&optional args)
  ;; Handles all the list openers (ol ul dl).
  ;; This just fills the last paragraph, then increases the depth in
  ;; `w3-state' and adds to `fill-prefix'
  (w3-handle-p)
  (let ((style (and (not (assq 'style args))
		    (w3-get-default-style-info 'style))))
    (if style
	(setq args (cons (cons 'style style) args))))
  ;; Default VALUE attribute for OL is 1.
  (if (eq tag 'ol)
      (or (assq 'value args)
          (setq args (cons (cons 'value 1) args))))
  (w3-put-state 'depth (1+ (w3-get-state 'depth)))
  (w3-set-fill-prefix-length (* (w3-get-state 'depth) w3-indent-level))
  (insert "\n\n" fill-prefix "  ")
  (w3-put-state 'lists (cons (cons tag (copy-alist args))
			      (w3-get-state 'lists))))

(defun w3-handle-table-definition (&optional args)
  (w3-handle-paragraph)
  (insert fill-prefix "  "))

(defun w3-handle-table-term (&optional args)
  (w3-handle-paragraph)
  (insert "\n" fill-prefix))

(defun w3-handle-list-item (&optional args)
  (w3-handle-paragraph)
  (let* ((info (car (w3-get-state 'lists)))
	 (type (car info))
	 (endr (or (nth (1- (or (w3-get-state 'depth) 1))
			(cdr (or (assoc type w3-list-chars-assoc)
				 (car w3-list-chars-assoc))))
		   "*")))
    (setq info (cdr info))
    (cond
     ((assq 'plain info)
      ;; We still need to indent from the left margin for lists without
      ;; bullets.  This is especially important with nested lists.
      ;; Question: Do we want this to be equivalent to replacing the
      ;; bullet by a space (" ") or by indenting so that the text starts
      ;; where the bullet would have been?  I've chosen the latter after
      ;; looking at both kinds of output.
      (insert fill-prefix))
     ((eq type 'ol)
      (let ((next (assq 'value info))
	    (type (cdr-safe (assq 'style info)))
	    (uppr (assq 'upper info))
	    (user-spec (cdr-safe (assq 'value args)))
	    (tokn nil))
	(if user-spec (setcdr next (string-to-int user-spec)))
	(cond
	 ((or (assq 'roman info)
	      (member type '("i" "I")))
	  (setq tokn (concat
		      (w3-pad-string (w3-decimal-to-roman (cdr next)) 3 ?
				     'left)
		      endr)))
	 ((or (assq 'arabic info)
	      (member (cdr-safe (assq 'style info)) '("a" "A")))
	  (setq tokn (concat (w3-pad-string
			      (w3-decimal-to-alpha (cdr next)) 3 ?  'left)
			     endr)))
	 (t
	  (setq tokn (concat (w3-pad-string (int-to-string (cdr next))
					    2 ?  'left)
			     endr))))
	(insert fill-prefix tokn " ")
	(setcdr next (1+ (cdr next)))
	(w3-put-state 'needspace 'never)))
     (t
      (insert fill-prefix endr " ")))))

(defun w3-pad-string (str len pad side)
  ;; Pads a string STR to a certain length LEN, using fill character
  ;; PAD by concatenating PAD to SIDE of the string.
  (let ((strlen (length str)))
    (cond
     ((>= strlen len) str)
     ((eq side 'right) (concat str (make-string (- len strlen) pad)))
     ((eq side 'left)  (concat (make-string (- len strlen) pad) str)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Routines to handle character-level formatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-q (&optional args)
  (w3-handle-emphasis)
  (w3-handle-text (or (w3-get-default-style-info 'startquote) "\"")))

(defun w3-handle-/q (&optional args)
  (let ((tag (cdr-safe (assoc tag w3-end-tags))))
    (w3-handle-text (or (w3-get-default-style-info 'endquote) "\"")))
  (w3-handle-emphasis-end))

(defun w3-handle-emphasis (&optional args)
  ;; Generic handler for character-based emphasis.  Increments the state
  ;; of TAG (which must be bound by the calling procedure).  This
  ;; checks all the various stylesheet mechanisms that may cause an
  ;; alignment shift as well.
  (let ((align (or (w3-get-default-style-info 'align)
		   (and (eq tag 'address) w3-right-justify-address 'right))))
    (if (and align (not (memq tag '(h1 h2 h3 h4 h5 h6))))
	(progn
	  (w3-handle-paragraph)
	  (w3-push-alignment))))
  (let* ((spec (and w3-delimit-emphasis (assoc tag w3-style-tags-assoc)))
	 (class (cdr-safe (assq 'class args)))
	 (face (w3-face-for-element))
	 (beg (and spec (car (cdr spec)))))
    (if spec
	(insert beg))
    (if face
	(setq w3-active-faces (cons face w3-active-faces)))))

(defun w3-handle-emphasis-end (&optional args)
  ;; Generic handler for ending character-based emphasis.  Decrements
  ;; the state of TAG (which must be bound by the calling procedure).
  ;; Stylesheet mechanisms may cause arbitrary alignment changes.
  (let* ((tag (cdr-safe (assq tag w3-end-tags)))
	 (spec (and w3-delimit-emphasis (assq tag w3-style-tags-assoc)))
	 (end (and spec (cdr (cdr spec)))))
    (if (assq tag w3-active-faces)
	(setq w3-active-faces (cdr (memq (assq tag w3-active-faces)
					 w3-active-faces)))
      (setq w3-active-faces (delq tag w3-active-faces)))
    (if spec (insert end))
    (if (eq tag 'address)
	(progn
	  (w3-handle-paragraph)
	  (w3-pop-alignment)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML 3.0 compliance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-div (&optional args)
  (let ((align (cdr-safe (assq 'align args))))
    (w3-handle-emphasis args)
    (w3-handle-paragraph)
    (setq align (and align (intern (downcase align))))
    (w3-push-alignment)))

(defun w3-handle-/div (&optional args)
  (w3-handle-emphasis-end)
  (let ((tag (cdr-safe (assq tag w3-end-tags))))
    (w3-handle-paragraph)
    (w3-pop-alignment)))

(defun w3-handle-note (&optional args)
  (w3-handle-emphasis)
  (w3-handle-paragraph)
  (let ((align (or (w3-get-default-style-info 'align) 'indent)))
    (w3-push-alignment))
  (w3-handle-text (concat (or (cdr-safe (assq 'role args)) "CAUTION") ":")))

(defun w3-handle-/note (&optional args)
  (w3-handle-paragraph)
  (w3-handle-emphasis-end)
  (let ((tag (cdr-safe (assoc tag w3-end-tags))))
    (w3-pop-alignment)))

(defun w3-handle-fig (&optional args)
  (w3-put-state 'figdata args)
  (w3-put-state 'figalt (set-marker (make-marker) (point)))
  )

(defun w3-handle-caption (&optional args)
  )

(defun w3-handle-/caption (&optional args)
  )

(defun w3-handle-/fig (&optional args)
  (let* ((data (w3-get-state 'figdata))
	 (src (cdr-safe (assq 'src data)))
	 (aln (cdr-safe (assq 'align data)))
	 (alt (if (w3-get-state 'figalt)
		  (prog1
		      (buffer-substring (w3-get-state 'figalt) (point))
		    (delete-region (w3-get-state 'figalt) (point)))))
	 (ack nil))
    (setq w3-last-fill-pos (point))
    (if (not src)
	(w3-warn 'html "Malformed <fig> tag.")
      (setq ack (list (cons 'src src)
		      (cons 'alt alt)
		      (cons 'align aln)))
      (w3-handle-pre nil)
      (w3-handle-image ack)
      (w3-handle-/pre nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Netscape Compatibility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For some reason netscape treats </br> like <br> - ugh.
(fset 'w3-handle-/br 'w3-handle-br)

(defun w3-handle-font (&optional args)
  (let* ((sizearg (or (cdr-safe (assq 'size args)) "5"))
	 (sizenum (cond
		   ((null sizearg) nil)
		   ((= ?+ (string-to-char sizearg))
		    (+ 3 (string-to-int (substring sizearg 1))))
		   ((= ?- (string-to-char sizearg))
		    (- 3 (string-to-int (substring sizearg 1))))
		   ((string= sizearg (int-to-string (string-to-int sizearg)))
		    (string-to-int sizearg))
		   (t 4)))
	 (tag (if (integerp sizenum)
		  (intern (concat "font" (int-to-string sizenum)))
		'font4)))
    (w3-handle-emphasis args)))

(defun w3-handle-/font (&optional args)
  (let ((tags '(font0 font1 font2 font3 font4 font5 font6 font7)))
    (while tags
      (setq w3-active-faces (delq (car tags) w3-active-faces)
	    tags (cdr tags)))))

(defun w3-handle-center (&optional args)
  (w3-handle-paragraph)
  (let ((align 'center))
    (w3-push-alignment)))

(defun w3-handle-/center (&optional args)
  (w3-handle-paragraph)
  (let ((tag 'center))
    (w3-pop-alignment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bonus HTML Tags just for fun :)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-embed (&optional args)
  (let* ((buf (url-generate-new-buffer-name " *embed*"))
	 (w3-draw-buffer (current-buffer))
	 (url-working-buffer buf)
	 (data (cdr-safe (assq 'data args)))
	 (href (and (not data)
		    (url-expand-file-name
		     (or (cdr-safe (assq 'src args))
			 (cdr-safe (assq 'href args)))
		     (cdr-safe (assoc (cdr-safe (assq 'base args))
				      w3-base-alist)))))
	 (type (or (cdr-safe (assq 'type args)) "text/plain"))
	 (parse nil))
    (if (and href (not (string= type "video/mpeg")))
	;; MPEG movies can be _HUGE_, delay loading them as
	;; long as possible
	(save-excursion
	  (set-buffer (get-buffer-create buf))
	  (setq url-be-asynchronous nil)
	  (url-retrieve href)
	  (setq data (buffer-string))
	  (kill-buffer (current-buffer))))
    (cond
     ((string= type "text/plain")
      (insert data))
     ((string-match "^text/html" type)
      (save-excursion
	(set-buffer (get-buffer-create
		     (url-generate-new-buffer-name " *embed*")))
	(erase-buffer)
	(insert data)
	(setq parse (w3-preparse-buffer (current-buffer) t))
	(kill-buffer (current-buffer)))
      (while parse
	(w3-handle-single-tag (car (car parse)) (cdr (car parse)))
	(setq parse (cdr parse))))
     ((string= type "video/mpeg")
      (let ((width (cdr-safe (assq 'width args)))
	    (height (cdr-safe (assq 'height args))))
	(setq width (if width (string-to-int width))
	      height (if height (string-to-int height)))
	(w3-add-delayed-mpeg href (point) width height))))))

(defun w3-handle-blink (&optional args)
  ;; Keep track of all the buffers with blinking in them, and do GC
  ;; of this list whenever a new <blink> tag is encountered.  The
  ;; timer checks this list to see if any of the buffers are visible,
  ;; and only blinks the face if there are any visible.  This cuts
  ;; down tremendously on the amount of X traffic, and frame !@#!age
  ;; due to lots of face munging.
  (w3-handle-emphasis args)
  (let ((buffs w3-blinking-buffs)
	(name1 (buffer-name))
	(name2 nil)
	(add t))
    (setq w3-blinking-buffs nil)
    ;; Get rid of old buffers
    (while buffs
      (setq name2 (buffer-name (car buffs)))
      (if (null name2)
	  nil
	(setq w3-blinking-buffs (cons (car buffs) w3-blinking-buffs))
	(if (string= name1 name2)
	    (setq add nil)))
      (setq buffs (cdr buffs)))
    (if add
	(setq w3-blinking-buffs (cons (current-buffer) w3-blinking-buffs)))))

(defun w3-handle-/blink (&optional args)
  (w3-handle-emphasis-end args))

(defun w3-handle-peek (&optional args)
  ;; Handle the peek tag.  Valid attributes are:
  ;; VARIABLE:: any valid lisp variable
  ;; If VARIABLE is bound and non-nil, then the value of the variable is
  ;; inserted at point.  This can handle variables whos values are any
  ;; arbitrary lisp type.
  (let* ((var-name (cdr-safe (assq 'variable args)))
	 (var-sym  (and var-name (intern var-name)))
	 (val      (and var-sym (boundp var-sym) (symbol-value var-sym))))
    (cond
     ((null val) nil)
     ((stringp val) (w3-handle-text val))
     (t (w3-handle-text (format "%S" val))))))

(defun w3-rotate-region (st nd &optional rotation)
  "Ceasar rotate a region between ST and ND using ROTATION as the
amount to rotate the text.  Defaults to caesar (13)."
  (setq rotation (or rotation 13))
  (save-excursion
    (let (x)
      (while (< st nd)
	(setq x (char-after st))
	(cond
	 ((and (>= x ?a) (<= x ?z))
	  (setq x (- x ?a)
		x (char-to-string (+ (% (+ x rotation) 26) ?a))))
	 ((and (>= x ?A) (<= x ?Z))
	  (setq x (- x ?A)
		x (char-to-string (+ (% (+ x rotation) 26) ?A))))
	 (t (setq x nil)))
	(if x (progn (goto-char st) (delete-char 1) (insert x)))
	(setq st (1+ st))))))

(defun w3-handle-kill-sgml (&optional args)
  (w3-handle-text "SGML is the spawn of evil!  It must be stopped!"))

(defun w3-handle-secret (&optional args)
  (if (fboundp 'valid-specifier-locale-p)
      (let ((tag 'rot13))
	(w3-handle-emphasis))
    (w3-put-state 'secret (set-marker (make-marker) (point)))))

(defun w3-handle-/secret (&optional args)
  "Close a secret region of text."
  (if (fboundp 'valid-specifier-locale-p)
      (let ((tag '/rot13))
	(w3-handle-emphasis-end))
    (if (integer-or-marker-p (w3-get-state 'secret))
	(progn
	  (w3-rotate-region (w3-get-state 'secret) (point))
	  (w3-put-state 'secret nil)))))

(defun w3-handle-hype (&optional args)
  (if (and (or (featurep 'nas-sound) (featurep 'native-sound))
	   (assoc 'hype sound-alist))
      (play-sound 'hype 100)
    (w3-handle-text "Hey, has Marca A. told you how cool he is?")))

(defun w3-handle-yogsothoth (&optional args)
  (w3-handle-image (list (cons "src" "href-to-yogsothoth-pic")
			 (cons "alt" "YOGSOTHOTH LIVES!!!"))))

(defun w3-handle-roach (&optional args)
  (w3-handle-text "Man, I am so wasted..."))

(defun w3-handle-/roach (&optional args)
  (w3-handle-text (concat "So, you wanna get some "
			  (or (cdr-safe (assq 'munchy args))
			      "nachos") "? ")))

(defun w3-invert-face (&optional face)
  (setq face (or face w3-blink-style))
  (let ((buffs w3-blinking-buffs)
	(blink nil)
	(buff nil))
    (if buffs
	(while buffs
	  (setq buff (car buffs))
	  (cond
	   ((bufferp buff)
	    (if (buffer-name buff)
		(setq buff (car buffs))
	      (setq buff nil)))
	   ((stringp buff)
	    (setq buff (get-buffer buff)))
	   (t
	    (setq buff nil)))
	  (setq buffs (cdr buffs)
		buff (and buff (get-buffer-window buff 'visible))
		buff (and buff (window-live-p buff)))
	  (if buff (setq buffs nil
			 blink t))))
    (if blink (invert-face face))))

(autoload 'sentence-ify "flame")
(autoload 'string-ify "flame")
(autoload '*flame "flame")
(if (not (fboundp 'flatten)) (autoload 'flatten "flame"))

(defvar w3-cookie-cache nil)

(defun w3-handle-cookie (&optional args)
  (if (not (fboundp 'cookie))
      (w3-handle-text "Sorry, no cookies today.")
    (let* ((url-working-buffer (url-generate-new-buffer-name " *cookie*"))
	   (href (url-expand-file-name
		  (or (cdr-safe (assq 'src args))
		      (cdr-safe (assq 'href args)))
		  (cdr-safe (assoc (cdr-safe (assq 'base args))
				   w3-base-alist))))
	   (fname (or (cdr-safe (assoc href w3-cookie-cache))
		      (url-generate-unique-filename "%s.cki")))
	   (st (or (cdr-safe (assq 'start args)) "Loading cookies..."))
	   (nd (or (cdr-safe (assq 'end args))
		   "Loading cookies... done.")))
      (if (not (assoc href w3-cookie-cache))
	  (save-excursion
	    (url-clear-tmp-buffer)
	    (setq url-be-asynchronous nil)
	    (url-retrieve href)
	    (url-uncompress)
	    (write-region (point-min) (point-max) fname 5)
	    (setq w3-cookie-cache (cons (cons href fname) w3-cookie-cache))))
      (w3-handle-text (cookie fname st nd)))))

(defun w3-handle-flame (&optional args)
  (condition-case ()
      (w3-handle-text
       (concat
	(sentence-ify
	 (string-ify
	  (append-suffixes-hack (flatten (*flame)))))))
    (error nil)))

(defun w3-handle-pinhead (&optional args)
  (if (fboundp 'yow)
      (w3-handle-text (yow))))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tags that don't really get drawn, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun w3-handle-body (&optional args)
  (if (not w3-user-colors-take-precedence)
      (let* ((vlink (cdr-safe (assq 'vlink args)))
	     (alink (cdr-safe (assq 'alink args)))
	     (link (cdr-safe (assq 'link args)))
	     (text (cdr-safe (assq 'text args)))
	     (backg (cdr-safe (assq 'background args)))
	     (rgb (or (cdr-safe (assq 'bgcolor args))
		      (cdr-safe (assq 'rgb args))))
	     (temp-face nil))
	(setq backg (url-expand-file-name
		     backg
		     (cdr-safe (assoc (cdr-safe (assq 'base args))
				      w3-base-alist))))
	(if rgb (setq rgb (font-normalize-color rgb)))
	(if link (setq link (font-normalize-color link)))
	(if text (setq text (font-normalize-color text)))
	(if vlink (setq vlink (font-normalize-color vlink)))
	(if alink (setq alink (font-normalize-color alink)))
	(cond
	 (w3-running-epoch
	  (if link
	      (progn
		(setq temp-face (intern (concat "w3-temp-face-" link)))
		(make-face temp-face)
		(w3-munge-color-fore temp-face link)
		(setq w3-node-style temp-face)))
	  (if vlink
	      (progn
		(setq temp-face (intern (concat "w3-temp-face-" vlink)))
		(make-face temp-face)
		(w3-munge-color-fore temp-face vlink)
		(setq w3-visited-node-style temp-face)))
	  (if (or text rgb)
	      (let ((face nil)
		    (ctr 0))
		(while (find-face (intern (format "w3-temp-face-%d" ctr)))
		  (setq ctr (1+ ctr)))
		(setq face (intern (format "w3-temp-face-%d" ctr)))
		(make-face face)
		(and text (w3-munge-color-fore face text))
		(and rgb (w3-munge-color-back face rgb))
		(setq buffer-style (face-instance face)))))
	 ;; Only XEmacs will be able to do the locale stuff for faces
	 ((fboundp 'valid-specifier-locale-p)
	  (w3-put-state 'background backg)
	  (and text (w3-munge-color-fore 'default text (current-buffer)))
	  (and rgb (w3-munge-color-back 'default rgb (current-buffer)))
	  (and link (w3-munge-color-fore w3-node-style link (current-buffer)))
	  (and vlink (w3-munge-color-fore w3-visited-node-style vlink
					  (current-buffer)))
	  (and alink (w3-munge-color-fore w3-active-node-style alink
					  (current-buffer))))
	 ((fboundp 'make-face)
	  (if alink
	      (progn
		(setq temp-face (intern (concat "w3-temp-face-" alink)))
		(make-face temp-face)
		(w3-munge-color-fore temp-face alink)
		(setq w3-active-node-style temp-face)))
	  (if link
	      (progn
		(setq temp-face (intern (concat "w3-temp-face-" link)))
		(make-face temp-face)
		(w3-munge-color-fore temp-face link)
		(setq w3-node-style temp-face)))
	  (if vlink
	      (progn
		(setq temp-face (intern (concat "w3-temp-face-" vlink)))
		(make-face temp-face)
		(w3-munge-color-back temp-face vlink)
		(setq w3-visited-node-style temp-face))))))))

(defun w3-handle-cryptopts (&optional args)
  (put 'text 'w3-formatter 'ack))

(defun w3-handle-/cryptopts (&optional args)
  (put 'text 'w3-formatter nil))

(defun w3-handle-certs (&optional args)
  (put 'text 'w3-formatter 'ack))

(defun w3-handle-/certs (&optional args)
  (put 'text 'w3-formatter nil))

(defun w3-handle-base (&optional args)
  (setq w3-base-alist (cons
		       (cons (or (cdr-safe (assq 'name args))
				 (cdr-safe (assq 'id args)))
			     (or (cdr-safe (assq 'href args))
				 (cdr-safe (assq 'src args))
				 (url-view-url t)))
		       w3-base-alist)))

(defun w3-handle-isindex (&optional args)
  (let ((prompt (or (cdr-safe (assq 'prompt args))
		    "Search on (+ separates keywords): "))
	action)
    (setq action (url-expand-file-name
		  (or (cdr-safe (assq 'src args))
		      (cdr-safe (assq 'href args))
		      (url-view-url t))
		  (cdr-safe (assoc (cdr-safe (assq 'base args))
				   w3-base-alist))))
    (if (and prompt (string-match "[^: \t-]+$" prompt))
	(setq prompt (concat prompt ": ")))
    (if w3-use-forms-index
	(progn
	  (w3-handle-hr)
	  (w3-handle-form (list (cons "action" action)
				(cons "method" "get")))
	  (w3-handle-text (concat prompt " "))
	  (w3-handle-input (list (cons "type" "text")
				 (cons "name" "isindex")))))
    (setq w3-current-isindex (cons action prompt))))

(defun w3-handle-meta (&optional args)
  (let* ((equiv (cdr-safe (assq 'http-equiv args)))
	 (value (cdr-safe (assq 'content args)))
	 (node  (and equiv (assoc (setq equiv (downcase equiv))
				  url-current-mime-headers))))
    (cond
     ((and equiv node) (setcdr node value))
     (equiv (setq url-current-mime-headers (cons (cons equiv value)
						 url-current-mime-headers)))
     (t nil))
    ;; Special-case the refresh header
    (if (and equiv (string= (downcase equiv) "refresh"))
	(url-handle-refresh-header value))))

(defun w3-handle-link (&optional args)
  (let* ((dest (cdr-safe (assq 'href args)))
	 (type (if (assq 'rel args) "Parent of" "Child of"))
	 (desc (or (cdr-safe (assq 'rel args))
		   (cdr-safe (assq 'rev args))))
	 (node-1 (assoc type w3-current-links))
	 (node-2 (and node-1 desc (assoc desc (cdr node-1))))
	 (base (cdr-safe (assq 'base args))))
    (if dest
	(progn
	  (setq dest (url-expand-file-name
		      dest
		      (cdr-safe (assoc base w3-base-alist))))
	  (cond
	   (node-2 (setcdr node-2 dest)) ; Override old setting
	   (node-1 (setcdr node-1 (cons (cons desc dest) (cdr node-1))))
	   (t (setq w3-current-links
		    (cons (cons type (list (cons desc dest)))
			  w3-current-links))))
	  (if (and dest desc (member (downcase desc)
					 '("style" "stylesheet")))
	      (w3-handle-style (list (cons 'src dest))))))))

;;; slightly modified by the MULE contributors
(defun w3-handle-image (&optional args)
  (let* ((parms args)
	 (height (cdr-safe (assq 'height parms)))
	 (width (cdr-safe (assq 'width parms)))
	 (src (or (cdr-safe (assq 'src parms))
		  "Error Image"))
	 (our-alt (cond
		   ((null w3-auto-image-alt) "")
		   ((eq t w3-auto-image-alt)
		    (concat "[IMAGE(" (url-basepath src t) ")] "))
		   ((stringp w3-auto-image-alt)
		    (format w3-auto-image-alt (url-basepath src t)))))
	 (alt (or (cdr-safe (assq 'alt parms))
		  our-alt))
	 (ismap (and (assq 'ismap args) 'ismap))
	 (dest (w3-get-state 'href))
	 (base (cdr-safe (assq 'base args)))
	 (align (intern (or (cdr-safe (assq 'align parms)) "middle"))))
    (setq src (url-expand-file-name src
				    (cdr-safe (assoc base w3-base-alist))))
    (if (fboundp 'w3-insert-graphic)
	(w3-add-delayed-graphic (cons src (cons dest ismap))
				(set-marker (make-marker) (point))
				align alt)
      (w3-put-state 'w3-graphic src)
      (w3-handle-text alt)
      (w3-put-state 'w3-graphic nil)
      (and w3-auto-image-alt dest
	   (progn
	     (w3-handle-text " ")
	     (setq w3-invisible-href-list
		   (append w3-invisible-href-list
			   (list
			    (list
			     (count-lines (point-min) (point))
			     dest
			     (url-basepath dest t))))))))))

(defun w3-handle-title (&optional args)
  (if (w3-get-state 'title)
      (w3-put-state 'title nil))
  (put 'text 'w3-formatter 'w3-handle-title-text))

(defun w3-handle-title-text (&optional args)
  (w3-put-state 'title
       (concat (w3-get-state 'title) args)))

(defun w3-handle-/title (&optional args)
  (put 'text 'w3-formatter nil)
  (let ((ttl (w3-get-state 'title)))
    (cond
     ((and (symbolp ttl) (eq ttl t))
      nil)
     ((stringp ttl)
      (setq ttl (w3-fix-spaces ttl))
      (if (and ttl (string= ttl ""))
	  (setq ttl (w3-fix-spaces (url-view-url t))))
      (rename-buffer (url-generate-new-buffer-name ttl))
      (w3-put-state 'title t))
     (t nil))))

(fset 'w3-handle-/head 'w3-handle-/title)

(defun w3-handle-hyperlink (&optional args)
  (let ((href (cdr-safe (assq 'href args)))
	(base (cdr-safe (assq 'base args)))
	(name (or (cdr-safe (assq 'id args))
		  (cdr-safe (assq 'name args)))))
    (if href
	(setq href (url-expand-file-name href
					 (cdr-safe
					  (assoc base w3-base-alist)))))
    (if (and w3-delimit-links (not (eq w3-delimit-links 'linkname)) href)
	(progn
	  (w3-put-state 'seen-this-url (url-have-visited-url href))
	  (if (w3-get-state 'seen-this-url)
	      (w3-handle-text (cdr w3-link-start-delimiter))
	    (w3-handle-text (car w3-link-start-delimiter)))
	  (w3-put-state 'needspace 'never)))
    (w3-put-state 'zone nil)
    (if href (w3-put-state 'href href))
    (if name (w3-put-state 'name name))))

(defun w3-handle-hyperlink-end (&optional args)
  (let* ((href (w3-get-state 'href))
	 (name (w3-get-state 'name))
	 (btdt (and href (w3-get-state 'seen-this-url))))
    (w3-put-state 'zone nil)
    (w3-put-state 'href nil)
    (w3-put-state 'name nil)

    (if (and w3-delimit-links href)
	(progn
	  (delete-region (point) (progn (skip-chars-backward " ")
					(point)))
	  (if (eq w3-delimit-links 'linkname)
	      (w3-handle-text (concat (if btdt (cdr w3-link-start-delimiter)
					(car w3-link-start-delimiter))
				      (or name "noname")
				      (if btdt (cdr w3-link-end-delimiter)
					(car w3-link-end-delimiter))))
	    (if btdt
		(w3-handle-text (cdr w3-link-end-delimiter))
	      (w3-handle-text (car w3-link-end-delimiter)))))
	  (goto-char (point-max)))
    (if (and w3-link-delimiter-info (fboundp w3-link-delimiter-info))
	(let ((info (condition-case ()
			(funcall w3-link-delimiter-info href)
		      (error nil))))
	  (if (and info (stringp info))
	      (w3-handle-text (concat (if btdt (cdr w3-link-start-delimiter)
					(car w3-link-start-delimiter))
				      info
				      (if btdt (cdr w3-link-end-delimiter)
					(car w3-link-end-delimiter)))))))))

(defvar w3-tab-alist nil
  "An assoc list of tab stops and their respective IDs")
(make-variable-buffer-local 'w3-tab-alist)

(defun w3-handle-tab (&optional args)
  (let* ((id (cdr-safe (assq 'id args)))
	 (to (cdr-safe (assq 'to args)))
	 (pos (cdr-safe (assoc to w3-tab-alist))))
    (cond
     (id				; Define a new tab stop
      (setq w3-tab-alist (cons (cons id (current-column)) w3-tab-alist)))
     ((and to pos)			; Go to a currently defined tabstop
      (while (<= (current-column) pos)
	(insert " ")))
     (to				; Tabstop 'to' is no defined yet
      (w3-warn 'html (format "Unkown tab stop -- `%s'" to)))
     (t					; Just do a tab
      (insert (make-string w3-indent-level ? ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Some bogus shit for pythia
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-margin (&optional args)
  (if (assq 'reset args)
      (w3-handle-/blockquote nil)
    (w3-handle-blockquote nil)))
  
(fset 'w3-handle-l 'w3-handle-br)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Guts of the forms interface for the new display engine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-handle-form (&optional args)
  (let ((actn (cdr-safe (assq 'action args)))
	(enct (cdr-safe (assq 'enctype args)))
	(meth (cdr-safe (assq 'method args))))
    (if (not meth) (setq args (cons (cons 'method "GET") args)))
    (if (not actn)
	(setq args (cons (cons 'action
			       (or
				(cdr-safe (assoc (cdr-safe (assq 'base args))
						 w3-base-alist))
				(url-view-url t))) args))
      (setcdr (assq 'action args)
	      (url-expand-file-name
	       actn
	       (cdr-safe (assoc (cdr-safe (assq 'base args))
				w3-base-alist)))))
    (if (not enct)
	(setq args
	      (cons (cons 'enctype "application/x-www-form-urlencoded")
		    args)))
    (w3-put-state 'form args)))

(defun w3-handle-/form (&optional args)
  (w3-handle-paragraph)
  (w3-put-state 'form nil)
  (w3-put-state 'formnum (1+ (w3-get-state 'formnum)))
  )

(defun w3-handle-input (&optional args)
  (if (or (not (w3-get-state 'form))
	  (w3-get-state 'select))
      (w3-warn
       'html
       "<input> outside of a <form> or inside <select> construct - ERROR!!")
    (let* ((type (upcase (or (cdr-safe (assq 'type args)) "text")))
	   (name (cdr-safe (assq 'name args)))
	   (value (or (cdr-safe (assq 'value args)) ""))
	   (size (string-to-int (or (cdr-safe (assq 'size args)) "20")))
	   (maxlength (cdr (assoc 'maxlength args)))
	   (default value)
	   (action (w3-get-state 'form))
	   (options)
	   (num (w3-get-state 'formnum))
	   (id (cdr-safe (assq 'id args)))
	   (checked (assq 'checked args)))
      (if (and (string-match "^[ \t\n\r]+$" value)
	       (not (string= type "HIDDEN")))
	  (setq value ""))
      (if maxlength (setq maxlength (string-to-int maxlength)))
      (if (and name (string-match "[\r\n]" name))
	  (setq name (mapconcat (function
				 (lambda (x) (if (memq x '(?\r ?\n)) "" (char-to-string x))))
				name "")))
      (if (member type '("CHECKBOX" "RADIO")) (setq default checked))
      (if (and (string= type "CHECKBOX") (string= value ""))
	  (setq value "on"))
      (if (string= type "HIDDEN")
	  (setq w3-hidden-forms (cons (list 'w3form action type name default
					    value checked size maxlength num
					    options id) w3-hidden-forms))
	(let ((formatfun nil)
	      (prompt nil) pos)
	  (setq formatfun (intern (concat "w3-form-format-"
					  (downcase type))))
	  (if (not (fboundp formatfun))
	      (setq formatfun 'w3-form-format-unknown))
	  (if (string= type "IMAGE")
	      (setq checked (cons (or (cdr-safe (assq 'alt args))
				      "imageinput") name)))
	  (if (and w3-delimit-links (member type '("RESET" "SUBMIT")))
	      (w3-handle-text (car w3-link-start-delimiter)))
	  (cond
	   ((and (not value) (string= type "RESET"))
	    (setq value "Reset form"))
	   ((and (not value) (string= type "SUBMIT"))
	    (setq value "Submit form"))
	   ((string= type "RANGE")
	    (let* ((arg (or (cdr-safe (assq 'size args)) "1,10"))
		   (min (string-to-int arg))
		   (max nil))
	      (setq max 
		    (if (string-match ",\\(.*\\)" arg)
			(string-to-int (url-match arg 1))
		      (+ min 10))
		    maxlength (cons min max)
		    size (string-width (int-to-string (max min max))))))
	   (t nil))
	  (setq prompt (funcall formatfun value size checked)
		pos (point))
	  (if w3-running-FSF19 (insert prompt) (w3-insert prompt))
	  (w3-add-zone pos (point) w3-node-style
		       (list 'w3form
			     action type name default value
			     checked size maxlength num options id) t)
	  (if (and w3-delimit-links (member type '("RESET" "SUBMIT")))
	      (w3-handle-text (car w3-link-end-delimiter)))
	  (w3-put-state 'needspace t))))))

(defun w3-handle-/select (&optional args)
  (if (not (and (w3-get-state 'form)
		(w3-get-state 'select)))
      (w3-warn 'html
	       "</select> outside of a <form> or <select> construct - ERROR!!")
    (put 'text 'w3-formatter 'w3-handle-text)
    (let* ((args (w3-get-state 'select))
	   (opts (w3-get-state 'options))
	   (form (w3-get-state 'form))
	   (max-size nil)
	   (type "OPTION")
	   (default nil)
	   (tmp nil)
	   (id (cdr-safe (assq 'id args)))
	   (checked nil)
	   )
      (setq tmp (reverse opts))
      (if (assq 'multiple args)
	  (let ((tag 'ul)		; Convert to a list of checkboxes
		(nam (or (cdr-safe (assq 'name args)) "option"))
		(old (w3-get-state 'align))
		(first nil))
	    (w3-put-state 'options nil)
	    (w3-put-state 'select nil)
	    (w3-handle-list-opening)
	    (w3-put-state 'align nil)
	    (while tmp
	      (w3-handle-list-item)
	      (w3-handle-input (list (cons 'type "checkbox")
				     (cons 'name nam)
				     (cons 'value
					   (or (cdr-safe
						(assq 'value (car tmp)))
					       (cdr-safe
						(assoc 'ack (car tmp)))
					       "unknown"))
				     (if (or (assq 'checked (car tmp))
					     (assq 'selected (car tmp)))
					 (cons 'checked "checked"))))
	      (w3-handle-text (concat " " (or
					   (cdr-safe (assq 'ack (car tmp)))
					   "unknown")))
	      (setq tmp (cdr tmp)))
	    (w3-handle-list-ending)
	    (w3-put-state 'align old))
	(while (and (not default) tmp)
	  (if (or (assq 'checked (car tmp))
		  (assq 'selected (car tmp)))
	      (setq default (car tmp)))
	  (setq tmp (cdr tmp)))
	(setq default (cdr (assq 'ack (or default
					    (nth (1- (length opts)) opts))))
	      checked (mapcar
		       (function
			(lambda (x)
			  (cons (cdr-safe (assq 'ack x))
				(or (cdr-safe (assq 'value x))
				    (cdr-safe (assq 'ack x))))))
		       opts)
	      max-size (car (sort (mapcar
				   (function
				    (lambda (x)
				      (length (cdr-safe (assq 'ack x)))))
				   opts)
				  '>)))
	(if (and form args opts)
	    (let ((pos (point))
		  (siz (max max-size
			    (string-to-int
			     (or (cdr-safe (assq 'size args)) "0")))))
	      (insert (w3-form-format-text default siz))
	      (w3-add-zone pos (point) w3-node-style
			   (list 'w3form form type
				 (or (cdr-safe (assq 'name args)) "option")
				 default default
				 checked
				 siz
				 (string-to-int
				  (or (cdr-safe (assq 'maxlength args))
				      "1000"))
				 (w3-get-state 'formnum)
				 (mapcar
				  (function
				   (lambda (x)
				     (cons (cdr-safe (assq 'ack x))
					   (cdr-safe (assq 'ack x)))))
				  opts) id) t)))))
    (w3-put-state 'options nil)
    (w3-put-state 'select nil)))

(defun w3-handle-option-data (&optional args)
  (let ((text (cond
	       ((null args) nil)
	       ((stringp args) args)
	       ((listp args) (mapconcat 'identity args " ")))))
    (if text
	(progn
	  (setq text (url-strip-leading-spaces
		      (url-eat-trailing-space text)))
	  (w3-put-state 'options (cons (cons (cons 'ack text)
					      (w3-get-state 'optargs))
					(w3-get-state 'options))))))
  (put 'text 'w3-formatter 'w3-handle-text))
			   
(defun w3-handle-option (&optional args)
  (if (not (and (w3-get-state 'form)
		(w3-get-state 'select)))
      (w3-warn 'html
	       "<option> outside of a <form> or <select> construct - ERROR!!")
    (w3-put-state 'optargs args)
    (put 'text 'w3-formatter 'w3-handle-option-data)))
			     
(defun w3-handle-select (&optional args)
  (if (not (w3-get-state 'form))
      (w3-warn 'html "<select> outside of a <FORM> construct - ERROR!!")
    (w3-put-state 'select args))
  )

(defun w3-handle-textarea (&optional args)
  (if (not (w3-get-state 'form))
      (w3-warn 'html "<textarea> outside of a <FORM> construct - ERROR!!")
    (let ((node (assq 'maxlength args)))
      (cond
       ((null node)
	(setq args (cons (cons 'maxlength nil) args)))
       ((null (cdr-safe node))
	nil)
       ((string= (downcase (cdr-safe node)) "unlimited")
	(setcdr node nil))))
    (let* (
	   (value (cdr-safe (assq 'data args)))
	   (type "TEXTAREA")
	   (name (cdr-safe (assq 'name args)))
	   (size (string-to-int (or (cdr-safe (assq 'size args)) "20")))
	   (maxlength (string-to-int
		       (or (cdr (assq 'maxlength args)) "10000")))
	   (default nil)
	   (action (w3-get-state 'form))
	   (options)
	   (pos)
	   (num (w3-get-state 'formnum))
	   (id (cdr-safe (assq 'id args)))
	   (checked (assq 'checked args)))
      (setq default value
	    pos (point))
      (put 'text 'w3-formatter 'w3-handle-text)
      (w3-handle-text "Multiline text area")
      (w3-add-zone pos (point) w3-node-style
		   (list 'w3form
			 action type name default value
			 checked size maxlength num options id) t))))

(defun w3-handle-label-text (&optional args)
  (setcdr (w3-get-state 'label-text)
	  (concat (cdr (w3-get-state 'label-text)) args))
  (w3-handle-text args))

(defun w3-handle-/label (&optional args)
  (let ((num (w3-get-state 'formnum))
	(dat (w3-get-state 'label-text)))
    (setq w3-form-labels (cons (cons (format "%d:%s" num (car dat))
				     (cdr dat))
			       w3-form-labels))
    (put 'text 'w3-formatter 'w3-handle-text)))

(defun w3-handle-label (&optional args)
  (if (not (w3-get-state 'form))
      (w3-warn 'html "<label> outside of a <FORM> construct - ERROR!!")
    (put 'text 'w3-formatter 'w3-handle-label-text)
    (w3-put-state 'label-text (cons (or (cdr-safe (assq 'for args))
					"Unknown label") ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For w3-beta
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-show-buffer ()
  (let ((potential-title
	 (and (not (w3-get-state 'title))
	      (url-generate-new-buffer-name
	       (url-basepath url-current-file t)))))
    (if (and potential-title (string= potential-title ""))
	(setq potential-title
	      (url-generate-new-buffer-name url-current-file)))
    (if (and potential-title (not (string= potential-title "")))
	(rename-buffer potential-title)))
  (if url-find-this-link
      (w3-find-specific-link url-find-this-link))
  (w3-fix-extent-endpoints)
  (cond
   ((not (fboundp 'w3-insert-graphic)) nil) ; No graphics abilities
   (w3-delay-image-loads 
    ;; (w3-maybe-load-images)
    )
   (t
    (message "Processing images...")	; Grab the images
    (w3-load-delayed-images)
    (message "Done.")))
  (if (and (fboundp 'valid-specifier-locale-p)
	   (fboundp 'w3-insert-graphic)
	   (not w3-user-colors-take-precedence)
	   (not w3-delay-image-loads)
	   (not (eq (device-type) 'tty))
	   (w3-get-state 'background))
      (let* ((buffer (get-buffer-create " *background*"))
	     (url-working-buffer buffer)
	     (ourbuf (current-buffer))
	     (fname (url-generate-unique-filename "%s.xpm"))
	     (bitmap (w3-get-state 'background)))
	(save-excursion
	  (set-buffer buffer)
	  (setq url-be-asynchronous nil)
	  (erase-buffer)
	  (url-retrieve bitmap)
	  (w3-convert-graphic-to-useable-format buffer
						fname
						(not (featurep 'xpm)))
	  (erase-buffer)
	  (insert-file-contents fname)
	  (setq bitmap (buffer-string))
	  (kill-buffer buffer))
	(set-face-background-pixmap 'default
				    bitmap
				    (current-buffer))))
  (if (and w3-default-style
	   (fboundp 'make-face)
	   (fboundp 'find-face)
	   (find-face w3-default-style)
	   (face-differs-from-default-p w3-default-style))
      (if (not (fboundp 'valid-specifier-locale-p))
	  (w3-add-zone (point-min) (point-max) w3-default-style nil nil)
	(w3-my-safe-copy-face w3-default-style 'default (current-buffer))))
  (let ((pop-up-windows nil))
    (display-buffer (current-buffer))))

;;; from MULE contributors
(defun w3-show-invisible-href ()
  ;; Displaying `href', which is not seen in normal.
  (let ((buffer-read-only nil)
	hlist line beg props data ovl)
    (while w3-invisible-href-list
      (setq hlist (car w3-invisible-href-list)
	    w3-invisible-href-list (cdr w3-invisible-href-list)
	    line (car hlist)
	    data nil)
      (goto-line line)
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (and (string-match
	    (cond ((stringp w3-auto-image-alt)
		   (concat
		    (regexp-quote
		     (substring w3-auto-image-alt 0
				(string-match "%s" w3-auto-image-alt)))
		    ".*"
		    (regexp-quote
		     (substring w3-auto-image-alt (match-end 0)
				(string-match "[ \t]*$" w3-auto-image-alt)))
		    "[ \t]*$")
		   )
		  (t
		   (concat (regexp-quote "[IMAGE(") ".*"
			   (regexp-quote ")]") "[ \t]*$")
		   ))
	    (buffer-substring beg (point)))
	   (progn
	     (setq data (w3-zone-at (+ beg (match-beginning 0)))
		   data (if data (prog1
				     (w3-zone-data data)
				   (w3-delete-zone data)))
		   data (if (and data (eq (car data) 'w3))
			    (cdr data)))
	     (setq beg (point))
	     (and data
		  (progn
		    (insert " ")
		    (w3-put-state 'href (url-expand-file-name (nth 1 data)))
		    (w3-handle-text (car (cdr (cdr hlist))))
		    (w3-put-state 'href nil)
		    (w3-add-zone (1+ beg) (point) 'w3-graphic-face nil nil)
		    )))))
    (set-buffer-modified-p nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-delimit-headers (st nd &optional end)
  (if (and end w3-delimit-emphasis)
      (let* ((tag (intern (substring (symbol-name tag) 1)))
	     (beg-char (and w3-delimit-emphasis
			    end
			    (nth 1 (cdr-safe
				    (assoc tag w3-header-chars-assoc)))))
	     (end-char (and w3-delimit-emphasis
			    end
			    (nth 0 (cdr-safe
				    (assoc tag w3-header-chars-assoc)))))
	     (st-pos (w3-get-state 'header-start))
	     (nd-pos (point)))
	(if end-char
	    (progn
	      (goto-char nd-pos)
	      (insert "\n" (make-string (- nd-pos st-pos) end-char))))
	(if beg-char
	    (progn
	      (goto-char st-pos)
	      (insert "\n" (make-string (- nd-pos st-pos) beg-char)))))))

(defun w3-upcase-region (st nd &optional end)
  (and st nd (upcase-region st nd)))

(provide 'w3-draw)

