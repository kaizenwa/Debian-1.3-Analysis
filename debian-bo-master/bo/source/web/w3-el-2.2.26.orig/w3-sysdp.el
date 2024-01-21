;;; sysdep.el --- consolidate Emacs-version dependencies in one file.

;; Copyright (C) 1995 Ben Wing.

;; Author: Ben Wing <wing@netcom.com>
;; Keywords: lisp, tools
;; Version: 0.001

;; The purpose of this file is to eliminate the cruftiness that
;; would otherwise be required of packages that want to run on multiple
;; versions of Emacs.  The idea is that we make it look like we're running
;; the latest version of XEmacs (currently 19.12) by emulating all the
;; missing functions.

;; #### This file does not currently do any advising but should.
;; Unfortunately, advice.el is a hugely big package.  Is any such
;; thing as `advice-lite' possible?

;; #### - This package is great, but its role needs to be thought out a bit
;; more.  Sysdep will not permit programs written for the old XEmacs API to
;; run on new versions of XEmacs.  Sysdep is a backward-compatibility
;; package for the latest and greatest XEmacs API.  It permits programmers
;; to use the latest XEmacs functionality and still have their programs run
;; on older versions of XEmacs...perhaps even on FSF Emacs.  It should NEVER
;; ever need to be loaded in the newest XEmacs.  It doesn't even make sense
;; to put it in the lisp/utils part of the XEmacs distribution because it's
;; real purpose is to be distributed with packages like w3 which take
;; advantage of the latest and greatest features of XEmacs but still need to
;; be run on older versions.  --Stig

;; Any packages that wish to use this file should load it using
;; `load-library'.  It will not load itself if a version of sysdep.el
;; that is at least as recent has already been loaded, but will
;; load over an older version of sysdep.el.  It will attempt to
;; not redefine functions that have already been custom-redefined,
;; but will redefine a function if the supplied definition came from
;; an older version of sysdep.el.

;; Packages such as w3 that wish to include this file with the package
;; should rename it to something unique, such as `w3-sysdep.el', and
;; load it with `load-library'.  That will ensure that no conflicts
;; arise if more than one package in the load path provides a version
;; of sysdep.el.  If multiple packages load sysdep.el, the most recent
;; version will end up loaded; as long as I'm careful not to
;; introduce bugs in previously working definitions, this should work
;; fine.

;; You may well discover deficiencies in this file as you use it.
;; The preferable way of dealing with this is to send me a patch
;; to sysdep.el; that way, the collective body of knowledge gets
;; increased.

;; DO NOT load this file with `require'.
;; DO NOT put a `provide' statement in this file.

;; IMPORTANT: leave the version string in the format X.XXX (e.g. 1.001)
;; so that string comparisons to other versions work properly.

(defconst sysdep-potential-version "0.001")

(if (and (boundp 'sysdep-version)
	 (not (string-lessp sysdep-version sysdep-potential-version)))
    ;; if a more recent version of sysdep was already loaded,
    ;; or if the same package is loaded again, don't load.
    nil

(defconst sysdep-version sysdep-potential-version)

;; this macro means: define the function, but only if either it
;; wasn't bound before, or the supplied binding comes from an older
;; version of sysdep.el.  That way, user-supplied bindings don't
;; get overridden.

;; note: sysdep-defalias is often more useful than this function,
;; esp. since you can do load-time conditionalizing and can
;; optionally leave the function undefined. (e.g. frame functions
;; in v18.)

(defmacro sysdep-defun (function &rest everything-else)
  (` (cond ((or (not (fboundp (quote (, function))))
		(get (quote (, function)) 'sysdep-defined-this))
	    (put (quote (, function)) 'sysdep-defined-this t)
	    (defun (, function) (,@ everything-else))))))

(defmacro sysdep-defvar (function &rest everything-else)
  (` (cond ((or (not (boundp (quote (, function))))
		(get (quote (, function)) 'sysdep-defined-this))
	    (put (quote (, function)) 'sysdep-defined-this t)
	    (defvar (, function) (,@ everything-else))))))

(defmacro sysdep-defconst (function &rest everything-else)
  (` (cond ((or (not (boundp (quote (, function))))
		(get (quote (, function)) 'sysdep-defined-this))
	    (put (quote (, function)) 'sysdep-defined-this t)
	    (defconst (, function) (,@ everything-else))))))

;; similar for fset and defalias.  No need to quote as the argument
;; is already quoted.

(defmacro sysdep-fset (function def)
  (` (cond ((and (or (not (fboundp (, function)))
		     (get (, function) 'sysdep-defined-this))
		 (, def))
	    (put (, function) 'sysdep-defined-this t)
	    (fset (, function) (, def))))))

(defmacro sysdep-defalias (function def)
  (` (cond ((and (or (not (fboundp (, function)))
		     (get (, function) 'sysdep-defined-this))
		 (, def)
		 (or (listp (, def))
		     (and (symbolp (, def))
			  (fboundp (, def)))))
	    (put (, function) 'sysdep-defined-this t)
	    (defalias (, function) (, def))))))

;; bootstrapping: defalias and define-function don't exist
;; in older versions of lemacs

(sysdep-fset 'defalias 'fset)
(sysdep-defalias 'define-function 'defalias)

;; useful ways of determining what version is running
;; emacs-major-version and emacs-minor-version are
;; already defined in recent versions of FSF Emacs and XEmacs

(sysdep-defconst emacs-major-version
		 ;; will string-match ever fail?  If so, assume 19.0.
		 ;; (should we assume 18.something?)
		 (if (string-match "^[0-9]+" emacs-version)
		     (string-to-int
		      (substring emacs-version
				 (match-beginning 0) (match-end 0)))
		   19))

(sysdep-defconst emacs-minor-version
		 (if (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version)
		     (string-to-int
		      (substring emacs-version
				 (match-beginning 1) (match-end 1)))
		   0))

(sysdep-defconst sysdep-running-xemacs
		 (or (string-match "Lucid" emacs-version)
		     (string-match "XEmacs" emacs-version)))

(sysdep-defconst window-system nil)

(sysdep-defvar x-library-search-path '("/usr/X11R6/lib/X11/"
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
				       "/usr/openwin/share/lib/X11/")
  "Search path used for X11 libraries.")

;; frame-related stuff.

(sysdep-defalias 'buffer-dedicated-frame 'buffer-dedicated-screen)
(sysdep-defalias 'deiconify-frame
  (cond ((fboundp 'deiconify-screen) 'deiconify-screen)
	;; make-frame-visible will be defined as necessary
	(t 'make-frame-visible)))
(sysdep-defalias 'delete-frame 'delete-screen)
(sysdep-defalias 'event-frame 'event-screen)
(sysdep-defalias 'event-glyph-extent 'event-glyph)
(sysdep-defalias 'face-frame 'face-screen)
(sysdep-defalias 'find-file-other-frame 'find-file-other-screen)
(sysdep-defalias 'find-file-read-only-other-frame
  'find-file-read-only-other-screen)
(sysdep-defalias 'frame-height 'screen-height)
(sysdep-defalias 'frame-iconified-p 'screen-iconified-p)
(sysdep-defalias 'frame-left-margin-width 'screen-left-margin-width)
(sysdep-defalias 'frame-list 'screen-list)
(sysdep-defalias 'frame-live-p
  (cond ((fboundp 'screen-live-p) 'screen-live-p)
	((fboundp 'live-screen-p) 'live-screen-p)
	;; #### not sure if this is correct (this is for Epoch)
	;; but gnuserv.el uses it this way
	((fboundp 'screenp) 'screenp)))
(sysdep-defalias 'frame-name 'screen-name)
(sysdep-defalias 'frame-parameters 'screen-parameters)
(sysdep-defalias 'frame-pixel-height 'screen-pixel-height)
(sysdep-defalias 'frame-pixel-width 'screen-pixel-width)
(sysdep-defalias 'frame-right-margin-width 'screen-right-margin-width)
(sysdep-defalias 'frame-root-window 'screen-root-window)
(sysdep-defalias 'frame-selected-window 'screen-selected-window)
(sysdep-defalias 'frame-totally-visible-p 'screen-totally-visible-p)
(sysdep-defalias 'frame-visible-p 'screen-visible-p)
(sysdep-defalias 'frame-width 'screen-width)
(sysdep-defalias 'framep 'screenp)
(sysdep-defalias 'get-frame-for-buffer 'get-screen-for-buffer)
(sysdep-defalias 'get-frame-for-buffer-noselect 'get-screen-for-buffer-noselect)
(sysdep-defalias 'get-other-frame 'get-other-screen)
(sysdep-defalias 'iconify-frame 'iconify-screen)
(sysdep-defalias 'lower-frame 'lower-screen)
(sysdep-defalias 'mail-other-frame 'mail-other-screen)

(sysdep-defalias 'make-frame
  (cond ((fboundp 'make-screen)
	 (function (lambda (&optional parameters device)
		     (make-screen parameters))))
	((fboundp 'x-create-screen)
	 (function (lambda (&optional parameters device)
		     (x-create-screen parameters))))))

(sysdep-defalias 'make-frame-invisible 'make-screen-invisible)
(sysdep-defalias 'make-frame-visible
  (cond ((fboundp 'make-screen-visible) 'make-screen-visible)
	((fboundp 'mapraised-screen) 'mapraised-screen)
	((fboundp 'x-remap-window)
	 (lambda (&optional x)
	   (x-remap-window)
	   (accept-process-output)))))
(sysdep-defalias 'modify-frame-parameters 'modify-screen-parameters)
(sysdep-defalias 'new-frame 'new-screen)
(sysdep-defalias 'next-frame 'next-screen)
(sysdep-defalias 'next-multiframe-window 'next-multiscreen-window)
(sysdep-defalias 'other-frame 'other-screen)
(sysdep-defalias 'previous-frame 'previous-screen)
(sysdep-defalias 'previous-multiframe-window 'previous-multiscreen-window)
(sysdep-defalias 'raise-frame
  (cond ((fboundp 'raise-screen) 'raise-screen)
	((fboundp 'mapraise-screen) 'mapraise-screen)))
(sysdep-defalias 'redraw-frame 'redraw-screen)
(sysdep-defalias 'select-frame 'select-screen)
(sysdep-defalias 'selected-frame 'selected-screen)
(sysdep-defalias 'set-buffer-dedicated-frame 'set-buffer-dedicated-screen)
(sysdep-defalias 'set-frame-height 'set-screen-height)
(sysdep-defalias 'set-frame-left-margin-width 'set-screen-left-margin-width)
(sysdep-defalias 'set-frame-position 'set-screen-position)
(sysdep-defalias 'set-frame-right-margin-width 'set-screen-right-margin-width)
(sysdep-defalias 'set-frame-size 'set-screen-size)
(sysdep-defalias 'set-frame-width 'set-screen-width)
(sysdep-defalias 'show-temp-buffer-in-current-frame 'show-temp-buffer-in-current-screen)
(sysdep-defalias 'switch-to-buffer-other-frame 'switch-to-buffer-other-screen)
(sysdep-defalias 'visible-frame-list 'visible-screen-list)
(sysdep-defalias 'window-frame 'window-screen)
(sysdep-defalias 'x-create-frame 'x-create-screen)
(sysdep-defalias 'x-set-frame-icon-pixmap 'x-set-screen-icon-pixmap)
(sysdep-defalias 'x-set-frame-pointer 'x-set-screen-pointer)
(sysdep-defalias 'x-display-color-p 'x-color-display-p)
(sysdep-defalias 'x-display-grayscale-p 'x-grayscale-display-p)
(sysdep-defalias 'menu-event-p 'misc-user-event-p)

(sysdep-defun add-submenu (menu-path submenu &optional before)
  "Add a menu to the menubar or one of its submenus.
If the named menu exists already, it is changed.
MENU-PATH identifies the menu under which the new menu should be inserted.
 It is a list of strings; for example, (\"File\") names the top-level \"File\"
 menu.  (\"File\" \"Foo\") names a hypothetical submenu of \"File\".
 If MENU-PATH is nil, then the menu will be added to the menubar itself.
SUBMENU is the new menu to add.
 See the documentation of `current-menubar' for the syntax.
BEFORE, if provided, is the name of a menu before which this menu should
 be added, if this menu is not on its parent already.  If the menu is already
 present, it will not be moved."
  (add-menu menu-path (car submenu) (cdr submenu) before))

(sysdep-defun add-menu-button (menu-path menu-leaf &optional before)
  "Add a menu item to some menu, creating the menu first if necessary.
If the named item exists already, it is changed.
MENU-PATH identifies the menu under which the new menu item should be inserted.
 It is a list of strings; for example, (\"File\") names the top-level \"File\"
 menu.  (\"File\" \"Foo\") names a hypothetical submenu of \"File\".
MENU-LEAF is a menubar leaf node.  See the documentation of `current-menubar'.
BEFORE, if provided, is the name of a menu item before which this item should
 be added, if this item is not on the menu already.  If the item is already
 present, it will not be moved."
 (add-menu-item menu-path (aref menu-leaf 0) (aref menu-leaf 1)
		(aref menu-leaf 2) before))

(sysdep-defun make-glyph (&optional spec-list)
  (if (and spec-list (cdr-safe (assq 'x spec-list)))
      (make-pixmap (cdr-safe (assq 'x spec-list)))))

(sysdep-defalias 'face-list 'list-faces)

;; Device functions
(sysdep-defalias 'selected-device 'ignore)

(sysdep-defun device-baud-rate (&optional device)
  "Return the output baud rate of DEVICE."
  baud-rate)

(sysdep-defun device-name (&optional device)
  "Return the name of the specified device."
  ;; doesn't handle the 19.29 multiple X display stuff yet
  ;; doesn't handle NeXTStep either
  (cond
   ((null window-system) "stdio")
   ((getenv "DISPLAY")
    (let ((str (getenv "DISPLAY"))
	  (x (1- (length (getenv "DISPLAY"))))
	  (y 0))
      (while (/= y x)
	(if (or (= (aref str y) ?:)
		(= (aref str y) ?.))
	    (aset str y ?-))
	(setq y (1+ y)))
      str))
   (t "stdio")))

(sysdep-defalias 'device-color-cells
  (cond
   ((null window-system) 'ignore)
   ((fboundp 'display-color-cells) 'display-color-cells)
   ((fboundp 'x-display-color-cells) 'x-display-color-cells)
   ((fboundp 'ns-display-color-cells) 'ns-display-color-celles)
   (t 'ignore)))

(sysdep-defun try-font-name (fontname &rest args)
  (car-safe (x-list-fonts fontname)))

(sysdep-defalias 'device-pixel-width
  (cond
   ((and (eq window-system 'x) (fboundp 'x-display-pixel-width))
    'x-display-pixel-width)
   ((and (eq window-system 'ns) (fboundp 'ns-display-pixel-width))
    'ns-display-pixel-width)
   (t 'ignore)))

(sysdep-defalias 'device-pixel-height
  (cond
   ((and (eq window-system 'x) (fboundp 'x-display-pixel-height))
    'x-display-pixel-height)
   ((and (eq window-system 'ns) (fboundp 'ns-display-pixel-height))
    'ns-display-pixel-height)
   (t 'ignore)))

(sysdep-defalias 'device-mm-width
  (cond
   ((and (eq window-system 'x) (fboundp 'x-display-mm-width))
    'x-display-mm-width)
   ((and (eq window-system 'ns) (fboundp 'ns-display-mm-width))
    'ns-display-mm-width)
   (t 'ignore)))

(sysdep-defalias 'device-mm-height
  (cond
   ((and (eq window-system 'x) (fboundp 'x-display-mm-height))
    'x-display-mm-height)
   ((and (eq window-system 'ns) (fboundp 'ns-display-mm-height))
    'ns-display-mm-height)
   (t 'ignore)))

(sysdep-defalias 'device-bitplanes
  (cond
   ((and (eq window-system 'x) (fboundp 'x-display-planes))
    'x-display-planes)
   ((and (eq window-system 'ns) (fboundp 'ns-display-planes))
    'ns-display-planes)
   (t 'ignore)))

(sysdep-defalias 'device-class
  (cond
   ((and (eq window-system 'x) (fboundp 'x-display-visual-class))
    (function
     (lambda (&optional device)
       (let ((val (symbol-name (x-display-visual-class device))))
	 (cond
	  ((string-match "color" val) 'color)
	  ((string-match "gray-scale" val) 'grayscale)
	  (t 'mono))))))
   ((fboundp 'number-of-colors)
    (function
     (lambda (&optional device)
       (if (= 2 (number-of-colors))
	   'mono
	 'color))))
   ((and (eq window-system 'x) (fboundp 'x-color-p))
    (function
     (lambda (&optional device)
       (if (x-color-p)
	   'color
	 'mono))))
   ((and (eq window-system 'ns) (fboundp 'ns-display-visual-class))
    (function
     (lambda (&optional device)
       (let ((val (symbol-name (ns-display-visual-class))))
	 (cond
	  ((string-match "color" val) 'color)
	  ((string-match "gray-scale" val) 'grayscale)
	  (t 'mono))))))
   (t (function (lambda (&optional device) 'mono)))))
  
(sysdep-defun device-type (&optional device)
  "Return the type of the specified device (e.g. `x' or `tty').
Value is `tty' for a tty device (a character-only terminal),
`x' for a device which is a connection to an X server,
'ns' for a device which is a connection to a NeXTStep dps server,
'win32' for a Windows-NT window,
'pm' for an OS/2 Presentation Manager window,
'intuition' for an Amiga screen"
  (or window-system 'tty))

;; misc
(sysdep-defun match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num)))))

(sysdep-defun add-hook (hook-var function &optional at-end)
  "Add a function to a hook.
First argument HOOK-VAR (a symbol) is the name of a hook, second
 argument FUNCTION is the function to add.
Third (optional) argument AT-END means to add the function at the end
 of the hook list instead of the beginning.  If the function is already
 present, this has no effect.
Returns nil if FUNCTION was already present in HOOK-VAR, else new
 value of HOOK-VAR."
      (if (not (boundp hook-var)) (set hook-var nil))
      (let ((old (symbol-value hook-var)))
	(if (or (not (listp old)) (eq (car old) 'lambda))
	    (setq old (list old)))
	(if (member function old)
	    nil
	  (set hook-var
	       (if at-end
		   (append old (list function)) ; don't nconc
		 (cons function old))))))

(sysdep-defalias 'valid-color-name-p
  (cond
   ((fboundp 'x-valid-color-name-p)	; XEmacs/Lucid
    'x-valid-color-name-p)
   ((and window-system
	 (fboundp 'color-defined-p))	; NS/Emacs 19
    'color-defined-p)
   ((and window-system
	 (fboundp 'x-color-defined-p))	; Emacs 19
    'x-color-defined-p)
   ((fboundp 'get-color)		; Epoch
    (function (lambda (color)
		(let ((x (get-color color)))
		  (if x
		      (setq x (progn
				(free-color x)
				t)))
		  x))))
   (t 'identity)))			; All others

;; Misc.
(sysdep-defun split-string (string pattern)
  "Return a list of substrings of STRING which are separated by PATTERN."
  (let (parts (start 0))
    (while (string-match pattern string start)
      (setq parts (cons (substring string start (match-beginning 0)) parts)
	    start (match-end 0)))
    (nreverse (cons (substring string start) parts))
    ))

(sysdep-defun member (elt list)
  (while (and list (not (equal elt (car list))))
    (setq list (cdr list)))
  list)

(sysdep-defun rassoc (key list)
  (let ((found nil))
    (while (and list (not found))
      (if (equal (cdr (car list)) key) (setq found (car list)))
      (setq list (cdr list)))
    found))

(sysdep-defun display-error (error-object stream)
  "Display `error-object' on `stream' in a user-friendly way."
  (funcall (or (let ((type (car-safe error-object)))
		 (catch 'error
		   (and (consp error-object)
			(symbolp type)
			;;(stringp (get type 'error-message))
			(consp (get type 'error-conditions))
			(let ((tail (cdr error-object)))
			  (while (not (null tail))
			    (if (consp tail)
				(setq tail (cdr tail))
			      (throw 'error nil)))
			  t)
			;; (check-type condition condition)
			(get type 'error-conditions)
			;; Search class hierarchy
			(let ((tail (get type 'error-conditions)))
			  (while (not (null tail))
			    (cond ((not (and (consp tail)
					     (symbolp (car tail))))
				   (throw 'error nil))
				  ((get (car tail) 'display-error)
				   (throw 'error (get (car tail)
						      'display-error)))
				  (t
				   (setq tail (cdr tail)))))
			  ;; Default method
			  (function
			   (lambda (error-object stream)
			     (let ((type (car error-object))
				   (tail (cdr error-object))
				   (first t))
			       (if (eq type 'error)
				   (progn (princ (car tail) stream)
					  (setq tail (cdr tail)))
				 (princ (or (get type 'error-message) type)
					stream))
			       (while tail
				 (princ (if first ": " ", ") stream)
				 (prin1 (car tail) stream)
				 (setq tail (cdr tail)
				       first nil)))))))))
	       (function
		(lambda (error-object stream)
		  (princ "Peculiar error " stream)
		  (prin1 error-object stream))))
	   error-object stream))

(sysdep-defun find-face (face)
  (car-safe (memq face (face-list))))

;; window functions

;; not defined in v18
(sysdep-defun eval-buffer (bufname &optional printflag)
  (save-excursion
    (set-buffer bufname)
    (eval-current-buffer)))

(sysdep-defun window-minibuffer-p (window)
  "Returns non-nil if WINDOW is a minibuffer window."
  (eq window (minibuffer-window)))

;; not defined in v18
(sysdep-defun window-live-p (window)
  "Returns t if OBJ is a window which is currently visible."
  (and (windowp window)
       (window-point window)))

;; this parenthesis closes the if statement at the top of the file.

)

;; DO NOT put a provide statement here.  This file should never be
;; loaded with `require'.  Use `load-library' instead.

;;; sysdep.el ends here

;;;(sysdep.el) Local Variables:
;;;(sysdep.el) eval: (put 'sysdep-defun 'lisp-indent-function 'defun)
;;;(sysdep.el) eval: (put 'sysdep-defalias 'lisp-indent-function 'defun)
;;;(sysdep.el) eval: (put 'sysdep-defconst 'lisp-indent-function 'defun)
;;;(sysdep.el) eval: (put 'sysdep-defvar 'lisp-indent-function 'defun)
;;;(sysdep.el) End:
