;;; w3-e19.el,v --- Emacs 19.xx specific functions for emacs-w3
;; Author: wmperry
;; Created: 1995/10/28 03:53:54
;; Version: 1.132
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
;;; Enhancements For Emacs 19
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-e19-popup-menu nil
  "A menu for button3.")

(defvar w3-air-hotlists nil
  "*A list of AIR-Mosaic hotlists to put in the menubar.")

(defvar menu-bar-w3-help-menu (if (boundp 'menu-bar-help-menu)
				  (copy-keymap menu-bar-help-menu)
				nil)
  "*A copy of the help menu so W3 can add its own items.")

(defvar w3-html-faq
  "http://www.ncsa.uiuc.edu/General/Internet/WWW/HTMLPrimer.html"
  "*The location of the HTML FAQ to put in the help menu.")

(defvar w3-url-faq "http://www.ncsa.uiuc.edu/demoweb/url-primer.html"
  "*The location of the URL FAQ to put in the help menu.")

(defvar w3-mode-file-menu nil)
(defvar w3-air-hotlist-menu nil)
(defvar w3-mode-go-menu nil)
(defvar w3-print-menu nil)
(defvar w3-mail-menu nil)
(defvar w3-hotlist-menu nil)

(defun w3-e19-setup-menus ()
  ;; Setup base menus for emacs 19
  (add-hook 'menu-bar-final-items 'w3help)
  (add-hook 'menu-bar-final-items 'hotlist)
  (define-key w3-mode-map [menu-bar help] 'undefined)
  ;; Emacs 19.29 and later use help-menu as the keyname
  (define-key w3-mode-map [menu-bar help-menu] 'undefined)
  (define-key w3-mode-map [menu-bar w3help] (cons "Help"
						  menu-bar-w3-help-menu))

  (let ((cntr 0))
    (mapcar
     (function
      (lambda (dat)
	(setq cntr (1+ cntr))
	(if (stringp dat)
	    (setq dat (cons "" 'ignore)))
	(define-key menu-bar-w3-help-menu
	  (vector (intern (concat "w3" (int-to-string cntr))))
	  dat)))
     (nreverse
      (list
       (cons "About W3" (function
			 (lambda ()
			   (interactive)
			   (w3-fetch "about:"))))
       (cons "W3 Manual"
	     (list 'lambda () '(interactive)
		   (list 'w3-fetch (concat w3-documentation-root
					   "docs/w3_toc.html"))))
       "---"
       (cons (concat "Help on v" w3-version-number)
	     (list 'lambda () '(interactive)
		   (list 'w3-fetch (concat w3-documentation-root
					   "help_on_" 
					   w3-version-number ".html"))))
       (cons "On Window" (list 'lambda () '(interactive)
			       (list 'w3-fetch
				     (concat w3-documentation-root
					     "window-help.html"))))
       (cons "On FAQ" (list 'lambda () '(interactive)
			    (list 'w3-fetch (concat w3-documentation-root
						    "FAQ.html"))))
       "---"
       (cons "On HTML" '(lambda () (interactive) (w3-fetch w3-html-faq)))
       (cons "On URLs" '(lambda () (interactive) (w3-fetch w3-url-faq)))
       (cons "Submit W3 Bug" 'w3-submit-bug)
       (cons "--" 'ignore)))))
  
  (setq w3-e19-popup-menu (make-sparse-keymap "WWW"))
  (define-key w3-e19-popup-menu [kill] (cons "Leave & kill buffer"
					     'w3-quit))
  (define-key w3-e19-popup-menu [leave] (cons "Leave & bury buffer"
					      'w3-leave-buffer))
  (define-key w3-e19-popup-menu [sep0] (cons "----" 'ignore))
  (define-key w3-e19-popup-menu [anno] (cons "Add annotation"
					     'w3-annotation-add))
  (define-key w3-e19-popup-menu [sep1] (cons "----" 'ignore))
  (define-key w3-e19-popup-menu [sep2] (cons "----" 'ignore))
  (define-key w3-e19-popup-menu [copyurl] (cons "Copy URL to clipboard"
						'w3-save-url))
  (define-key w3-e19-popup-menu [url] (cons "Open URL" 'w3-fetch))
  (define-key w3-e19-popup-menu [file] (cons "Open file" 'w3-open-local))

  (define-key w3-mode-map [menu-bar view]
    (cons "View" (make-sparse-keymap "View")))
  (if (fboundp 'w3-show-graphics)
      (define-key w3-mode-map [menu-bar view show-graphics]
	(cons "Show Graphics" 'w3-show-graphics)))
  (define-key w3-mode-map [menu-bar view reload]
    (cons "Reload" 'w3-reload-document))
  (define-key w3-mode-map [menu-bar view refresh]
    (cons "Refresh" 'w3-refresh-buffer))
  (define-key w3-mode-map [menu-bar view separator]
    (cons "----" 'ignore))
  (define-key w3-mode-map [menu-bar view source]
    (cons "Document Source" 'w3-source-document))
  (define-key w3-mode-map [menu-bar view info]
    (cons "Document Information" 'w3-document-information))

  (setq w3-mode-go-menu (make-sparse-keymap "Go"))
  (define-key w3-mode-go-menu [links] (cons "Links..."
					    'w3-e19-show-links-menu))
  (define-key w3-mode-go-menu [go-sep1] '("--"))
  (define-key w3-mode-go-menu [go-hist4] 'undefined)
  (define-key w3-mode-go-menu [go-hist3] 'undefined)
  (define-key w3-mode-go-menu [go-hist2] 'undefined)
  (define-key w3-mode-go-menu [go-hist1] 'undefined)
  (define-key w3-mode-go-menu [go-hist0] 'undefined)
  (define-key w3-mode-go-menu [go-sep0] '("--"))
  (define-key w3-mode-go-menu [history]
    (cons "View History" 'w3-show-history-list))
  (define-key w3-mode-go-menu [go-sep1]
    '("--"))
  (define-key w3-mode-go-menu [home]
    (cons "Home" 'w3))
  (define-key w3-mode-go-menu [back]
    (cons "Back" 'w3-backward-in-history))
  (define-key w3-mode-go-menu [forw]
    (cons "Forward" 'w3-forward-in-history))

  (setq w3-mail-menu (make-sparse-keymap "Mail"))
  (define-key w3-mail-menu [latex]
    '("LaTeX Source" . (lambda ()
		    (interactive)
		    (w3-mail-current-document nil "LaTeX Source"))))
  (define-key w3-mail-menu [postscript]
    '("PostScript" . (lambda ()
		       (interactive)
		       (w3-mail-current-document nil "PostScript"))))
  (define-key w3-mail-menu [text]
    '("Formatted Text" . (lambda ()
			   (interactive)
			   (w3-mail-current-document nil "Formatted Text"))))
  (define-key w3-mail-menu [html]
    '("HTML Source" . (lambda ()
			(interactive)
			(w3-mail-current-document nil "HTML Source"))))

  (setq w3-print-menu (make-sparse-keymap "Print"))
  (define-key w3-print-menu [latex]
    '("LaTeX'd" . (lambda ()
		    (interactive)
		    (w3-print-this-url nil "LaTeX'd"))))
  (define-key w3-print-menu [postscript]
    '("PostScript" . (lambda ()
		       (interactive)
		       (w3-print-this-url nil "PostScript"))))
  (define-key w3-print-menu [text]
    '("Formatted Text" . (lambda ()
			   (interactive)
			   (w3-print-this-url nil "Formatted Text"))))
  (define-key w3-print-menu [html]
    '("HTML Source" . (lambda ()
			(interactive)
			(w3-print-this-url nil "HTML Source"))))
  ;; Disable some file menu items
  (define-key w3-mode-map [menu-bar file] 'undefined)
  (define-key w3-mode-map [menu-bar files] 'undefined)
  (setq w3-mode-file-menu (make-sparse-keymap "File"))
  (define-key w3-mode-file-menu [die]
    (cons "Exit Emacs" 'save-buffers-kill-emacs))
  (define-key w3-mode-file-menu [separator-exit]
    '("--"))
  (define-key w3-mode-file-menu [quit]
    (cons "Kill Buffer" 'w3-quit))
  (define-key w3-mode-file-menu [leave]
    (cons "Leave Buffer" 'w3-leave-buffer))
  (define-key w3-mode-file-menu [separator-misc]
    '("--"))
  (define-key w3-mode-file-menu [mail]
    (cons "Mail Document..." w3-mail-menu))
  (define-key w3-mode-file-menu [print-buffer]
    (cons "Print..." w3-print-menu))
  (define-key w3-mode-file-menu [annotate]
    (cons "Add Annotation" 'w3-annotation-add))
  (define-key w3-mode-file-menu [separator-frames]
    '("--"))
  (define-key w3-mode-file-menu [delete-frame]
    '("Delete Frame" . delete-frame))
  (define-key w3-mode-file-menu [make-frame]
    '("Make New Frame" . make-frame))
  (define-key w3-mode-file-menu [separator-buffers]
    '("--"))
  (define-key w3-mode-file-menu [bookmark]
    '("Bookmarks" . menu-bar-bookmark-map))
  (define-key w3-mode-file-menu [write-file]
    '("Save Buffer As..." . write-file))
  (define-key w3-mode-file-menu [open-local]
    (cons "Open File" 'w3-open-local))
  (define-key w3-mode-file-menu [open-url]
    (cons "Open URL" 'w3-fetch))
  (setq w3-hotlist-menu (make-sparse-keymap "Hotlist"))
  (define-key w3-hotlist-menu [pull]
    (cons "Hotlist..." 'w3-e19-show-hotlist-menu))
  (define-key w3-hotlist-menu [sep1] '("--"))
  (define-key w3-hotlist-menu [append]
    (cons "Append new hotlist file" 'w3-hotlist-append))
  (define-key w3-hotlist-menu [rename]
    (cons "Rename item in hotlist" 'w3-hotlist-rename-entry))
  (define-key w3-hotlist-menu [delete]
    (cons "Delete item from hotlist" 'w3-hotlist-delete))
  (define-key w3-hotlist-menu [add]
    (cons "Add this document to hotlist" 'w3-hotlist-add-document))
  (define-key w3-hotlist-menu [view]
    (cons "View Hotlist..." 'w3-show-hotlist))
  (define-key w3-mode-map [menu-bar hot]
    (cons "Hotlist" w3-hotlist-menu))
  (define-key w3-mode-map [menu-bar go]
    (cons "Go" w3-mode-go-menu))
  (define-key w3-mode-map [menu-bar options]
    '("Options" . w3-e19-options-menu))
  (define-key w3-mode-map [menu-bar w3file]
    (cons "File" w3-mode-file-menu)))

; This needs to be re-implemented (if possible) for the hashtable version
; of url-history-list
;(defun w3-shuffle-history-menu ()
;  ;; Set up the history menu
;  (if (keymapp w3-mode-go-menu)
;      (let ((hist-size 4)
;	    (key nil))
;	(while (>= hist-size 0)
;	  (setq key (intern (concat "go-hist" (int-to-string hist-size))))
;	  (if (nth hist-size url-history-list)
;	      (define-key w3-mode-go-menu (vector key)
;		(cons (cdr (nth hist-size url-history-list))
;		      (list 'lambda () '(interactive)
;			    (list 'w3-fetch
;				  (car (nth hist-size url-history-list))))))
;	    (define-key w3-mode-go-menu (vector key) 'undefined))
;	  (setq hist-size (1- hist-size))))
;    (w3-warn 'emacs19 "Something wrong! w3-mode-go-menu is not a keymap!")))

(defun w3-create-hotlist-menu (chunk)
  ;; Create a hotlist menu as a native menu bar.  Returns a keymap
  ;; representing CHUNK as a menu.  Called recursively as often as
  ;; necessary.
  (let ((keymap (make-sparse-keymap (car chunk)))
	(ctr 0)
	(ttl (car chunk)))
    (setq chunk (cdr (cdr chunk)))
    (while chunk
      (cond
       ((and (listp (car chunk))
	     (null (nth 1 (car chunk))))
	(define-key keymap (vector (intern (concat "hot" (int-to-string ctr))))
	  (w3-create-hotlist-menu (car chunk))))
       ((listp (car chunk))
	(define-key keymap (vector (intern (concat "hot" (int-to-string ctr))))
	  (cons (nth 0 (car chunk))
		(list 'lambda () '(interactive)
		      (list 'w3-fetch
			    (nth 1 (car chunk)))))))
       (t 'undefined))
      (setq chunk (cdr chunk)
	    ctr (1+ ctr)))
    (cons ttl keymap)))

(defun w3-initialize-hotlist-menu (&optional path)
  (let ((tmp w3-air-hotlists)
	(ctr 0)
	(dat nil))
    (setq w3-air-hotlist-menu (make-sparse-keymap "Hotlists"))
    (while tmp
      (if (file-exists-p (car tmp))
	  (progn
	    (setq dat (w3-create-hotlist-menu (w3-parse-air-hotlist (car tmp)))
		  ctr (1+ ctr))
	    (if (listp dat)
		(define-key w3-air-hotlist-menu
		  (vector (intern (concat "hot" (int-to-string ctr)))) dat))))
      (setq tmp (cdr tmp)))))
	      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some hackery to get emacs19 to do bold/underline/etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tips for creating a new w3-emacs19-hack-XXX function:
;;; use /etc/termcap, and look for these fields in the definition for your
;;; terminal:
;;; us = turn on underline
;;; ue = turn off underline
;;; md = bold
;;; se = normal

(defun w3-emacs19-hack-vt100 ()
  ;; Hack 'faces' for ttys (vt100)
  (or standard-display-table
      (setq standard-display-table (make-vector 261 nil)))
  (aset standard-display-table 1 (vector (create-glyph "\e[4m ")))
  (aset standard-display-table 2 (vector (create-glyph "\e[m ")))
  (aset standard-display-table 3 (vector (create-glyph "\e[1m ")))
  (aset standard-display-table 4 (vector (create-glyph "\e[m ")))
  )

(fset 'w3-emacs19-hack-vt102 'w3-emacs19-hack-vt100)
(fset 'w3-emacs19-hack-vt200 'w3-emacs19-hack-vt100)
(fset 'w3-emacs19-hack-vt220 'w3-emacs19-hack-vt100)
(fset 'w3-emacs19-hack-vt300 'w3-emacs19-hack-vt100)
(fset 'w3-emacs19-hack-vt320 'w3-emacs19-hack-vt100)
(fset 'w3-emacs19-hack-xterms 'w3-emacs19-hack-xterm)

(defun w3-emacs19-hack-xterm ()
  ;; Hack 'faces' for ttys (xterm)
  (or standard-display-table
      (setq standard-display-table (make-vector 261 nil)))
  (aset standard-display-table 1 (vector (create-glyph "\e[4m ")))
  (aset standard-display-table 2 (vector (create-glyph "\e[m ")))
  (aset standard-display-table 3 (vector (create-glyph "\e[5m ")))
  (aset standard-display-table 4 (vector (create-glyph "\e[m ")))
  )

(defun w3-emacs19-hack-console ()
  ;; Hack 'faces' for ttys (linux-console)
  ;; This isn't exactly right, but close enough
  (or standard-display-table
      (setq standard-display-table (make-vector 261 nil)))
  (aset standard-display-table 1 (vector (create-glyph "\e[1m ")))
  (aset standard-display-table 2 (vector (create-glyph "\e[m ")))
  (aset standard-display-table 3 (vector (create-glyph "\e[4m ")))
  (aset standard-display-table 4 (vector (create-glyph "\e[m ")))
  )

(defun w3-emacs19-unhack-faces ()
  "Remove faces hacks in emacs 19"
  (interactive)
  (standard-display-default 1 4)
  (setq w3-delimit-emphasis t
	w3-delimit-links t))

(defvar w3-links-menu nil "Menu for w3-mode in emacs 19.")
(make-variable-buffer-local 'w3-links-menu)

(defun w3-e19-options-menu (e)
  (interactive "e")
  (let ((val (x-popup-menu
	      e
	      (list
	       ""
	       (cons
		""
		(mapcar (function
			 (lambda (x) (cons
				      (concat 
				       (if (and (boundp (car x))
						(symbol-value (car x)))
					   "* "
					 "  ")
				       (cdr x)) (car x))))
			'(
			  (w3-dump-to-disk          . "Download to disk")
			  (url-automatic-caching    . "Automatic Caching")
			  (url-standalone-mode      . "Rely solely on cache")
			  (ps-print-color-p         . "Color Printing")
			  (url-honor-refresh-requests . "Honor Automatic Refreshes")
			  (w3-user-colors-take-precedence .
							  "Honor Color Requests")
			  (url-use-hypertext-gopher . "Hypertext Gopher")
			  (url-use-hypertext-dired  . "Hypertext Dired"))))))))
    (cond
     ((null val) nil)
     (t (eval (list 'setq val (list 'not val)))))))

(defun w3-create-faces ()
  ;; Create faces, the emacs 19 way
  (if (and (memq (device-type)
		 '(x			; X-windows?
		   ns			; NeXTStep?
		   pm			; OS/2?
		   win32		; Windows NT?
		   ))
	   (fboundp 'make-face))
      (progn
	(make-face w3-node-style)
	(make-face w3-visited-node-style)
	(make-face w3-default-style)
	(make-face w3-active-node-style)
	(if (not (face-differs-from-default-p w3-visited-node-style))
	    (copy-face 'bold-italic w3-visited-node-style))
	(if (not (face-differs-from-default-p w3-active-node-style))
	    (copy-face 'bold-italic w3-active-node-style))
	(if (not (face-differs-from-default-p w3-node-style))
	    (copy-face 'bold w3-node-style)))
    (setq w3-default-style nil)))

(defun w3-add-hotlist-menu ()
  ;; Add the hotlist menu to this buffer - used when it changes.
  (let ((hot-menu (make-sparse-keymap "w3-hotlist"))
	(ctr 0)
	(hot w3-hotlist))
    (while hot
      (define-key hot-menu (vector (intern (concat "w3-hotlist-"
						   (int-to-string ctr))))
	(cons (car (car hot))
	      (list 'lambda () '(interactive)
		    (list 'w3-fetch (car (cdr (car hot)))))))
      (setq ctr (1+ ctr)
	    hot (cdr hot)))
    (setq w3-e19-hotlist-menu hot-menu)))

(defun w3-link-at (pt)
  "Return the link(s) at point"
  (get-text-property pt 'w3))

(defun w3-follow-mouse-other-frame (e)
  "Function suitable to being bound to a mouse key.  Follows the link under
the mouse click, opening it in another frame."
  (interactive "e")
  (mouse-set-point e)
  (w3-follow-link-other-frame))

(defun w3-follow-mouse-down (e)
  (interactive "e")
  (let* ((posn (event-start e))
	 (point (and posn (posn-point posn)))
	 (window (and posn (posn-window posn)))
	 (buff (and window (window-buffer window)))
	 (ext nil)
	 (event nil)
	 (old-face nil))
    (if (not buff)
	nil
      (set-buffer buff)
      (setq ext (w3-zone-at point))
      (cond
       ((not ext) nil)
       ((not (overlay-get ext 'w3))
	(w3-follow-mouse e)
	(goto-char point))
       (t
	(overlay-put ext 'mouse-face nil)
	(setq old-face (overlay-get ext 'face))
	(overlay-put ext 'face w3-active-node-style)
	(while (and (setq event (read-event))
		    (not (eq (event-basic-type event) 'mouse-2)))
	  nil)
	(overlay-put ext 'face old-face)
	(overlay-put ext 'mouse-face 'highlight)
	(if (or (< (posn-point (event-end event))
		   (overlay-start ext))
		(> (posn-point (event-end event))
		   (overlay-end ext)))
	    nil
	  (w3-follow-mouse e)))))))

(defun w3-follow-mouse (e)
  "Function suitable to being bound to a mouse key.  Follows the link under
the mouse click."
  (interactive "e")
  (mouse-set-point e)
  (w3-follow-link))

(defun w3-follow-inlined-image-mouse (e)
  "Follow the inlined image under the mouse - ignore any hyperlinks or
form entry areas and blindly try to find an image."
  (interactive "e")
  (mouse-set-point e)
  (w3-follow-inlined-image))

(define-key w3-mode-map [down-mouse-2] 'w3-follow-mouse-down)
(define-key w3-mode-map [mouse-2] 'undefined)
(define-key w3-mode-map [down-mouse-3] 'w3-popup-menu)
(define-key w3-mode-map [S-mouse-2] 'w3-follow-mouse-other-frame)
(and (lookup-key global-map [mouse-movement])
     (define-key w3-mode-map [mouse-movement] 'w3-mouse-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to build menus of urls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-e19-show-hotlist-menu (e)
  (interactive "e")
  (let* ((x (condition-case ()
		(x-popup-menu e w3-e19-hotlist-menu)
	      (error nil)))		; to trap for empty menus
	 (y (and x (lookup-key w3-e19-hotlist-menu (apply 'vector x)))))
    (if (and x y)
	(funcall y))))

(defun w3-e19-show-links-menu (e)
  (interactive "e")
  (let* ((x (condition-case ()
		(x-popup-menu e w3-e19-links-menu)
	      (error nil)))		; to trap for empty menus
	 (y (and x (lookup-key w3-e19-links-menu (apply 'vector x)))))
    (if (and x y)
	(funcall y))))

(defun w3-build-FSF19-menu ()
  ;; Build emacs19 menus from w3-links-list
  (let* ((ctr 0)
	 (menu-ctr 0)
	 (tmp nil)
	 (ovls (nreverse (w3-only-links)))
	 (menus nil))
    (setq tmp (make-sparse-keymap "Links"))
    (while ovls
      (let ((data (w3-zone-data (car ovls))))
	(if (and (eq (car-safe data) 'w3) (nth 2 data))
	    (progn
	      (if (> ctr w3-max-menu-length)
		  (setq menus (cons tmp menus)
			ctr 0
			tmp (make-sparse-keymap
			     (concat "Links" (int-to-string
					      (setq menu-ctr
						    (1+ menu-ctr)))))))
	      (let ((ttl (w3-fix-spaces
			  (buffer-substring
			   (overlay-start (car ovls))
			   (overlay-end (car ovls)))))
		    (key (vector (intern (concat "link"
						 (int-to-string
						  (setq ctr (1+ ctr))))))))
		(if (and (> (length ttl) 0) (nth 2 data))
		    (define-key tmp key 
		      (cons ttl
			    (list 'lambda () '(interactive)
				  (list 'w3-fetch (nth 2 data))))))))))
      (setq ovls (cdr ovls)))
    (if (not menus)
	(setq w3-e19-links-menu tmp)
      (setq w3-e19-links-menu (make-sparse-keymap "LinkMenu")
	    menus (nreverse (cons tmp menus))
	    ctr 0)
      (while menus
	(define-key w3-e19-links-menu
	  (vector (intern (concat "SubMenu" ctr)))
	  (cons "More..." (car menus)))
	(setq menus (cdr menus)
	      ctr (1+ ctr))))))

(defun w3-popup-menu (e)
  "Pop up a menu of common w3 commands"
  (interactive "e")
  (mouse-set-point e)
  (let* ((ext (w3-zone-at (point)))
	 (dat (and ext (w3-zone-data ext)))
	 url val)
    (cond
     ((null dat)
      (setq val (x-popup-menu e w3-e19-popup-menu)
	    val (and val (lookup-key w3-e19-popup-menu (apply 'vector val)))
	    val (and val (call-interactively val))
	    val nil))
     ((eq (car dat) 'w3)		; hyperlink
      (setq val (x-popup-menu e (list "" (cons "" w3-hyperlink-menu)))
	    url (nth 2 dat)))
     ((or (eq (car dat) 'w3graphic)
	  (eq (car dat) 'w3delayed))
      (setq val (x-popup-menu e (list "" (cons "" w3-graphlink-menu)))
	    url (nth 1 dat)))
     (t (setq val (x-popup-menu e w3-e19-popup-menu))))
    (cond
     ((and val (fboundp val) url)
      (funcall val url))
     ((and val (fboundp val))
      (funcall val))
     (t nil))))

(defun w3-setup-version-specifics ()
  ;; Set up routine for emacs 19
  (if (and (eq (device-type) 'tty) w3-emacs19-hack-faces-p)
      (let ((hack-fn (intern (concat "w3-emacs19-hack-" (getenv "TERM")))))
	(if (fboundp hack-fn)
	    (funcall hack-fn)
	  (url-warn 'emacs19
		    (format "Don't know how to hack faces for %s..."
			    (getenv "TERM")))
	  (setq w3-emacs19-hack-faces-p nil))))
  (if (not (memq (device-type)
		 '(x pm ns)))		; Only load this up in X,
					; presentation manager, or
					; or NeXTstep, otherwise emacs
					; will barf
      nil
    (if (boundp 'track-mouse)
	(let ((x track-mouse))
	  (make-variable-buffer-local 'track-mouse)
	  (setq-default track-mouse x))))

  (if (boundp 'menu-bar-help-menu) (w3-e19-setup-menus))
  (cond 
   ((memq (device-type) '(x pm))
    (fset 'w3-x-popup-menu 'x-popup-menu))
   ((eq (device-type) 'ns)
    (fset 'w3-x-popup-menu 'ns-popup-menu))))

(defun w3-store-in-x-clipboard (str)
  "Store string STR in the Xwindows clipboard"
  (cond
   ((memq (device-type) '(x pm))
    (x-set-selection 'PRIMARY str))
   ((eq (device-type) 'ns) (ns-store-pasteboard-internal str))
   (t nil)))

(defun w3-mode-version-specifics ()
  ;; Emacs 19 specific stuff for w3-mode
  (if (and (eq 'tty (device-type)) w3-emacs19-hack-faces-p)
      (recenter 1))
  (if w3-track-mouse (setq track-mouse t))
  (if (or (memq (device-type) '(x pm ns)))
      (w3-build-FSF19-menu)))

(defun w3-map-links (function &optional buffer from to maparg)
  "Map FUNCTION over the hypertext links which overlap region in BUFFER,
starting at FROM and ending at TO.  FUNCTION is called with the arguments
linkdata, START, END, and MAPARG.
The arguments FROM, TO, MAPARG, and BUFFER default to the beginning of
BUFFER, the end of BUFFER, nil, and (current-buffer), respectively.

In emacs19, FROM, TO, and BUFFER are ignored.... working on it."
  (mapcar (function (lambda (x)
		      (funcall function (w3-zone-data x)
			       (overlay-start x) (overlay-end x) maparg)
		      nil)) (w3-only-links))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Alternate zone-functions for emacs 19.2x - these use overlays instead
;;; of text properties.  In emacs 19.22, text props cause lots of garbage
;;; collection.  Overlays don't appear to cause this problem.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fset 'w3-zone-start 'overlay-start)
(fset 'w3-zone-end 'overlay-end)
(fset 'w3-zone-eq 'eq)
(fset 'w3-delete-zone 'delete-overlay)
(fset 'w3-insert 'insert-before-markers)

(defun w3-fix-extent-endpoints ()
  ;; Make sure no extents have whitespace/newlines at the end of them
  (let ((ovls (overlay-lists)) st nd cur)
    (setq ovls (nconc (car ovls) (cdr ovls)))
    (while ovls
      (setq cur (car ovls)
	    ovls (cdr ovls)
	    st (overlay-start cur)
	    nd (overlay-end cur))
      (while (memq (char-after st) '(?  ?\n ?\t))
	(setq st (1+ st)))
      (move-overlay cur st nd))))

(defun w3-mouse-handler (e)
  "Function to message the url under the mouse cursor"
  (interactive "e")
  (let* ((pt (posn-point (event-start e)))
	 (overlays (and pt (not (eq pt 'mode-line)) (overlays-at pt)))
	 (ovl nil)
	 (link nil) ;(nth 1 (nth 1 (memq 'w3 props))))
	 (form nil) ; (nth 1 (memq 'w3form props)))
	 (imag nil) ; (nth 1 (memq 'w3graphic props))))
	 )
    (while overlays
      (setq ovl (car overlays))
      (cond
       ((nth 1 (overlay-get ovl 'w3))
	(setq link (nth 1 (overlay-get ovl 'w3))))
       ((overlay-get ovl 'w3form)
	(setq form (overlay-get ovl 'w3form)))
       ((overlay-get ovl 'w3graphic)
	(setq imag (overlay-get ovl 'w3graphic))))
      (setq overlays (cdr overlays)))
    (cond
     (link (message "%s" link))
     (form
      (let ((args (nth 0 form)))
	(cond
	 ((string= "SUBMIT" (nth 1 form))
	  (message "Submit form to %s" (cdr-safe (assoc "action" args))))
	 ((string= "RESET" (nth 1 form))
	  (message "Reset form contents"))
	 (t
	  (message "Form entry (name=%s, type=%s)" (nth 2 form)
		   (if (equal "" (nth 1 form))
		       "text"
		     (downcase (nth 1 form))))))))
     (imag (message "Inlined image (%s)" (car imag)))
     (t (message "")))))

(defun w3-zone-data (zone)
  "Return the data from a zone"
  (let ((link (overlay-get zone 'w3))
	(form (overlay-get zone 'w3form))
	(grph (overlay-get zone 'w3graphic))
	(list (overlay-get zone 'w3expandlist)))
    (cond
     (link (cons 'w3 link))
     (form (cons 'w3form form))
     (grph (cons 'w3graphic grph))
     (list (cons 'w3expandlist list))
     (t nil))))
  
(defun w3-zone-at (pt)
  "Return the extent at point PT that is either a link or a forms area."
  (let ((ovls (overlays-at pt)) cur link form grph list)
    (while ovls
      (setq cur (car ovls)
	    ovls (cdr ovls)
	    link (or link (and (overlay-get cur 'w3) cur))
	    link (and link (nth 1 (overlay-get link 'w3)) link)
	    form (or form (and (overlay-get cur 'w3form) cur))
	    grph (or grph (and (overlay-get cur 'w3graphic) cur))
	    list (or list (and (overlay-get cur 'w3expandlist) cur))))
    (cond
     (link link)
     (form form)
     (grph grph)
     (list list)
     (t nil))))
  
(defun w3-only-links ()
  "Get all the zones from a buffer"
  (let ((ovls (overlay-lists)) cur result)
    (setq ovls (nconc (car ovls) (cdr ovls)))
    (while ovls
      (setq cur (car ovls) ovls (cdr ovls))
      (if (overlay-get cur 'w3) (setq result (cons cur result))))
    (nreverse result)))
  
(defun w3-all-forms-zones ()
  "Get all the zones from a buffer."
  (let ((ovls (overlay-lists)) cur result)
    (setq ovls (nconc (car ovls) (cdr ovls)))
    (while ovls
      (setq cur (car ovls) ovls (cdr ovls))
      (if (overlay-get cur 'w3form) (setq result (cons cur result))))
    (nreverse result)))
  
(defun w3-all-zones ()
  "Get all the zones from a buffer."
  (let ((ovls (overlay-lists)) cur result)
    (setq ovls (nconc (car ovls) (cdr ovls)))
    (while ovls
      (setq cur (car ovls) ovls (cdr ovls))
      (if (or (overlay-get cur 'w3) (overlay-get cur 'w3form))
	  (setq result (cons cur result))))
    (nreverse result)))
  
(defun w3-find-specific-link (link)
  "Find LINK in the current document"
  (let ((ovls (overlay-lists))
	cur found)
    (setq ovls (nconc (car ovls) (cdr ovls)))
    (while (and (not found) ovls)
      (setq cur (car ovls) ovls (cdr ovls))
      (if (equal link (overlay-get cur 'w3-ident))
	  (setq found (or (goto-char (overlay-start cur)) t))))
    (if found
	(let ((face (overlay-get cur 'face)))
	  (overlay-put cur 'face 'modeline)
	  (setq found nil)
	  (while (not (input-pending-p))
	    (sit-for 1))
	  (overlay-put cur 'face face))
      (error "Destination link #%s not found." link))))    

(defun w3-back-link (p)
  "Go back 1 link.  With prefix argument, go that many links."
  (interactive "p")
  (setq p (or p 1))
  (if (< p 0)
      (w3-forward-link (- p))
    (let ((x (w3-zone-at (point))))
      (and x (goto-char (w3-zone-start x))))
    (let ((ovls (overlay-lists)) tmp cur)
      (setq ovls (nconc (car ovls) (cdr ovls))
	    ovls (sort ovls
		       (function (lambda (x y)
				   (< (overlay-start x) (overlay-start y))))))
      (while (and ovls (< (overlay-start (car ovls)) (point)))
	(if (or (and (overlay-get (car ovls) 'w3)
		     (nth 1 (overlay-get (car ovls) 'w3)))
		(overlay-get (car ovls) 'w3form))
	    (setq tmp (cons (car ovls) tmp)))
	(setq ovls (cdr ovls)))
      (cond
       ((nth (1- p) tmp)
	(goto-char (overlay-start (nth (1- p) tmp)))
	(skip-chars-forward " \t\n")
	(cond
	 ((eq w3-echo-link 'url) (w3-view-this-url))
	 ((and (eq w3-echo-link 'text))
	  (message "%s" (buffer-substring (overlay-start (nth (1- p) tmp))
					  (overlay-end (nth (1- p) tmp)))))
	 (t nil)))
       (t
	(error "No more links."))))))
  
(defun w3-overlays-at (pt)
  ;; Return a list of just the overlays containing links/forms/images
  ;; in them at position PT
  (let ((done nil)
	(ovls (overlays-at pt))
	(rslt nil)
	(dat nil))
    (while ovls
      (setq dat (overlay-properties (car ovls)))
      (if (or (and (memq 'w3 dat)
		   (nth 2 (cdr (memq 'w3 dat))))
	      (memq 'w3form dat))
	  (setq rslt (cons (car ovls) rslt)))
      (setq ovls (cdr ovls)))
    (nreverse rslt)))

(defun w3-forward-link (p)
  "Go forward 1 link.  With prefix argument, go that many links."
  (interactive "p")
  (setq p (or p 1))
  (if (< p 0)
      (w3-back-link (- p))
    (if (/= p 1) (w3-forward-link (1- p)))
    (cond
     ((= (point-max) (next-overlay-change (point)))
      (error "No more links."))
     (t
      (let ((save-pos (point))
	    (x (next-overlay-change (point)))
	    (y (point-max)))
	(if (w3-overlays-at (point))
	    (progn
	      (goto-char (overlay-end (car (w3-overlays-at (point)))))
	      (setq x (point))))
	(while (and (not (w3-overlays-at x)) (/= x y))
	  (setq x (next-overlay-change x)))
	(if (= x y)
	    (progn
	      (goto-char save-pos)
	      (error "No more links."))
	  (goto-char x)
	  (cond
	   ((eq w3-echo-link 'url) (w3-view-this-url))
	   ((and (eq w3-echo-link 'text)
		 (setq x (w3-overlays-at x)))
	    (message "%s" (buffer-substring (overlay-start (car x))
					    (overlay-end (car x)))))
	   (t nil))))))))

(defun w3-zone-hidden-p (start end)
  "Return t iff the region from start to end is invisible."
  (let ((x (overlays-at (1+ start))) y)
    (while (and x (not y))
      (if (overlay-get (car x) 'invisible)
	  (setq y t))
      (setq x (cdr x)))
    y))
  
(defun w3-unhide-zone (start end)
  "Make a region from START TO END visible. (emacs19)"
  (let ((x (overlays-at (1+ start))))
    (while x
      (if (overlay-get (car x) 'invisible)
	  (overlay-put (car x) 'invisible nil))
      (setq x (cdr x)))))

(defun w3-hide-zone (start end)
  "Make a region from START to END invisible. (emacs19)"
  (overlay-put (make-overlay start end) 'invisible t))

(defun w3-extend-zone (zone new-end)
  (let ((beg (overlay-start zone)))
    (move-overlay zone beg new-end)))

(defun w3-add-zone (start end style data &optional highlight)
  "Add highlighting (emacs19)"
  (let ((ovl (make-overlay start end)))
    (overlay-put ovl 'face style)
    (overlay-put ovl (car data) (cdr data))
    (overlay-put ovl 'rear-nonsticky t)
    (overlay-put ovl 'front-sticky nil)
    (cond
     ((not (eq (device-type) 'tty)) nil) ; Don't hack faces in non-tty mode
     ((not w3-emacs19-hack-faces-p) nil) ; Don't hack faces
     ((memq style '(w3-node-style w3-visited-node-style b i))
      (goto-char end) (insert 4)
      (goto-char start) (insert 3))
     ((memq style '(h1 h2 h3 h4 h5 h6 u))
      (goto-char end) (insert 2)
      (goto-char start) (insert 1)))
    (if (and (eq (car data) 'w3) (nth 1 data))
	(overlay-put ovl 'w3-ident (nth 1 data)))
    (cond
     ((and (eq (car data) 'w3) highlight)
      (overlay-put ovl 'mouse-face 'highlight))
     ((eq (car data) 'w3form)
      (overlay-put ovl 'mouse-face 'region))
     ((eq (car data) 'w3graphic)
      (overlay-put ovl 'mouse-face 'secondary-selection))
     )
    ovl))

(defun w3-follow-inlined-image ()
  "Follow an inlined image, regardless of whether it is a hyperlink or not."
  (interactive)
  (let ((ovls (overlays-at (point))) done)
    (while (and ovls (not done))
      (if (not (overlay-get (car ovls) 'w3graphic))
	  nil
	(url-maybe-relative (nth 0 (overlay-get (car ovls) 'w3graphic)))
	(setq done t))
      (setq ovls (cdr ovls)))
    (if (not done) (error "No inlined image at point."))))


(provide 'w3-emacs19)
(provide 'w3-e19)
