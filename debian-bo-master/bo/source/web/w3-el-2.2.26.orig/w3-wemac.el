;;; w3-wemac.el,v --- WinEmacs specific functions for emacs-w3
;; Author: wmperry
;; Created: 1995/06/26 02:29:33
;; Version: 1.22
;; Keywords: faces, help, hypermedia, mouse

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
;;; Enhancements For Lucid Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar w3-main-menu 
  '("WWW"
    ["Open Local File" w3-open-local t]
    ["Open URL" w3-fetch t]
    ["Show document's address" url-view-url t]
    ["Copy document's address into cut buffer" w3-save-url t]
    "---"
    ["View Source" w3-source-document t]
    ["Edit Document Source" w3-find-this-file t]
    ["Reload Current Document" w3-reload-document t]
    "---"
    ("Mail document..."
     ["HTML Source" (w3-mail-current-document nil "HTML Source") t]
     ["Formatted Text" (w3-mail-current-document nil "Formatted Text") t]
     ["PostScript" (w3-mail-current-document nil "PostScript") t]
     ["LaTeX Source" (w3-mail-current-document nil "LaTeX Source") t])
    ("Print..."
     ["HTML Source" (w3-print-this-url nil "HTML Source") t]
     ["Formatted Text" (w3-print-this-url nil "Formatted Text") t]
     ["PostScript" (w3-print-this-url nil "PostScript") t]
     ["LaTeX'd" (w3-print-this-url nil "LaTeX'd") t])
    "---"
    ["Add annotation" w3-annotation-add t]
    "---"
    ["Leave & Bury Buffer" w3-leave-buffer t]
    ["Leave & Kill Buffer" w3-quit t]
    )
  "The main w3 menu"
  )

(defvar w3-view-menu
  '("View"
    ["Reload" w3-reload-document t]
    ["Load Delayed Images" w3-load-delayed-images w3-delayed-images]
    ["Load Delayed MPEGs" w3-load-delayed-mpegs w3-delayed-movies]
    ["Refresh" w3-refresh-buffer t]
    ["Document Source" w3-source-document t]
    ["Document Information" w3-document-information t]
    )
  "The top level 'View' menu.")

(defvar w3-help-menu
  (list
   "WWW"
   (vector "About" '(w3-fetch "about:") t)
   (vector "Manual"
	   (list 'w3-fetch (concat w3-documentation-root "docs/w3_toc.html")) t)
   "---"
   (vector (concat "Help on v" w3-version-number)
	   (list 'w3-fetch (concat w3-documentation-root "help_on_" 
				   w3-version-number ".html")) t)
   (vector "On Window" (list 'w3-fetch (concat w3-documentation-root
					       "window-help.html")) t)
   (vector "On FAQ" (list 'w3-fetch (concat w3-documentation-root
					    "FAQ.html")) t)
   "---"
   ["On HTML" (w3-fetch "http://www.ncsa.uiuc.edu/General/Internet/WWW/HTMLPrimer.html") t]
   ["On URLs" (w3-fetch "http://www.ncsa.uiuc.edu/demoweb/url-primer.html") t]
   ["Mail Developer(s)" w3-submit-bug t])
  "The help menu for w3.")

(defconst w3-navigate-menu
  '("Go"
    ["Back" w3-backward-in-history t]
    ["Forward" w3-forward-in-history t]
    "---"
    ["Goto Home Document" w3 t]
    ["Stop Loading" w3-cancel-current-download (get-buffer url-working-buffer)]
    ["Show History" w3-show-history-list url-keep-history]
    ["Show Hotlist" w3-show-hotlist w3-hotlist]
    ("Hotlist Maintenance"
     ["Add this document to hotlist" w3-hotlist-add-document t]
     ["Delete item from hotlist" w3-hotlist-delete t]
     ["Rename item in hotlist" w3-hotlist-rename-entry t]
     ["Append new hotlist file" w3-hotlist-append t])
    "---")
  "The navigation menu.")

(defvar w3-links-menu nil "Menu for w3-mode in lemacs")
(defvar w3-image-type-restriction nil)
(defvar w3-image-size-restriction nil)
(make-variable-buffer-local 'w3-links-menu)

(or (boundp 'emacs-major-version)
    (defconst emacs-major-version
      (progn (or (string-match "^[0-9]+" emacs-version)
		 (error "emacs-version unparsable"))
	     (string-to-int (substring emacs-version
					  (match-beginning 0) (match-end 0))))
      "Major version number of this version of Emacs, as an integer.
Warning, this variable did not exist in emacs versions earlier than:
  FSF Emacs:   19.23
  Lucid Emacs: 19.10"))

(or (boundp 'emacs-minor-version)
    (defconst emacs-minor-version
      (progn (or (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version)
		 (error "emacs-version unparsable"))
	     (string-to-int (substring emacs-version
					  (match-beginning 1) (match-end 1))))
      "Minor version number of this version of Emacs, as an integer.
Warning, this variable did not exist in emacs versions earlier than:
  FSF Emacs:   19.23
  Lucid Emacs: 19.10"))

(cond
 ((= emacs-minor-version 9)
  (defvar w3-options-menu
    '("Options"
      ["Delay Image Load"
       (setq w3-delay-image-loads (not w3-delay-image-loads)) t]
      ["Flush Image Cache" (setq w3-graphics-list nil) w3-graphics-list]
      ["Flush Disk Cache" (url-flush-cache) t]
      ("Hypertext Gopher Mode"
       ["Turn On" (setq url-use-hypertext-gopher t)
	(not url-use-hypertext-gopher)]
       ["Turn Off" (setq url-use-hypertext-gopher nil)
	url-use-hypertext-gopher])
      ("Hypertext Dired Mode"
       ["Turn On" (setq url-use-hypertext-dired t)
	(not url-use-hypertext-dired)]
       ["Turn Off" (setq url-use-hypertext-dired nil) url-use-hypertext-dired]
       )
      ["Clear History" (setq url-history-list nil) url-history-list])
    "The options menu for w3"))
 (t
  (defvar w3-options-menu
  '("Options"
    ["Delay Image Load" (setq w3-delay-image-loads (not w3-delay-image-loads))
     nil]
    ["Flush Image Cache" (setq w3-graphics-list nil) t]
    ["Flush Disk Cache" (url-flush-cache) t]
    ("Hypertext Gopher Mode"
     ["Turn On" (setq url-use-hypertext-gopher t) t]
     ["Turn Off" (setq url-use-hypertext-gopher nil) t])
    ("Hypertext Dired Mode"
     ["Turn On" (setq url-use-hypertext-dired t) t]
     ["Turn Off" (setq url-use-hypertext-dired nil) t])
    ["Clear History" (progn
		       (setq url-history-list nil)
		       (disable-menu-item '("Options" "Clear History"))) t])
  "The options menu for w3")))

(defun w3-create-faces ()
  "Create faces, the lucid way"
  (make-face w3-node-style)
  (make-face w3-visited-node-style)
  (if (not (face-differs-from-default-p w3-node-style))
      (copy-face 'bold w3-node-style))
  (if (not (face-differs-from-default-p w3-visited-node-style))
      (copy-face 'bold-italic w3-visited-node-style)))

(fset 'w3-delete-zone 'delete-extent)
(fset 'w3-zone-end 'extent-end-position)
(fset 'w3-zone-start 'extent-start-position)
(fset 'w3-zone-eq 'eq)
;(fset 'w3-insert 'insert)

(defun w3-insert (&rest args)
  (let ((start (point))
	(zones nil))
    (map-extents (function
		  (lambda (x y)
		    (setq zones (cons x zones))
		    nil)) nil start (if (eobp) start (1+ start)))
    (apply 'insert-before-markers args)
    (mapcar (function
	     (lambda (zone)
	       (cond
		((= (point) (extent-end-position zone)) nil)
		((< (extent-end-position zone) (point))
		 (set-extent-endpoints zone (extent-end-position zone)
				       (point)))
		((= (extent-start-position zone) start)
		 (set-extent-endpoints zone (point)
				       (extent-end-position zone))))))
	    zones)))

(defun w3-zone-hidden-p (start end)
  "Return t iff the region from start to end is invisible."
  (and (extent-at (1+ start))
       (extent-property (extent-at (1+ start)) 'invisible)))

(defun w3-unhide-zone (start end)
  "Make a region from START TO END visible. (lemacs)"
  (map-extents
   (function
    (lambda (ext)
      (if (and (= start (extent-start-position ext))
	       (= end   (extent-end-position ext))
	       (extent-property ext 'invisible))
	  (progn (delete-extent ext) t)
	nil))) start end))

(defun w3-hide-zone (start end)
  "Make a region from START to END invisible. (lemacs)"
  (set-extent-property (make-extent start end) 'invisible t))

(defun w3-fix-extent-endpoints ()
  "Make sure no extents contain trailing whitespace/newlines"
  (let ((skip-chars (list ?\t ?\r ?\n ?\ )))
    (map-extents (function
		  (lambda (ext maparg)
		    (if (or (and (fboundp 'annotationp)
				 (annotationp ext))
			    (memq (car (extent-data ext))
				  '(w3graphic w3delayed))
			    ) nil
		      (let ((st (extent-start-position ext))
			    (nd (extent-end-position ext))
			    (ch nil))
			(while (memq (char-after (1- nd)) skip-chars)
			  (setq nd (1- nd)
				ch t))
			(while (memq (char-after st) skip-chars)
			  (setq st (1+ st)
				ch t))
			(if ch
			    (if (<= nd st)
				(delete-extent ext)
			      (set-extent-endpoints ext st nd)))))
		    nil)))))

(defun w3-all-zones ()
  (let ((cur (next-extent (current-buffer)))
	(all nil))
    (while cur
      (setq all (cons cur all))
      (setq cur (next-extent cur)))
    all))

(defun w3-add-hotlist-menu ()
  (if (eq major-mode 'w3-mode)
      (let ((hot-menu nil)
	    (hot w3-hotlist))
	(while hot
	  (setq hot-menu (cons (vector
				(w3-truncate-menu-item (car (car hot)))
				(list 'w3-fetch (car (cdr (car hot))))
				t) hot-menu)
		hot (cdr hot)))
	(if (cdr w3-links-menu)
	    (add-submenu '("Go") (cons "Links" (w3-breakup-menu
						(cdr w3-links-menu)
						w3-max-menu-length)))
	  (condition-case ()
	      (delete-menu-item '("Go" "Links"))
	    (error nil)))
	(if hot-menu (add-submenu '("Go")
				  (cons "Hotlist"
					(w3-breakup-menu hot-menu
							 w3-max-menu-length)))
	  (condition-case ()
	      (delete-menu-item '("Go" "Hotlist")))))))

(defun w3-find-specific-link (link)
  "Find LINK in the current document"
  (let ((dat (map-extents
	      (function
	       (lambda (ext maparg)
		 (if (and (stringp (nth 1 (extent-data ext)))
			  (string= (nth 1 (extent-data ext)) link))
		     (cons ext (extent-start-position ext))
		   nil))))))
    (cond
     (dat
      (goto-char (cdr dat))
      (message "Found link %s" link)
      (force-highlight-extent (car dat) t)
      (while (not (input-pending-p))
	(sit-for 1))
      (force-highlight-extent (car dat) nil)))))     

(defun w3-zone-data (zone)
  "Return the data associated with zone"
  (let ((link (extent-data zone)))
    (if (memq (car link) '(w3 w3graphic w3form w3expandlist w3mpeg w3delayed))
	link
      nil)))

(defun w3-zone-at (pt)
  "Return the extent at point PT that is either a link or a forms area."
  (let* ((ext  (extent-at pt (current-buffer)))
	 (dat  (and ext (extent-data ext))))
    (cond
     ((null dat) nil)
     ((memq (car dat) '(w3 w3form w3delayed)) ext)
     (t nil))))

(defun w3-mouse-handler (e)
  "Function to message the url under the mouse cursor"
  (let* ((pt (event-point e))
	 (ext (and pt (extent-at pt)))
	 (dat (and ext (extent-data ext))))
    (cond
     ((null dat) (message ""))
     ((eq (car dat) 'w3)     (message "%s" (nth 1 (cdr dat))))
     ((eq (car dat) 'w3form)
      (let ((args (nth 0 (nth 1 dat)))
	    (form (cdr dat)))
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
     (t (message "")))))

(defun w3-next-extent (xt)
  "Return the next extent after XT that is a link or a forms area."
  (let ((x nil))
    (map-extents (function
		  (lambda (extent maparg)
		    (if (memq (car (extent-data extent)) '(w3 w3form))
			(setq x extent) nil)))
		 (current-buffer)
		 (if xt (1+ (extent-end-position xt)) (point))
		 (point-max))
    x))

(defun w3-forward-link (p)
  "Move forward to the next link in the document.  Error if no more links."
  (interactive "P")
  (setq p (or p 1))
  (if (< p 0)
      (w3-back-link (- p))
    (if (/= 1 p)
	(w3-forward-link (1- p)))
    (let* ((extent (extent-at (point)))
	   (data (and extent (extent-data extent)))
	   (x (w3-next-extent (if (memq (car-safe data) '(w3 w3form))
				  extent))))
      (if x (goto-char (extent-start-position x))
	(error "No more links.")))))

(defun w3-previous-extent (xt)
  (let ((x nil))
    (map-extents (function (lambda (extent maparg)
			     (if (memq (car (extent-data extent)) '(w3 w3form))
				 (setq x extent)) nil))
		 (current-buffer) (point-min)
		 (if xt (extent-start-position xt) (point)))
    x))

(defun w3-back-link (p)
  "Go back link"
  (interactive "P")
  (setq p (or p 1))
  (if (< p 0)
      (w3-forward-link (- p))
    (if (/= 1 p)
	(w3-back-link (1- p)))
    (let ((x (w3-previous-extent (extent-at (point)))))
      (if x (goto-char (extent-start-position x))
	(error "No previous link.")))))

(defun w3-extend-zone (zone new-end)
  (let ((beg (extent-start-position zone)))
    (set-extent-endpoints zone beg new-end)))

(defun w3-add-zone (start end style data &optional highlight)
  "Add highlighting (lucid)"
  (let ((ext))
    (if (markerp start)
	(setq ext (make-extent (marker-position start) (marker-position end)))
      (setq ext (make-extent start end)))
    (if style     (set-extent-face ext style))
    (if highlight (set-extent-attribute ext 'highlight))
    (set-extent-data ext data)
    (if (eq (car data) 'w3) (set-extent-priority ext 2))
    ext))

(defun w3-follow-mouse (e)
  (interactive "e")
  (mouse-set-point e)
  (w3-follow-link))

(define-key w3-mode-map 'button2 'w3-follow-mouse)
(define-key w3-mode-map '(control button2) 'w3-follow-inlined-image-mouse)
(define-key w3-mode-map 'button3 'w3-popup-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to build menus of urls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-toplevel-menu-exists-p (name)
  "Search for a top level menu called NAME.  Return non-nil iff it exists"
  (assoc name current-menubar))

(defun w3-build-lemacs-menu ()
  "Build lemacs menus from w3-links-list"
  (let* ((hot w3-hotlist)
	 (hot-menu nil))
    (or current-menubar
	(set-menubar default-menubar))
    (map-extents 'w3-build-links-helper)
    (setq w3-links-menu (cons "Links" w3-links-menu))
    (while hot
      (setq hot-menu
	    (cons (vector (car (car hot))
			  (list 'url-maybe-relative (car (cdr (car hot))))
			  t) hot-menu))
      (setq hot (cdr hot)))
    (setq hot-menu (cons "Hotlist" hot-menu))
    (set-buffer-menubar (copy-sequence current-menubar))
    (add-submenu '("Help") (cons "WWW" (cdr w3-help-menu)))
    (add-submenu nil (cons "WWW" (cdr w3-main-menu)))
    (add-submenu nil (cons "View" (cdr w3-view-menu)))
    (add-submenu nil (cons "Go" (cdr w3-navigate-menu)))
    (if (cdr hot-menu)
	(add-submenu '("Go")
		     (cons "Hotlist"
			   (w3-breakup-menu (cdr hot-menu)
					    w3-max-menu-length))))
    (if (cdr w3-links-menu)
	(add-submenu '("Go")
		     (cons "Links"
			   (w3-breakup-menu (cdr w3-links-menu)
					    w3-max-menu-length))))
    (if (w3-toplevel-menu-exists-p "Options")
	(add-submenu '("Options") (cons "WWW"  (cdr w3-options-menu))
		     "Save Options")
      (add-submenu nil (cons "Options" (cdr w3-options-menu))))))

(defun w3-build-links-helper (extent maparg)
  "Build a list of links using map-extents for lucid"
  (let ((x (if (eq (extent-data extent) 'w3) (extent-data extent))))
    (if (and x (not (null (nth 1 x))))
	(setq w3-links-menu
	      (nconc w3-links-menu
		     (list
		      (vector (w3-truncate-menu-item
			       (w3-fix-spaces (nth 2 x)))
			      (list 'url-maybe-relative (nth 1 x)) t)))))
    nil))

(defun w3-popup-menu (e)
  "Pop up a menu of common w3 commands"
  (interactive "e")
  (mouse-set-point e)
  (popup-menu w3-popup-menu))
      
(defun w3-x-popup-menu (pos menudesc)
  "If last command was a mouse command use a popup-menu, otherwise do a
completing read"
  (if (or (button-press-event-p last-command-event)
	  (button-release-event-p last-command-event)
	  (misc-user-event-p last-command-event))
      (w3-x-really-popup-menu pos menudesc)
    (completing-read "Please choose: " (cdr (cdr (car (cdr menudesc))))
		     nil t)))

(defun w3-x-really-popup-menu (pos menudesc)
  "My hacked up function to do a blocking popup menu..."
  (let ((echo-keystrokes 0)
	event menu)
    (setq menudesc (cdr (car (cdr menudesc)))) ; remove the title
    (while menudesc
      (setq menu (cons (vector (car (car menudesc))
			       (list (car (car menudesc))) t) menu)
	    menudesc (cdr menudesc)))
    (setq menu (cons "WWW" menu))
    (popup-menu menu)
    (catch 'popup-done
      (while t
	(setq event (next-command-event event))
	(cond ((and (misc-user-event-p event) (stringp (car-safe
						   (event-object event))))
	       (throw 'popup-done (event-object event)))
	      ((and (misc-user-event-p event)
		    (or (eq (event-object event) 'abort)
			(eq (event-object event) 'menu-no-selection-hook)))
	       (signal 'quit nil))
	      ((button-release-event-p event);; don't beep twice
	       nil)
	      (t
	       (beep)
	       (message "please make a choice from the menu.")))))))

(defun w3-setup-version-specifics ()
  "Set up routine for WinEmacs"
  (setq w3-temporary-directory
	(or (and w3-temporary-directory
		 (stringp w3-temporary-directory)
		 (file-exists-p w3-temporary-directory)
		 w3-temporary-directory)
	    (getenv "TEMP") (getenv "TMP") (getenv "temp") (getenv "tmp")
	    (getenv "EMACSTMP"))
	url-temporary-directory w3-temporary-directory))

(defun w3-store-in-x-clipboard (str)
  "Store string STR into the clipboard in X"
  (x-own-selection str 'PRIMARY)
  (x-selection-owner-p 'PRIMARY))  

(if (not (and (boundp 'emacs-major-version) (>= emacs-major-version 10)))
    (message "Image handling ignored"))

(defun w3-mode-version-specifics ()
  "Lucid emacs specific stuff for w3-mode"
  (w3-build-lemacs-menu)
  (if w3-track-mouse (setq mode-motion-hook 'w3-mouse-handler))
  (add-hook 'activate-menubar-hook 'w3-add-hotlist-menu)
  (setq mode-popup-menu w3-popup-menu))

(defun w3-map-links (function &optional buffer from to maparg)
  "Map FUNCTION over the hypertext links which overlap region in BUFFER,
starting at FROM and ending at TO.  FUNCTION is called with the arguments
linkdata, START, END, and MAPARG.
The arguments FROM, TO, MAPARG, and BUFFER default to the beginning of
BUFFER, the end of BUFFER, nil, and (current-buffer), respectively."
  (map-extents (function (lambda (x y)
			   (if (eq (car-safe (extent-data x)) 'w3)
			       (funcall function (w3-zone-data x)
					(extent-start-position x)
					(extent-end-position x) y))
			   nil)) buffer from to maparg))

(provide 'w3-wemacs)
(provide 'w3-wemac)
