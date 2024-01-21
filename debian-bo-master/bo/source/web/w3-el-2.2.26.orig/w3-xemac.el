;;; w3-xemac.el,v --- XEmacs specific functions for emacs-w3
;; Author: wmperry
;; Created: 1995/03/09 20:35:12
;; Version: 1.27
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
;;; Enhancements For XEmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-allowed-image-types
  (mapcar (function (lambda (x) (list (car x)))) w3-graphic-converter-alist))

(make-variable-buffer-local 'w3-links-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spiffy new toolbar for XEmacs 19.12 only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-links-menu nil "Menu for w3-mode in XEmacs.")
(defvar w3-image-type-restriction nil)
(defvar w3-image-size-restriction nil)
(defvar w3-options-menu nil "The options menu for w3.")

(defun w3-image-cache-timeout-function ()
  (setq w3-graphics-list nil)
  (garbage-collect))

(defun w3-start-image-cache-timer ()
  (interactive)
  (require 'itimer)
  (let ((timer (get-itimer "w3-image-flush")))
    (if timer (delete-itimer timer))
    (start-itimer "w3-image-flush" 'w3-image-cache-timeout-function
		  300 300)))
		  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spiffy new menus for XEmacs 19.12 only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-hotlist-menu-constructor (menu-items)
  (let ((hot-menu nil)
	(hot w3-hotlist))
    (while hot
      (setq hot-menu (cons (vector
			    (w3-truncate-menu-item (car (car hot)))
			    (list 'w3-fetch (car (cdr (car hot))))
			    t) hot-menu)
	    hot (cdr hot)))
    (or hot-menu '(["No Hotlist" undefined nil]))))

(defun w3-image-type-constructor (menu-items)
  (let ((nodes menu-items) cur)
    (if (not nodes)
	(setq menu-items
	      (mapcar
	       (function
		(lambda (data)
		  (let ((typ (car data)))
		    (vector typ
			    (list 'w3-ins-or-del-graphic typ)
			    ':style 'toggle
			    ':selected
			    (list 'assoc typ 'w3-allowed-image-types)))))
	       w3-graphic-converter-alist))
      )
    menu-items))

(defun w3-image-quality-constructor (menu-items)
  (let ((nodes menu-items)
	(cur nil))
    (while nodes
      (setq cur (car nodes)
	    nodes (cdr nodes))
      (if (not (vectorp cur))
	  nil
	(cond
	 ((string-match "Use " (aref cur 0))
	  (aset cur 0 (format "Use %dx%dx%dx colormap" w3-color-max-red
			       w3-color-max-green w3-color-max-blue)))
	 ((string-match "Dither to" (aref cur 0))
	  (aset cur 0 (format "Dither to %d colors"
			      (* w3-color-max-red w3-color-max-green
				 w3-color-max-blue))))
	 (t nil))))
    menu-items))

(defun w3-build-links-helper (extent maparg)
  (let ((x (extent-property extent 'w3)))
    (if (and x (not (null (nth 1 x))))
	(setq w3-links-menu
	      (nconc w3-links-menu
		     (list
		      (vector (w3-truncate-menu-item
			       (w3-fix-spaces
				(buffer-substring
				 (extent-start-position extent)
				 (extent-end-position extent))))
			      (list 'url-maybe-relative (nth 1 x))
			      t))))))
  nil)

(defun w3-links-menu-constructor (menu-items)
  (or menu-items
      (progn
	(map-extents 'w3-build-links-helper)
	(setq w3-links-menu (w3-breakup-menu w3-links-menu
					     w3-max-menu-length))
	(or w3-links-menu '(["No Links" undefined nil])))))

(defun w3-menu-xemacs-global-menubar ()
  (save-excursion
    (set-buffer (get-buffer-create "*scratch*"))
    current-menubar))

(defvar w3-menu
  (list
   '("File"
     :filter file-menu-filter
     ["Open URL..." w3-fetch t]
     ["Open File..." w3-open-local t]
     ["Open in New Frame..." w3-fetch-other-frame t]
     "---"
     ["Save" save-buffer t nil]
     ("Save As..."
      ["HTML" (w3-save-as "HTML Source") t]
      ["Formatted Text" (w3-save-as "Formatted Text") t]
      ["LaTeX" (w3-save-as "LaTeX Source") t]
      ["PostScript" (w3-save-as "PostScript") t]
      ["Binary" (w3-save-as "Binary") t])
     "---"
     ["New Frame"		make-frame		t]
     ["Delete Frame"		delete-frame		t]
     "---"
     ("Print As..."
      ["PostScript" (w3-print-this-url nil "PostScript") t]
      ["Formatted Text" (w3-print-this-url nil "Formatted Text") t]
      ["HTML Source" (w3-print-this-url nil "HTML Source") t]
      ["LaTeX'd" (w3-print-this-url nil "LaTeX'd") t])
     ("Mail Document..."
      ["HTML" (w3-mail-current-document nil "HTML Source") t]
      ["Formatted Text" (w3-mail-current-document nil "Formatted Text") t]
      ["PostScript" (w3-mail-current-document nil "PostScript") t]
      ["LaTeX Source" (w3-mail-current-document nil "LaTeX Source") t])
     ["Add Annotation" w3-annotation-add w3-personal-annotation-directory]
     "---"
     ["Leave Buffer" w3-leave-buffer t]
     ["Kill Buffer" w3-quit t nil]
     "---:shadowDoubleEtchedIn"
     ["Exit XEmacs" save-buffers-kill-emacs t]
     )
   '("Edit"
     :filter edit-menu-filter
     ["Undo"			advertised-undo		   t]
     ["Cut"			x-kill-primary-selection   t]
     ["Copy"			x-copy-primary-selection   t]
     ["Paste"			x-yank-clipboard-selection t]
     ["Clear"			x-delete-primary-selection t]
     "----"
     ["Search..."		isearch-forward		t]
     ["Search Backward..."	isearch-backward	t]
     ["Replace..."		query-replace		t]
     "----"
     ["Search (Regexp)..."	isearch-forward-regexp	t]
     ["Search Backward (Regexp)..." isearch-backward-regexp t]
     ["Replace (Regexp)..."	query-replace-regexp	t]
     "----"
     ["Goto Line..."		goto-line		t]
     ["What Line"		what-line		t]
     "----"
     ["Start Macro Recording"	start-kbd-macro	      (not defining-kbd-macro)]
     ["End Macro Recording"	end-kbd-macro		defining-kbd-macro]
     ["Execute Last Macro"	call-last-kbd-macro	last-kbd-macro]
     )
   '("View"
     ["Document Information" w3-document-information t]
     ["Document Source" w3-source-document t]
     ["Load Images" w3-load-delayed-images w3-delayed-images]
     "----"
     ["Refresh" w3-refresh-buffer w3-current-parse]
     ["Reload" w3-reload-document (and (url-view-url t)
				       (not (equal (url-view-url t) "")))]
     "----"
     ["Show URL" url-view-url t]
     ["Show URL At Point" w3-view-this-url t])
   '("Go"
     ["Forward" w3-forward-in-history t]
     ["Backward" w3-backward-in-history t]
     ["Home" w3 w3-default-homepage]
     ["View History..." w3-show-history-list url-keep-history]
     "----"
     ("Links" :filter w3-links-menu-constructor))
   '("Hotlist"
     ["View Hotlist..." w3-show-hotlist w3-hotlist]
     ["Add this document to hotlist" w3-hotlist-add-document t]
     ["Delete item from hotlist" w3-hotlist-delete t]
     ["Rename item in hotlist" w3-hotlist-rename-entry t]
     ["Append new hotlist file" w3-hotlist-append t]
     "----"
     ("Hotlist" :filter w3-hotlist-menu-constructor))
   '("Options"
     ["Show Toolbar" w3-toggle-toolbar
      :style toggle :selected (w3-toolbar-active)]
     ["Auto Load Images" (setq w3-delay-image-loads (not w3-delay-image-loads))
      :style toggle :selected (not w3-delay-image-loads)]
     ["Auto Load MPEGs" (setq w3-delay-mpeg-loads (not w3-delay-mpeg-loads))
      :style toggle :selected (not w3-delay-mpeg-loads)]
     "----"
     ("Image Quality"
      :filter w3-image-quality-constructor
      ["Never dither" (setq w3-color-use-reducing nil)
       :style radio :selected (null w3-color-use-reducing)]
      ["Use " (setq w3-color-filter 'ppmquant
		    w3-color-use-reducing t)
       :style radio :selected (and w3-color-use-reducing
				   (eq w3-color-filter 'ppmquant))]
      ["Dither to " (setq w3-color-filter 'ppmdither
			  w3-color-use-reducing t)
       :style radio :selected (and w3-color-use-reducing
				   (eq w3-color-filter 'ppmdither))]
      ["Other..." (setq w3-color-filter
			(read-string "Filter: "
				     (if (stringp w3-color-filter)
					 w3-color-filter ""))
			w3-color-use-reducing t)
       :style radio :selected (and w3-color-use-reducing
				   (stringp w3-color-filter))])
     ("Image Types" :filter w3-image-type-constructor)
     ["Flush Image Cache" (setq w3-graphics-list nil) w3-graphics-list]
     "----"
     ["Privacy Mode" (progn
		       (setq url-privacy-level
			     (if (eq 'paranoid url-privacy-level)
				 'none
			       'paranoid))
		       (url-setup-privacy-info))
      :style toggle :selected (not (eq url-privacy-level 'none))]
     ["Color Printing" (setq ps-print-color-p (not ps-print-color-p))
      :style toggle :selected (and (boundp 'ps-print-color-p)
				   ps-print-color-p)]
     ["Allow Document Stylesheets" (setq w3-honor-stylesheets
					 (not w3-honor-stylesheets))
      :style toggle :selected w3-honor-stylesheets]
     ["Honor Automatic Refreshes" (setq url-honor-refresh-requests
					(not url-honor-refresh-requests))
      :style toggle :selected (not (null url-honor-refresh-requests))]
     ["Honor Color Requests" (setq w3-user-colors-take-precedence
				   (not w3-user-colors-take-precedence))
      :style toggle :selected (not w3-user-colors-take-precedence)]
     "----"
     ["Download to disk" (setq w3-dump-to-disk (not w3-dump-to-disk))
      :style toggle :selected w3-dump-to-disk]
     ["Caching" (setq url-automatic-caching (not url-automatic-caching))
      :style toggle :selected url-automatic-caching]
     ["Use Cache Only" (setq url-standalone-mode (not url-standalone-mode))
      :style toggle :selected url-standalone-mode]
     "----"
     ["Fancy Gopher" (setq url-use-hypertext-gopher
			       (not url-use-hypertext-gopher))
      :style toggle :selected url-use-hypertext-gopher]
     ["Fancy Directory Listings" (setq url-use-hypertext-dired
				       (not url-use-hypertext-dired))
      :style toggle :selected url-use-hypertext-dired]
     "----"
     ["Save Options" w3-menu-save-options t])
   '("Buffers"
     :filter buffers-menu-filter
     ["List All Buffers" list-buffers t]
     "--!here")
   ["Emacs" w3-menu-toggle-menubar t]
   nil
   '("Help"
     ["About Emacs-w3" (w3-fetch "about:") t]
     ["Manual" (w3-fetch (concat w3-documentation-root "docs/w3_toc.html")) t]
     "---"
     ["Version Information..."
      (w3-fetch (concat w3-documentation-root "help_on_" 
			w3-version-number ".html")) t]
     ["On Window" (w3-fetch (concat w3-documentation-root
				    "window-help.html")) t]
     ["On FAQ" (w3-fetch (concat w3-documentation-root
				 "FAQ.html")) t]
     "---"
     ["On HTML" (w3-fetch "http://www.ncsa.uiuc.edu/General/Internet/WWW/HTMLPrimer.html") t]
     ["On URLs" (w3-fetch "http://www.ncsa.uiuc.edu/demoweb/url-primer.html") t]
     ["Mail Developer(s)" w3-submit-bug t])))

(defun w3-menu-toggle-menubar ()
  (interactive)
  (if (null (car (find-menu-item current-menubar '("Emacs"))))
      (set-buffer-menubar w3-menu)
    (set-buffer-menubar (copy-sequence (w3-menu-xemacs-global-menubar)))
    (add-menu-button nil ["W3" w3-menu-toggle-menubar t] nil)))

(defun w3-menu-save-options ()
  (interactive)
  (let ((output-buffer (find-file-noselect
			(expand-file-name
			 (concat "~" init-file-user "/.emacs"))))
	output-marker)
    (save-excursion
      (set-buffer output-buffer)
      ;;
      ;; Find and delete the previously saved data, and position to write.
      ;;
      (goto-char (point-min))
      (if (re-search-forward "^;; W3 Options Settings *\n" nil 'move)
	  (let ((p (match-beginning 0)))
	    (goto-char p)
	    (or (re-search-forward
		 "^;; End of W3 Options Settings *\\(\n\\|\\'\\)"
		 nil t)
		(error "can't find END of saved state in .emacs"))
	    (delete-region p (match-end 0)))
	(goto-char (point-max))
	(insert "\n"))
      (setq output-marker (point-marker))
      (let ((print-readably t)
	    (print-escape-newlines t)
	    (standard-output output-marker))
	(princ ";; W3 Options Settings\n")
	(princ ";; ===================\n")
	(mapcar (function
		 (lambda (var)
		   (princ "  ")
		   (if (and (symbolp var) (boundp var))
		       (prin1 (list 'setq-default var
				    (let ((val (symbol-value var)))
				      (if (or (memq val '(t nil))
					      (and (not (symbolp val))
						   (not (listp val))))
					  val
					(list 'quote val))))))
		   (if var (princ "\n"))))
		'(
		  w3-delay-image-loads
		  w3-delay-mpeg-loads
		  ps-print-color-p
		  w3-color-use-reducing
		  w3-color-filter
		  w3-dump-to-disk
		  url-automatic-caching
		  url-standalone-mode
		  url-use-hypertext-gopher
		  url-use-hypertext-dired
		  url-proxy-services
		  url-be-asynchronous
		  w3-default-homepage
		  url-privacy-level
		  w3-toolbar-orientation
		  )
		)
	(princ ";; ==========================\n")
	(princ ";; End of W3 Options Settings\n")))
    (set-marker output-marker nil)
    (save-excursion
      (set-buffer output-buffer)
      (save-buffer))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Make the menu acceptable to old versions of Lucid Emacs/XEmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-downgrade-menus ()
  "Strip out the XEmacs 19.12'isms from the w3 menu"
  (require 'pp)
  (let ((need-to-replace nil))
    (save-excursion
      (set-buffer (get-buffer-create " *w3-temp*"))
      (erase-buffer)
      (pp w3-menu (current-buffer))
      (goto-char (point-min))
      (if (search-forward ":filter" nil t)
	  (setq need-to-replace t))
      (goto-char (point-min))
      (delete-matching-lines ":filter")
      (goto-char (point-min))
      (w3-replace-regexp "---:shadowDoubleEtchedIn" "----")
      (goto-char (point-min))
      (if (search-forward "Show Toolbar" nil t)
	  (progn
	    (beginning-of-line)
	    (kill-sexp 1)))
      (goto-char (point-min))
      (if (search-forward "View History..." nil t)
	  (progn
	    (beginning-of-line)
	    (forward-sexp 2)
	    (end-of-line)
	    (insert ")")))
      (if (search-forward "Append new hotlist file" nil t)
	  (progn
	    (beginning-of-line)
	    (forward-sexp 2)
	    (end-of-line)
	    (insert ")")))
      (if (and need-to-replace
	       (search-forward "Never dither" nil t))
	  (progn
	    (beginning-of-line)
	    (insert "(\"Image Quality\"\n")))
      (goto-char (point-min))
      (if (and need-to-replace
	       (search-forward "Exit XEmacs" nil t))
	  (progn
	    (end-of-line)
	    (insert "\n(\"Edit\"\n")))
      (goto-char (point-min))
      (if (and need-to-replace
	       (search-forward "Save Options" nil t))
	  (progn
	    (end-of-line)
	    (insert "\n(\"Buffers\"\n")))
      (goto-char (point-min))
      (if (and (= emacs-minor-version 6)
	       (search-forward "Options" nil t))
	  (progn
	    (beginning-of-line)
	    (kill-sexp 1)
	    (insert
	     "(\"Options\"
	    [\"Delay Image Load\" (setq w3-delay-image-loads (not w3-delay-image-loads))
	     nil]
	    [\"Flush Image Cache\" (setq w3-graphics-list nil) t]
	    [\"Flush Disk Cache\" (url-flush-cache) t]
	    (\"Hypertext Gopher Mode\"
	     [\"Turn On\" (setq url-use-hypertext-gopher t) t]
	     [\"Turn Off\" (setq url-use-hypertext-gopher nil) t])
	    (\"Hypertext Dired Mode\"
	     [\"Turn On\" (setq url-use-hypertext-dired t) t]
	     [\"Turn Off\" (setq url-use-hypertext-dired nil) t])
	    [\"Clear History\" (progn
                               (url-clrhash url-history-list)
			       (disable-menu-item '(\"Options\" \"Clear History\"))) t])")
	    (goto-char (point-min))))
      (and need-to-replace (insert "((\"File\"\n"))
      (goto-char (point-min))
      (if (not need-to-replace)
	  (w3-replace-regexp "\"----\"))" "\"----\")"))
      (goto-char (point-min))
      (setq w3-menu (read (current-buffer)))
      (kill-buffer (current-buffer)))))


(defun w3-ins-or-del-graphic (typ)
  (if (assoc typ w3-allowed-image-types)
      (setq w3-allowed-image-types
	    (mapcar (function (lambda (x) (if (equal typ (car x)) nil x)))
		    w3-allowed-image-types))
    (setq w3-allowed-image-types (cons (list typ) w3-allowed-image-types))))

(defun w3-create-faces ()
  "Create faces, the XEmacs way"
  
  (make-face w3-node-style)
  (make-face w3-default-style)
  (make-face w3-visited-node-style)
  (make-face w3-active-node-style)
  
  (if (not (face-differs-from-default-p w3-node-style))
      (copy-face 'bold w3-node-style))
  (if (not (face-differs-from-default-p w3-visited-node-style))
      (copy-face 'bold-italic w3-visited-node-style))
  (if (not (face-differs-from-default-p w3-active-node-style))
      (copy-face 'bold w3-active-node-style)))
 
(fset 'w3-delete-zone 'delete-extent)
(fset 'w3-zone-end 'extent-end-position)
(fset 'w3-zone-start 'extent-start-position)
(fset 'w3-zone-eq 'eq)

(if (< emacs-minor-version 12)
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
  (fset 'w3-insert 'insert-before-markers))

(defun w3-zone-hidden-p (start end)
  "Return t iff the region from start to end is invisible."
  (and (extent-at (1+ start))
       (extent-property (extent-at (1+ start)) 'invisible)))

(defun w3-unhide-zone (start end)
  "Make a region from START TO END visible. (xemacs)"
  (map-extents
   (function
    (lambda (ext)
      (if (and (= start (extent-start-position ext))
	       (= end   (extent-end-position ext))
	       (extent-property ext 'invisible))
	  (progn (delete-extent ext) t)
	nil))) start end))

(defun w3-hide-zone (start end)
  "Make a region from START to END invisible. (xemacs)"
  (set-extent-property (make-extent start end) 'invisible t))

(defun w3-fix-extent-endpoints ()
  "Make sure no extents contain trailing whitespace/newlines"
  ;; Using char-after instead of skip-chars-backward means we don't have
  ;; to actually move point to do this.
  (let ((skip-chars (list ?\t ?\r ?\n ?\ )))
    (map-extents (function
		  (lambda (ext maparg)
		    (if (or (and (fboundp 'annotationp)
				 (annotationp ext))
			    (extent-property ext 'w3graphic)
			    (extent-property ext 'w3delayed)
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

(defun w3-sensitize-menu ()
  (if (and (eq major-mode 'w3-mode) current-menubar
	   (car (find-menu-item current-menubar '("Emacs"))))
      (let ((hot-menu nil)
	    (hot w3-hotlist)
	    (image (find-menu-item current-menubar
				   '("Options" "Image Quality"))))
	(if (setq image (cdr (car image)))
	    (progn
	      (aset (nth 1 image) 0 (format "Use %d colors"
					    (* w3-color-max-red
					       w3-color-max-green
					       w3-color-max-blue)))
	      (aset (nth 2 image) 0 (format "Dither to %dx%dx%d colormap"
					    w3-color-max-red
					    w3-color-max-green
					    w3-color-max-blue))))
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
	(if hot-menu
	    (add-submenu '("Hotlist") (cons "Hotlist"
				       (w3-breakup-menu hot-menu
							w3-max-menu-length)))
	  (condition-case ()
	      (delete-menu-item '("Hotlist" "Hotlist")))))
    t))

(defun w3-find-specific-link (link)
  "Find LINK in the current document"
  (let ((dat (map-extents
	      (function
	       (lambda (ext maparg)
		 (if (equal link (extent-property ext 'w3-ident))
		     (cons ext (extent-start-position ext))
		   nil))))))
    (if (not dat)
	(error "Destination link #%s not found." link)
      (goto-char (cdr dat))
      (message "Found link %s" link)
      (force-highlight-extent (car dat) t)
      (while (not (input-pending-p))
	(sit-for 1))
      (force-highlight-extent (car dat) nil))))

(defun w3-zone-data (zone)
  "Return the data associated with zone"
  (if (extentp zone)
      (let ((link (extent-property zone 'w3))
	    (grph (extent-property zone 'w3graphic))
	    (form (extent-property zone 'w3form))
	    (list (extent-property zone 'w3expandlist))
	    (mpeg (extent-property zone 'w3mpeg))
	    (dely (extent-property zone 'w3delayed)))
	(cond
	 (link (cons 'w3 link))
	 (form (cons 'w3form form))
	 (dely (cons 'w3delayed dely))
	 (grph (cons 'w3graphic grph))
	 (mpeg (cons 'w3mpeg mpeg))
	 (list (cons 'w3expandlist list))
	 (t nil)))
    zone))

(defun w3-zone-at (pt)
  "Return the extent at point PT that is either a link or a forms area."
  (let ((link (extent-at pt (current-buffer) 'w3))
	(form (extent-at pt (current-buffer) 'w3form))
	(grph (extent-at pt (current-buffer) 'w3graphic))
	(list (extent-at pt (current-buffer) 'w3expandlist))
	(mpeg (extent-at pt (current-buffer) 'w3mpeg))
	(dely (extent-at pt (current-buffer) 'w3delayed)))
    (cond
     (link link)
     (form form)
     (dely dely)
     (grph grph)
     (list list)
     (mpeg mpeg)
     (t nil))))

(defun w3-mouse-handler (e)
  "Function to message the url under the mouse cursor"
  (let* ((pt (event-point e))
	 (props (and pt (extent-properties-at pt)))
	 (link (nth 1 (nth 1 (memq 'w3 props)))) ; The link info if it exists
	 (form (nth 1 (memq 'w3form props))) 	 ; The form info it it exists
	 (dely (nth 0 (nth 1 (memq 'w3delayed props))))	 ; The delayed img info
	 (mpeg (nth 1 (memq 'w3mpeg props)))     ; the delayed mpeg info
	 (imag (nth 1 (memq 'w3graphic props)))) ; The image info if it exists
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
     (dely (message "Delayed image (%s)" (car dely)))
     (imag (message "Inlined image (%s)" (car imag)))
     (mpeg (message "Delayed mpeg (%s)" (car mpeg)))
     (t (message "")))))

(defun w3-next-extent (xt)
  "Return the next extent after XT that is a link or a forms area."
  (let ((x nil))
    (map-extents (function (lambda (extent maparg)
			     (if (or (extent-property extent 'w3)
				     (extent-property extent 'w3form))
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
    (let ((x (w3-next-extent (or (extent-at (point) nil 'w3)
				 (extent-at (point) nil 'w3form)))))
      (if x (goto-char (extent-start-position x))
	(error "No more links.")))))

(defun w3-previous-extent (xt)
  (let ((x nil))
    (map-extents (function (lambda (extent maparg)
			     (if (or (extent-property extent 'w3)
				     (extent-property extent 'w3form))
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
  (if (extent-property zone 'detached)
      (insert-extent zone (point) new-end)
    (let ((beg (extent-start-position zone)))
      (set-extent-endpoints zone beg new-end))))

(defun w3-add-zone (start end style data &optional highlight)
  "Add highlighting (xemacs)"
  (if (markerp start) (setq start (marker-position start)))
  (if (markerp end)   (setq end   (marker-position end)))
  (let ((ext (make-extent start end)))
    (set-extent-property ext 'face style)
    (set-extent-property ext 'detachable nil)
    (set-extent-property ext 'highlight highlight)
    (set-extent-property ext (car data) (cdr data))
    (cond
     ((eq (car data) 'w3)
      (set-extent-property ext 'priority 2)
      (if (nth 1 data) (set-extent-property ext 'w3-ident (nth 1 data)))
      (if (nth 2 data) (set-extent-property ext 'help-echo (nth 2 data))))
     ((eq (car data) 'w3form)
      (let* ((args (nth 1 data))
	     (mesg
	      (cond
	       ((string= "SUBMIT" (nth 2 data))
		(format "Submit form to %s" (cdr-safe (assoc "action" args))))
	       ((string= "RESET" (nth 2 data))
		"Reset form contents")
	       (t
		(format "Form entry (name=%s, type=%s)" (nth 3 data)
			(if (equal "" (nth 2 data))
			    "text"
			  (downcase (nth 2 data))))))))
	(set-extent-property ext 'help-echo mesg))))
    ext))

(defun w3-follow-mouse-other-frame (e)
  "Function suitable to being bound to a mouse key.  Follows the link under
the mouse click, opening it in another frame."
  (interactive "e")
  (mouse-set-point e)
  (w3-follow-link-other-frame))

(defun w3-follow-mouse-down (e)
  (interactive "e")
  (let ((ext (extent-at (event-closest-point e) (event-buffer e) 'w3))
	(st (event-closest-point e))
	(old-face nil)
	(event (allocate-event)))
    (if ext
	(progn
	  (force-highlight-extent ext nil)
	  (setq old-face (extent-face ext))
	  (set-extent-property ext 'highlight nil)
	  (set-extent-face ext w3-active-node-style)
	  (while (and (next-command-event event)
		      (not (button-release-event-p event))
		      (/= 2 (event-button event)))
	    nil)
	  (set-extent-property ext 'face old-face)
	  (set-extent-property ext 'highlight nil)
	  (if (or (< (event-closest-point event) (extent-start-position ext))
		  (> (event-closest-point event) (extent-end-position ext)))
	      nil
	    (w3-follow-mouse event)))
      (w3-follow-mouse e))))

(defun w3-follow-mouse (e)
  (interactive "e")
  (mouse-set-point e)
  (w3-follow-link))

(defun w3-follow-inlined-image-mouse (e)
  "Follow an inlined image from the mouse"
  (interactive "e")
  (mouse-set-point e)
  (w3-follow-inlined-image))

(defun w3-follow-inlined-image ()
  "Follow an inlined image, regardless of whether it is a hyperlink or not."
  (interactive)
  (let ((grph (extent-at (point) (current-buffer) 'w3graphic)))
    (cond
     (grph (url-maybe-relative (nth 0 (extent-property grph 'w3graphic))))
     (t (message "No inlined image at point.")))))

(define-key w3-mode-map 'button2 'w3-follow-mouse-down)
(define-key w3-mode-map 'button3 'w3-popup-menu)
(define-key w3-mode-map '(control button2) 'w3-follow-inlined-image-mouse)
(define-key w3-mode-map '(shift button2) 'w3-follow-mouse-other-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to build menus of urls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-toplevel-menu-exists-p (name)
  "Search for a top level menu called NAME.  Return non-nil iff it exists"
  (assoc name current-menubar))

(defun w3-build-xemacs-menu ()
  "Build xemacs menus from w3-links-list"
  (if current-menubar
      (let* ((hot w3-hotlist)
	     (hot-menu nil))
	(or current-menubar
	    (set-menubar default-menubar))
	(setq w3-links-menu nil)
	(map-extents 'w3-build-links-helper)
	(setq w3-links-menu (cons "Links" w3-links-menu))
	(while hot
	  (setq hot-menu
		(cons (vector (car (car hot))
			      (list 'url-maybe-relative (car (cdr (car hot))))
			      t) hot-menu))
	  (setq hot (cdr hot)))
	(setq hot-menu (cons "Hotlist" hot-menu))
	(set-buffer-menubar (copy-tree w3-menu t))
	(if (cdr hot-menu)
	    (add-submenu '("Hotlist")
			 (cons "Hotlist"
			       (w3-breakup-menu (cdr hot-menu)
						w3-max-menu-length))))
	(if (cdr w3-links-menu)
	    (add-submenu '("Go")
			 (cons "Links"
			       (w3-breakup-menu (cdr w3-links-menu)
						w3-max-menu-length)))))))

(defun w3-popup-menu (e)
  "Pop up a menu of common w3 commands"
  (interactive "e")
  (mouse-set-point e)
  (let* ((ext (w3-zone-at (point)))
	 (dat (and ext (w3-zone-data ext)))
	 url)
    (if (event-glyph-extent e)
	(setq ext (event-glyph-extent e)
	      dat (and ext (extent-property ext 'w3graphic))
	      dat (and dat (list 'w3graphic dat))))
    (cond
     ((eq (car dat) 'w3)		; hyperlink
      (setq url (nth 2 dat))
      (popup-menu (cons "Hyperlink"
			(mapcar
			 (function
			  (lambda (x) (vector (car x) (list (cdr x) url) t)))
			 w3-hyperlink-menu))))
     ((or (eq (car dat) 'w3graphic)
	  (eq (car dat) 'w3delayed))
      (setq url (if (listp (nth 1 dat))
		    (car (nth 1 dat))
		  (nth 1 dat)))
      (popup-menu (cons "Image"
			(mapcar
			 (function
			  (lambda (x) (vector (car x) (list (cdr x) url) t)))
			 w3-graphlink-menu))))
     (t (popup-menu w3-popup-menu)))))

(defun w3-x-popup-dialog (pos descr)
  "My hacked up function to do a blocking popup menu..."
  (let ((echo-keystrokes 0)
	event dialog)
    (setq dialog (cons (car descr) dialog)
	  descr (cdr descr))
    (while descr
      (setq dialog (nconc dialog
			  (list (vector (car descr)
					(list (car descr)) t)))
	    descr (cdr descr)))
    (popup-dialog-box dialog)
    (catch 'dialog-done
      (while t
	(setq event (next-command-event event))
	(cond
	 ((and (misc-user-event-p event)
	       (stringp (car-safe (event-object event))))
	  (throw 'dialog-done (car-safe (event-object event))))
	 ((and (misc-user-event-p event)
	       (or (eq (event-object event) 'abort)
		   (eq (event-object event) 'menu-no-selection-hook)))
	  (signal 'quit nil))
	 ((button-release-event-p event) nil)
	 (t
	  (beep)
	  (message "Please make a choice from the dialog")))))))
      
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
	      ((not (popup-menu-up-p))
	       (throw 'popup-done nil))
	      ((button-release-event-p event);; don't beep twice
	       nil)
	      (t
	       (beep)
	       (message "please make a choice from the menu.")))))))

(defun w3-setup-version-specifics ()
  "Set up routine for XEmacs 19.12 or later"
  (cond
   ((>= emacs-minor-version 12)
    (w3-toolbar-make-buttons))
   ((>= emacs-minor-version 10)
    (w3-downgrade-menus)
    (fset 'w3-insert 'insert))
   (t
    ;; Really old version - this will get filled in when
    ;; the WinEmacs stuff is merged in here.
    ))

  ;; Add our menus, but make sure that we do it to the global menubar
  ;; not the current one, which could be anything, but usually GNUS or
  ;; VM if not the default.
  (let ((current-menubar (default-value 'current-menubar)))
    (if current-menubar
	(progn
	  (add-submenu '("Options") (cons "WWW" (cdr
						 (assoc "Options" w3-menu)))
		       "Save Options")
	  (add-submenu '("Help") (cons "WWW" (cdr (assoc "Help" w3-menu)))))))

  ;; Add the local etc directory to the icon search path
  (if (boundp 'data-directory)
      (let ((maybe-dir (file-name-as-directory
			(expand-file-name "w3" data-directory))))
	(if (file-directory-p maybe-dir)
	    (setq w3-icon-directory-list (cons (concat "file:" maybe-dir)
					       w3-icon-directory-list)))))
  )

(defun w3-store-in-x-clipboard (str)
  "Store string STR into the clipboard in X"
  (if (or (<= emacs-minor-version 11)
	  (not (eq (device-type) 'tty)))
      (progn
	(x-own-selection str 'PRIMARY)
	(x-selection-owner-p 'PRIMARY))
    (message "No cut buffer on a tty!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graphics functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun w3-maybe-load-images ()
  (let ((tmp w3-delayed-images)		; All images
	(ldd nil)			; Loaded ones
	(rest nil)			;
	(buffer-read-only nil)
	)
    (or (fboundp 'w3-insert-graphic)
	(error "Cannot do images..."))
    (while tmp
      (if (assoc (car (car (car tmp))) w3-graphics-list)
	  (progn
	    (apply 'w3-insert-graphic (car tmp))
	    (setq ldd (cons (car (car (car tmp))) ldd)
		  w3-graphics-list (delq (car tmp) w3-graphics-list))))
      (setq tmp (cdr tmp)))
    (map-extents
     (function
      (lambda (ext maparg)
	(if (extent-property ext 'w3delayed)
	    (setq rest (cons ext rest)))
	nil)))
    (while rest
      (if (member (car (car (extent-property (car rest) 'w3delayed))) ldd)
	  (progn
	    (delete-region (extent-start-position (car rest))
			   (extent-end-position (car rest)))
	    (delete-extent (car rest))))
      (setq rest (cdr rest)))))

(defun w3-load-delayed-images ()
  "Load inlined images that were delayed, if necessary."
  (interactive)
  (if (eq (device-type) 'tty)
      nil
    (let ((buffer-read-only nil) rest)
      (map-extents
       (function
	(lambda (ext maparg)
	  (if (extent-property ext 'w3delayed)
	      (setq rest (cons ext rest)))
	  nil)))
      (while rest
	(delete-region (extent-start-position (car rest))
		       (extent-end-position (car rest)))
	(delete-extent (car rest))
	(setq rest (cdr rest)))
      (mapcar (function
	       (lambda (data)
		 (save-excursion
		   (apply 'w3-insert-graphic data))))
	      w3-delayed-images)
      (setq w3-delayed-images nil))
    (set-buffer-modified-p nil)))

(defun w3-load-delayed-mpegs ()
  "Load all delayed mpeg movies for this buffer"
  (interactive)
  (let ((buffer-read-only nil) rest)
    (map-extents
     (function
      (lambda (ext maparg)
	(if (extent-property ext 'w3mpeg)
	    (setq rest (cons ext rest)))
	nil)))
    (while rest
      (delete-region (extent-start-position (car rest))
		     (extent-end-position (car rest)))
      (delete-extent (car rest))
      (setq rest (cdr rest)))
    (mapcar (function (lambda (data)
			(apply 'w3-insert-mpeg data)))
	    w3-delayed-movies)
    (setq w3-delayed-movies nil)))

(defun w3-insert-mpeg (src pt &optional width height)
  "Insert an mpeg file SRC at point PT"
  (let* ((ext (make-extent pt pt))
	 (win (make-x-window-glyph (or width w3-mpeg-size)
				   (or height w3-mpeg-size)))
	 (fname (url-generate-unique-filename "%s.mpg"))
	 (w3-mpeg-args (append w3-mpeg-args
			       (list "-window" (int-to-string
						(x-window-glyph-xid win))
				     fname)))
	 (url-working-buffer (url-generate-new-buffer-name " *embed*")))
    (save-excursion
      (set-buffer (get-buffer-create url-working-buffer))
      (setq url-be-asynchronous nil)
      (url-retrieve src)
      (write-region (point-min) (point-max) fname nil 5)
      (kill-buffer (current-buffer)))
    (set-extent-begin-glyph ext win)
    (set-extent-property ext 'w3-mpeg
			 (cons (apply 'start-process src nil
				      w3-mpeg-program w3-mpeg-args)
			       win))))

(defun w3-mpeg-kill-processes (&optional buffer)
  "Kill all mpeg_play processes associated with this buffer"
  (interactive)
  (map-extents
   (function
    (lambda (ext maparg)
      (let ((data (extent-property ext 'w3-mpeg)))
	(if (not data)
	    nil
	  (delete-process (car data))
	  (delete-extent ext)
	  nil))))))	    

(defun w3-load-single-delayed-mpeg (st nd src pt)
  "Load a single delayed mpeg"
  (let ((buffer-read-only nil))
    (delete-region st nd)
    (w3-insert-mpeg src st)))

(defun w3-load-single-delayed-graphic (st nd src pt align alt)
  "Load a single delayed image."
  (let ((buffer-read-only nil))
    (delete-region st nd)
    (w3-insert-graphic src pt align alt)))  

(defvar w3-mode-xemacs-data-map (make-sparse-keymap))
(defvar w3-mode-xemacs-event-map (make-sparse-keymap))
(set-keymap-name w3-mode-xemacs-data-map 'annotation-local-map)
(set-keymap-name w3-mode-xemacs-event-map 'annotation-local-map)

(cond
 ((fboundp 'glyph-width) (fset 'w3-pixmap-width 'glyph-width))
 ((fboundp 'pixmap-width) (fset 'w3-pixmap-width 'pixmap-width))
 (t (fset 'w3-pixmap-width 'identity)))

(define-key w3-mode-xemacs-data-map
  'button2 'annotation-activate-function-default)
(define-key w3-mode-xemacs-event-map
  'button2 'annotation-activate-function-with-event)
(define-key w3-mode-xemacs-data-map 'button3 'w3-popup-menu)
(define-key w3-mode-xemacs-event-map 'button3 'w3-popup-menu)

(defun w3-right-spaces (glyph)
  "Return the number of spaces to insert in order to right-justify
the given glyph (may be a string or a pixmap).
Assume spaces are as wide as avg-pixwidth.  
Won't be quite right for proportional fonts, but it's the best we can do."
  (let* ((avg-pixwidth     (round (/ (frame-pixel-width) (frame-width))))
	 (fill-area-width  (* avg-pixwidth (- fill-column left-margin)))
	 (glyph-pixwidth   (cond ((stringp glyph) 
				  (* avg-pixwidth (length glyph)))
				 ((glyphp glyph)
				  (glyph-width glyph))
				 (t
				  (error "startup-center-spaces: bad arg")))))
    (+ left-margin
       (round (/ (- fill-area-width glyph-pixwidth) avg-pixwidth)))))
  
(defun w3-center-spaces (glyph)
  "Return the number of spaces to insert in order to center
the given glyph (may be a string or a pixmap).
Assume spaces are as wide as avg-pixwidth.  
Won't be quite right for proportional fonts, but it's the best we can do."
  (let* ((avg-pixwidth     (round (/ (frame-pixel-width) (frame-width))))
	 (fill-area-width  (* avg-pixwidth (- fill-column left-margin)))
	 (glyph-pixwidth   (cond ((stringp glyph) 
				  (* avg-pixwidth (length glyph)))
				 ((glyphp glyph)
				  (glyph-width glyph))
				 (t
				  (error "startup-center-spaces: bad arg")))))
    (+ left-margin
       (round (/ (/ (- fill-area-width glyph-pixwidth) 2) avg-pixwidth)))))

(defun w3-make-pixmap (fname alt)
  (make-glyph (list (cons 'x fname)
		    (cons 'tty alt))))

(defun w3-insert-graphic (name pt align alt &optional force)
  "Insert the graphic pointed to by the URL NAME, at buffer position POINT,
with alignment specified by ALIGN (one of 'center 'top or 'bottom).  If the
conversion of the picture fails for any reason, use ALT as the alternative
text.  If the reading of the pixmap is successful, the url and a pointer to
the pixmap are stored in w3-graphics-list for possible re-use later."
  (let ((bit nil)
	(add-to-list nil)
	(buffer-read-only nil)
	(url-request-method "GET")
	(url-be-asynchronous nil)
	(url-request-data nil)
	(url-request-extra-headers nil)
	(url-source t)
	(url-mime-accept-string nil)
	(err nil)
	(lnk (cdr name))
	(fname (url-generate-unique-filename)))
    (setq name (car name)
	  url-mime-accept-string
	  (substring
	   (mapconcat
	    (function
	     (lambda (x)
	       (if x (concat (car x) ",") ""))) w3-allowed-image-types "")
	   0 -1))
    (setq pt (max pt (point-min))
	  pt (min pt (point-max)))
    (save-excursion
      (let ((w3-working-buffer " *W3GRAPH*")
	    (url-working-buffer " *W3GRAPH*")
	    (attribs (or (assoc name w3-graphics-list)
			 (url-file-attributes name))))
	(set-buffer (get-buffer-create url-working-buffer))
	(setq url-be-asynchronous nil)
	(cond
	 ((assoc name w3-graphics-list)
	  (message "Reusing image...")
	  (setq bit (cdr (assoc name w3-graphics-list))))
	 ((and (not force)
	       (not (assoc (nth 8 attribs) w3-allowed-image-types)))
	  (url-lazy-message "Skipping image %s [%s]" 
			    (url-basepath name t) (nth 8 attribs))
	  (let ((anno (make-annotation alt pt 'text)))
	    (set-extent-property anno 'w3graphic name)
	    (set-annotation-data anno
				 (list (cons name lnk) pt align alt t))
	    (set-extent-property anno 'keymap w3-mode-xemacs-data-map)
	    (set-extent-property anno 'help-echo (cond
						  ((listp lnk) (car lnk))
						  ((stringp lnk) lnk)
						  (t nil)))
	    (set-annotation-action anno 'w3-annotation-action-2)))
	 ((and (not force)
	       (numberp w3-image-size-restriction)
	       (> 0 (nth 7 attribs))
	       (> (nth 7 attribs) w3-image-size-restriction))
	  (url-lazy-message "Skipping image %s [%s bytes]" 
			    (url-basepath name t) (nth 7 attribs))
	  (let ((anno (make-annotation alt pt 'text)))
	    (set-extent-property anno 'w3graphic name)
	    (set-extent-property anno 'detachable nil)
	    (set-annotation-data anno
				 (list (cons name lnk) pt align alt t))
	    (set-extent-property anno 'help-echo (cond
						  ((listp lnk) (car lnk))
						  ((stringp lnk) lnk)
						  (t nil)))
	    (set-extent-property anno 'keymap w3-mode-xemacs-data-map)
	    (set-annotation-action anno 'w3-annotation-action-2)))
	 (t
	  (setq add-to-list t
		err t)
	  (url-retrieve name)
	  (url-uncompress)
	  (w3-convert-graphic-to-useable-format url-working-buffer
						fname
						(not (featurep 'xpm)))
	  (message "Reading image %s..." url-current-file)
	  (if (equal 0 (nth 7 (file-attributes fname)))
	      (save-excursion
		(set-buffer url-working-buffer)
		(let ((x (buffer-string)))
		  (w3-warn 'image
			   (concat "Reading of image " name " failed!\n"
				   x))))
	    (condition-case ()
		(setq bit (w3-make-pixmap fname alt))
	      (error (save-excursion
		       (set-buffer url-working-buffer)
		       (let ((x (buffer-string)))
			 (w3-warn 'image
				  (concat "Reading of image " name " failed!\n"
					  x)))))))
	  (condition-case ()
	      (delete-file fname)
	    (error nil))))))
    (and add-to-list
	 (setq w3-graphics-list (cons (cons name bit) w3-graphics-list)))
    (cond 
     (bit
      (if (fboundp 'set-glyph-baseline)
	  (set-glyph-baseline bit (cond
				   ((eq align 'top) 0)
				   ((memq align '(center middle)) 50)
				   ((eq align 'bottom) 100)
				   (t 50))))
      (if (= (or (char-after pt) 0) ?\t) (setq pt (max 1 (1- pt))))
      (if (>= (w3-pixmap-width bit) (/ (frame-pixel-width) 2))
	  (save-excursion
	    (goto-char pt)
	    (insert "\n\n")
	    (setq pt (1+ pt))))
      (let ((anno (make-annotation bit pt 'text nil t)))
	(set-extent-property anno 'w3graphic name)
	(set-extent-property anno 'keymap w3-mode-xemacs-data-map)
	(set-annotation-data anno lnk)
	(set-extent-property anno 'help-echo (cond
					      ((listp lnk) (car lnk))
					      ((stringp lnk) lnk)
					      (t nil)))
	(set-extent-property anno 'keymap w3-mode-xemacs-event-map)
	(set-annotation-action anno 'w3-annotation-action-3)))
     (err
      (let ((anno (make-annotation alt pt 'text)))
	(set-extent-property anno 'w3graphic name)
	(set-extent-property anno 'keymap w3-mode-xemacs-data-map)
	(set-annotation-data anno (cons name lnk))
	(set-extent-property anno 'help-echo (cond
					      ((listp lnk) (car lnk))
					      ((stringp lnk) lnk)
					      (t nil)))
	(set-extent-property anno 'keymap w3-mode-xemacs-data-map)
	(set-annotation-action anno 'w3-annotation-action-1)))
     (t nil))
    bit))

(defun w3-annotation-action-1 (data)
  "Annotation function that passes a failed image off to an external viewer"
  (w3-fetch (car data)))

(defun w3-annotation-action-2 (data)
  "Annotation function that tries to load 1 delayed image."
  (set-buffer (extent-buffer extent))
  (delete-annotation extent)
  (apply 'w3-insert-graphic data))

(defun w3-annotation-action-3 (data event)
  "Annotation function that tries send off an imagemap click"
  (let* ((url (car data))
	 (x (and (fboundp 'event-glyph-x-pixel) (event-glyph-x-pixel event)))
	 (y (and (fboundp 'event-glyph-y-pixel) (event-glyph-y-pixel event)))
	 )
    (cond
     ((and (eq (cdr data) 'ismap) (stringp url))
      (if (and x y)
	  (w3-fetch (concat url "?" (int-to-string x) "," (int-to-string y)))
	(error "Imagemaps not implemented in this version of emacs.")))
     ((stringp url) (w3-fetch url))
     (t nil))))

(defun w3-mode-version-specifics ()
  "XEmacs specific stuff for w3-mode"
  (if current-menubar
      (if (not (fboundp 'set-specifier))
	  (progn
	    (w3-build-xemacs-menu)
	    (let ((formats
		   (cons "Image Types"
			 (mapcar
			  (function
			   (lambda (data)
			     (let ((typ (car data)))
			       (vector typ
				       (list 'w3-ins-or-del-graphic typ)
				       ':style 'toggle
				       ':selected
				       (list 'assoc typ
					     'w3-allowed-image-types)))))
			  w3-graphic-converter-alist))))
	      (add-hook 'activate-menubar-hook 'w3-sensitize-menu)
	      (add-submenu '("Options") formats "Flush Image Cache")))
	(set-buffer-menubar w3-menu)))
  (cond
   ((not w3-track-mouse)
    nil)
   ((or (not (boundp 'inhibit-help-echo))
	inhibit-help-echo)
    (setq mode-motion-hook 'w3-mouse-handler))
   (t nil))
  (if (eq (device-type) 'tty)
      nil
    (w3-add-toolbar-to-buffer))
  (setq mode-popup-menu w3-popup-menu))

(defun w3-map-links (function &optional buffer from to maparg)
  "Map FUNCTION over the hypertext links which overlap region in BUFFER,
starting at FROM and ending at TO.  FUNCTION is called with the arguments
linkdata, START, END, and MAPARG.
The arguments FROM, TO, MAPARG, and BUFFER default to the beginning of
BUFFER, the end of BUFFER, nil, and (current-buffer), respectively."
  (map-extents (function (lambda (x y)
			   (if (extent-property x 'w3)
			       (funcall function (w3-zone-data x)
					(extent-start-position x)
					(extent-end-position x)
					y))
			   nil)) buffer from to maparg))

(require 'w3-toolbar)
(provide 'w3-xemacs)
(provide 'w3-xemac)
