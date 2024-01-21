;;; w3-style.el,v --- Emacs-W3 binding style sheet mechanism
;; Author: wmperry
;; Created: 1995/10/27 01:15:43
;; Version: 1.52
;; Keywords: faces, hypermedia

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
;;; A style sheet mechanism for emacs-w3
;;;
;;; This will eventually be able to under DSSSL[-lite] as well as the
;;; experimental W3C mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'font)

(defvar w3-style-syntax-table
  (copy-syntax-table mm-parse-args-syntax-table)
  "The syntax table for parsing stylesheets")
(modify-syntax-entry ?/ "\"" w3-style-syntax-table)

(defun w3-parse-dssl-lite (fname &optional string)
  (let ((dest-buf (current-buffer))
	(url-mime-accept-string
	 "Accept: application/stylesheet ; notation=dsssl-lite")
	(sheet nil))
    (save-excursion
      (set-buffer (get-buffer-create
		   (url-generate-new-buffer-name " *style*")))
      (erase-buffer)
      (if fname (url-insert-file-contents fname))
      (goto-char (point-max))
      (if string (insert string))
      (goto-char (point-min))
      (delete-matching-lines "^[ \t]*#") ; Nuke comments
      (delete-matching-lines "^[ \t\r]*$") ; Nuke blank lines
      (goto-char (point-min))
      (insert "(")
      (goto-char (point-max))
      (insert ")")
      (goto-char (point-min))
      (setq sheet (condition-case ()
		      (read (current-buffer))
		    (error nil)))
      ;; Now need to convert the DSSSL-lite flow objects
      ;; into our internal representation
      ;; WORK WORK WORK!
      )))

(defun w3-style-parse-args (st &optional nd defines)
  ;; Return an assoc list of attribute/value pairs from a CSS v4
  (let (
	name				; From name=
	value				; its value
	results				; Assoc list of results
	name-pos			; Start of XXXX= position
	val-pos				; Start of value position
	math				; what math was done
	)
    (save-excursion
      (if (stringp st)
	  (progn
	    (set-buffer (get-buffer-create " *w3-style-temp*"))
	    (set-syntax-table w3-style-syntax-table)
	    (erase-buffer)
	    (insert st)
	    (setq st (point-min)
		  nd (point-max)))
	(set-syntax-table w3-style-syntax-table))
      (save-restriction
	(narrow-to-region st nd)
	(goto-char (point-min))
	(while (not (eobp))
	  (skip-chars-forward ";, \n\t")
	  (setq name-pos (point))
	  (skip-chars-forward "^ \n\t:=,;")
	  (downcase-region name-pos (point))
	  (setq name (buffer-substring name-pos (point)))
	  (skip-chars-forward " \t\n")
	  (if (not (memq (char-after (point)) '(?= ?:))) ; There is no value
	      (setq value nil)
	    (if (= (or (char-after (point)) 0) ?=)
		(skip-chars-forward " \t\n=")
	      (skip-chars-forward " \t\n:"))
	    (setq val-pos (point)
		  value
		  (cond
		   ((or (= (or (char-after val-pos) 0) ?\")
			(= (or (char-after val-pos) 0) ?'))
		    (buffer-substring (1+ val-pos)
				      (condition-case ()
					  (prog2
					      (forward-sexp 1)
					      (1- (point))
					    (skip-chars-forward "\""))
					(error
					 (skip-chars-forward "^ \t\n")
					 (point)))))
		   (t
		    (buffer-substring val-pos
				      (progn
					(skip-chars-forward "^,;")
					(skip-chars-backward " \t")
					(point)))))))
	  (setq results (cons (cons name value) results))
	  (skip-chars-forward ";, \n\t"))
	results))))

(defvar w3-stylehseet-define-table nil)
(defun w3-stylesheet-handle-define ()
  (let ((name nil)
	(save-pos (point))
	(retval nil))
    (skip-chars-forward "^ \t\r\n") ; Past the name token
    (downcase-region save-pos (point))
    (setq name (buffer-substring save-pos (point)))
    (skip-chars-forward "= \t\r")
    (setq save-pos (point))
    (skip-chars-forward "^;")
    (setq retval (cons name (buffer-substring save-pos (point))))
    (skip-chars-forward " \t\r\n")))

(defun w3-stylesheet-handle-import ()
  (let ((url nil)
	(save-pos (point)))
    (if (looking-at "'\"")
	(condition-case ()
	    (forward-sexp 1)
	  (error (skip-chars-forward "^ \t\r\n;")))
      (skip-chars-forward "^ \t\r\n;"))
    (setq url (url-expand-file-name (buffer-substring save-pos (point))))
    (skip-chars-forward "\"; \t\r\n")
    (setq save-pos (point))
    (let ((url-working-buffer (url-generate-new-buffer-name " *styleimport*"))
	  (url-mime-accept-string
	   "application/stylesheet ; notation=css; level=4")
	  (sheet nil))
      (save-excursion
	(set-buffer (get-buffer-create url-working-buffer))
	(setq url-be-asynchronous nil)
	(url-retrieve url)
	(w3-stylesheet-clean-arena-sheet)
	(setq sheet (buffer-string))
	(kill-buffer (current-buffer)))
      (insert sheet)
      (goto-char save-pos))))

(defun w3-stylesheet-clean-arena-sheet ()
  (goto-char (point-min))
  (delete-matching-lines "^[ \t]*#")	; Nuke comments
  (delete-matching-lines "^[ \t\r]*$")	; Nuke blank lines
  (w3-replace-regexp "--.*$" "")	; Nuke new style comments
  (w3-replace-regexp "^[ \t\r]+" "")	; Nuke whitespace at beg. of line
  (w3-replace-regexp "[ \t\r]+$" "")	; Nuke whitespace at end of line
  (w3-replace-regexp "![ \t]*\\([^ \t\r\n]+\\)\\(.*\\)"
		     "; priority=\"\\1\" reason=\\2")
  (goto-char (point-min)))

(defun w3-stylesheet-applies-to (st nd)
  ;; The stupid way to do things - pre context sensitive stuff
  ;; (delete "" (split-string (buffer-substring st nd "[ \t\r\n,&]")))
  (let ((results nil)
	(save-pos nil))
    (narrow-to-region st nd)
    (goto-char st)
    (skip-chars-forward " \t\r\n")
    (while (not (eobp))
      (setq save-pos (point))
      (skip-chars-forward "^,")
      (skip-chars-backward " \r\t\n")
      (setq results (cons (buffer-substring save-pos (point)) results))
      (skip-chars-forward ", \t\r\n"))
    (widen)
    results))

(defun w3-parse-arena-style-sheet (fname &optional string)
  (let ((dest-buf (current-buffer))
	(url-mime-accept-string
	 "application/stylesheet ; notation=css; level=4")
	(save-pos nil)
	(applies-to nil)		; List of tags to apply style to
	(attrs nil)			; List of name/value pairs
	(tag nil)
	(att nil)
	(val nil)
	(class nil)
	(defines nil)
	(css-v4 t)
	(sheet nil))
    (save-excursion
      (set-buffer (get-buffer-create
		   (url-generate-new-buffer-name " *style*")))
      (set-syntax-table w3-style-syntax-table)
      (erase-buffer)
      (if fname (url-insert-file-contents fname))
      (goto-char (point-max))
      (if string (insert string))
      (w3-stylesheet-clean-arena-sheet)
      (while (not (eobp))
	(beginning-of-line)
	(setq save-pos (point))
	(if (looking-at "[ \t\r]*@\\([^ \t\r\n]\\)")
	    (let ((directive nil))
	      (skip-chars-forward " @\t\r") ; Past any leading whitespace
	      (setq save-pos (point))
	      (skip-chars-forward "^ \t\r\n") ; Past the @ directive
	      (downcase-region save-pos (point))
	      (setq directive (buffer-substring save-pos (point)))
	      (skip-chars-forward " \t\r") ; Past any trailing whitespace
	      (setq save-pos (point))
	      (cond
	       ((string= directive "define")
		(let ((retval (w3-stylesheet-handle-define)))
		  (and defines
		       (setq defines (cons retval defines)))))
	       ((string= directive "import")
		(w3-stylesheet-handle-import))
	       (t
		(w3-warn 'style (format "Unknown directive: @%s" directive)
			 'warning))))
	  (skip-chars-forward "^{:")
	  (downcase-region save-pos (point))
	  (setq applies-to (w3-stylesheet-applies-to save-pos (point))
		css-v4 (= (char-after (point)) ?{ ))
	  (if css-v4
	      (skip-chars-forward "^{")
	    (skip-chars-forward " \t:"))
	  (setq save-pos (point))
	  (if css-v4
	      (progn
		(forward-sexp 1)
		(end-of-line)
		(skip-chars-backward "\r}"))
	    (end-of-line)
	    (skip-chars-backward "\r"))
	  (subst-char-in-region save-pos (point) ?\n ? )
	  (subst-char-in-region save-pos (point) ?\r ? )
	  (setq attrs (w3-style-parse-args (if css-v4 (1+ save-pos) save-pos)
					   (point) defines))
	  (skip-chars-forward "}\r\n")
	  (while applies-to
	    (if (string-match "\\(.*\\)\\.\\(.*\\)" (car applies-to))
		;; We have a tag.class match
		(setq tag (intern (downcase (match-string 1 (car applies-to))))
		      class (match-string 2 (car applies-to)))
	      (setq tag (intern (downcase (car applies-to)))
		    class 'internal))
	    (setq applies-to (cdr applies-to))
	    (let ((loop attrs))
	      (while loop
		(if (stringp (car (car loop)))
		    (setcar (car loop) (intern (car (car loop)))))
		(setq att (car (car loop))
		      val (cdr (car loop))
		      loop (cdr loop))
		(cond
		 ((eq 'align att)
		  (setq val (intern val)))
		 ((memq att '(indent left-margin right-margin
				     top-margin bottom-margin))
		  (setq val (string-to-int val)))
		 (t nil))
		(let* ((node-1 (assq tag sheet))
		       (node-2 (and node-1 (assoc class node-1)))
		       (node-3 (and node-2 (assq att node-2))))
		  (cond
		   ((not node-1)	; New top-level element
		    (setq sheet (cons (cons tag (list (cons class
							    (list
							     (cons att val)))))
				      sheet)))
		   ((and node-1 (not node-2)) ; New class for existing element
		    (setcdr node-1 (cons (cons class (list (cons att val)))
					 (cdr node-1))))
		   ((and node-2 (not node-3)) ; attribute/value on old class
		    (setcdr node-2 (cons (cons att val) (cdr node-2))))
		   (node-3		; Replace existing attribute value
		    (setcdr node-3 val)))))))))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))
    (cons sheet defines)))


(defvar w3-style-font-size-mappings
  '(("xx-small" . 0)
    ("x-small"  . 1)
    ("small"    . 2)
    ("medium"   . 3)
    ("large"    . 4)
    ("x-large"  . 5)
    ("xx-large" . 6)
    )
  "A list of font size mappings")

(defvar w3-style-font-weight-mappings
  '(("-3" . :extra-light)
    ("-2" . :light)
    ("-1" . :demi-light)
    ("0"  . :medium)
    ("1"  . :normal)
    ("2"  . :demi-bold)
    ("3"  . :bold)
    ("4"  . :extrabold)
    ("demi-light" . :demi-light)
    ("demi-bold"  . :demi-bold)
    ("extra-bold" . :extra-bold)
    ("extra-light". :extra-light)
    )
  "A list of font weight mappings.")

(defun w3-style-font-size-for-index (index)
  (if (stringp index)
      (setq index (or
		   (cdr-safe (assoc (downcase index)
				    w3-style-font-size-mappings))
		      3)))
  (setq index (- index 3))
  (let ((scaler (if (> index 0)
		    (/ 12.0 10)		; Scale by 1.2 to get bigger
		  (/ 10 12.0)))		; Scale by .8333 to get smaller
	(size (font-spacial-to-canonical "12pt"))
	(remainder 0))
    (setq index (abs index))
    (while (/= index 0)
      (setq size (* size scaler)
	    index (1- index)))
    ;; This rounds to the nearest '10'
    (* 10 (round (/ size 10)))))

(defun w3-generate-stylesheet-faces (sheet)
  (url-lazy-message "Applying style hints...")
  (let ((todo sheet)
	(cur nil)
	(cur-classes nil)
	(node nil)
	(fore nil)
	(back nil)
	(pixm nil)
	(font nil)
	(family nil)
	(style nil)
	(size nil)
	(index nil)
	(var nil)
	(shorthand nil)
	(weight nil)
	(locale (current-buffer))
	(face-name nil))
    (while todo
      (setq cur (car todo)
	    cur-classes (cdr cur)
	    todo (cdr todo)
	    var (cdr-safe (assoc (car cur) w3-all-faces)))
      (while cur-classes
	(setq node (cdr (car cur-classes))
	      cur (car cur-classes)
	      cur-classes (cdr cur-classes))
	;; Some of the stuff in here gets a little ugly due to supporting
	;; the old stylesheet mechanism as well as the new CSS v4 stuff.
	;; The new style keywords are at the front of the checks, so that
	;; valid stylesheets go through 'node' looking for a keyword as
	;; few times as possible.
	(setq fore (or (cdr-safe (assq 'text-color node))
		       (cdr-safe (assq 'font-color node))
		       (cdr-safe (assq 'color.text node))
		       (cdr-safe (assq 'font.color node)))
	      back (or (cdr-safe (assq 'color.background node))
		       (cdr-safe (assq 'text-background node))
		       (cdr-safe (assq 'font-background node))
		       (cdr-safe (assq 'back.color node))
		       (cdr-safe (assq 'font.background node)))
	      index (cdr-safe (assq 'font-size-index node))
	      size (or (and index (format "%dpx"
					  (w3-style-font-size-for-index
					   index)))
		       (cdr-safe (assq 'font-size node))
		       (cdr-safe (assq 'font.size node)))
	      family (or (cdr-safe (assq 'font-family node))
			 (cdr-safe (assq 'font.family node)))
	      weight (or (cdr-safe (assq 'font-weight node))
			 (cdr-safe (assq 'font.weight node)))
	      weight (or (cdr-safe (assq 'weight
					 w3-style-font-weight-mappings))
			 weight)
	      style (or (cdr-safe (assq 'font-style node))
			(cdr-safe (assq 'font.style node)))
	      shorthand (cdr-safe (assq 'font node)))
	(if shorthand
	    (let ((shorthand (split-string shorthand "[ \t]")))
	      (setq size (or (nth 0 shorthand) size)
		    family (or (nth 1 shorthand) size)
		    weight (or (nth 2 shorthand) weight)
		    weight (or (cdr-safe
				(assoc weight
				       w3-style-font-weight-mappings))
			       weight)
		    style (or (nth 3 shorthand) style))))
	(if style
	    (setq style (mapcar
			 (function
			  (lambda (x)
			    (intern (concat ":" (downcase x)))))
			 (delete "" (split-string style "[ \t&,]")))))
	(if family
	    (setq family (delete "" (split-string family "[ \t]"))))
	(if fore
	    (setq fore (font-normalize-color fore)))
	(if back
	    (setq back (font-normalize-color back)))
	(if (or family weight style size)
	    (setq font (make-font :family family :weight weight
				  :style style :size size)))
	(if font
	    (progn
	      (setq face-name (intern (format "%s/%s" family size)))
	      (if (find-face face-name)
		  nil
		(make-face face-name)
		(set-face-font face-name font))
	      (setcdr cur (cons (cons 'face face-name) (cdr cur)))))
	(if fore
	    (progn
	      (setq face-name (intern (format "fore-%s" fore)))
	      (if (find-face face-name)
		  nil
		(make-face face-name)
		(set-face-foreground face-name fore))
	      (setcdr cur (cons (cons 'foreground face-name) (cdr cur)))))
	(if back
	    (progn
	      (setq face-name (intern (format "back-%s" back)))
	      (if (find-face face-name)
		  nil
		(make-face face-name)
		(set-face-background face-name back))
	      (setcdr cur (cons (cons 'background face-name) (cdr cur)))))
	)
      )
    )
  (url-lazy-message "Applying style hints... done"))

(defun w3-handle-style (&optional args)
  (let ((fname (or (cdr-safe (assq 'href args))
		   (cdr-safe (assq 'src args))
		   (cdr-safe (assq 'uri args))))
	(type (downcase (or (cdr-safe (assq 'notation args))
			    "experimental")))
	(url-working-buffer " *style*")
	(base (cdr-safe (assq 'base args)))
	(stylesheet nil)
	(defines nil)
	(string (cdr-safe (assq 'data args))))
    (if fname (setq fname (url-expand-file-name fname
						(cdr-safe
						 (assoc base w3-base-alist)))))
    (save-excursion
      (set-buffer (get-buffer-create url-working-buffer))
      (erase-buffer)
      (setq url-be-asynchronous nil)
      (cond
       ((member type '("experimental" "arena" "w3c-style" "css"))
	(let ((data (w3-parse-arena-style-sheet fname string)))
	  (setq stylesheet (nth 0 data)
		defines (nth 1 data))))
       ((string= type "dsssl-lite")
	(setq stylesheet (w3-parse-dsssl-lite fname string)))
       (t
	(w3-warn 'html "Unknown stylesheet notation: %s" type))))
    (setq w3-current-stylesheet stylesheet)
    (if (and w3-current-stylesheet (fboundp 'make-face))
	(w3-generate-stylesheet-faces w3-current-stylesheet))))

(provide 'w3-style)
