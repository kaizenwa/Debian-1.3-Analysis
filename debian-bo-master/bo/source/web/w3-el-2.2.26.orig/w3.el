;;; w3.el,v --- Main functions for emacs-w3 on all platforms/versions
;; Author: wmperry
;; Created: 1995/10/28 03:40:36
;; Version: 1.472
;; Keywords: faces, help, comm, news, mail, processes, mouse, hypermedia

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
;;; This is a major mode for browsing documents written in Hypertext Markup ;;;
;;; Language (HTML).  These documents are typicallly part of the World Wide ;;;
;;; Web (WWW), a project to create a global information net in hypertext    ;;;
;;; format.				                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; first start by making sure the load path is properly set.  This code
;;; is mostly taken from calc-2.02b
;;;
;;; this allows you to put the following in your .emacs file, instead of
;;; having to know what the load-path for the w3 files is.
;;;
;;;     (autoload 'w3 "w3/w3" "WWW Browser" t)

;;; If w3 files exist on the load-path, we're all set.
(let ((name (and (fboundp 'w3)
		 (eq (car-safe (symbol-function 'w3)) 'autoload)
		 (nth 1 (symbol-function 'w3))))
      (p load-path))
  (while (and p (not (file-exists-p
		      (expand-file-name "w3-vars.elc" (car p)))))
    (setq p (cdr p)))
  (or p
;;; If w3 is autoloaded using a path name, look there for w3 files.
;;; This works for both relative ("w3/w3.elc") and absolute paths.
      (and name (file-name-directory name)
	   (let ((p2 load-path)
		 (name2 (concat (file-name-directory name)
				"w3-vars.elc")))
	     (while (and p2 (not (file-exists-p
				  (expand-file-name name2 (car p2)))))
	       (setq p2 (cdr p2)))
	     (if p2
		 (setq load-path (nconc load-path
					(list
					 (directory-file-name
					  (file-name-directory
					   (expand-file-name
					    name (car p2)))))))))))
  )


(load-library "w3-sysdp.el")
(or (featurep 'efs)
    (featurep 'efs-auto)
    (require 'ange-ftp))
(require 'w3-vars)
(require 'w3-draw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMS processing for html+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(or (boundp 'MULE) (fset 'string-width 'length))

(defun w3-truncate-string (str len &optional pad)
  "Truncate string STR so that string-width of STR is not greater than LEN.
If width of the truncated string is less than LEN, and if a character PAD is
defined, add padding end of it."
  (if (boundp 'MULE)
      (let ((cl (string-to-char-list str)) (n 0) (sw 0))
	(if (<= (string-width str) len) str
	  (while (<= (setq sw (+ (char-width (nth n cl)) sw)) len)
	    (setq n (1+ n)))
	  (string-match (make-string n ?.) str)
	  (setq str (substring str 0 (match-end 0))))
	(if pad (concat str (make-string (- len (string-width str)) pad)) str))
    (concat (if (> (length str) len) (substring str 0 len) str)
	    (if (or (null pad) (> (length str) len))
		""
	      (make-string (- len (length str)) pad)))))

(defun w3-form-format-int (&rest args)
  (w3-truncate-string (or (nth 0 args) "") (nth 1 args) ?_))

(fset 'w3-form-format-url 'w3-form-format-int)
(fset 'w3-form-format-float 'w3-form-format-int)
(fset 'w3-form-format-date 'w3-form-format-int)

(defun w3-form-format-reset (&rest args)
  (if (string= (nth 0 args) "") "Reset fields" (nth 0 args)))

(defun w3-form-format-password (&rest args)
  (let ((value (or (nth 0 args) ""))
	(size (nth 1 args)))
    (concat (if (>= (length value) size) (make-string size ?*)
	      (make-string (length value) ?*))
	    (if (>= (length value) size) ""
	      (make-string (- size (length value)) ?.)))))

(defun w3-form-format-checkbox (&rest args)
  (concat "[" (if (nth 2 args) "X" " ") "]"))

(fset 'w3-form-format-radio 'w3-form-format-checkbox)

(defun w3-form-format-submit (&rest args)
  (if (string= (nth 0 args) "") "Submit this form" (nth 0 args)))

(defun w3-form-format-text (&rest args)
  (w3-truncate-string (nth 0 args) (nth 1 args) ?_))

(defun w3-form-format-textarea (&rest args)
  "Multiline text entry")

(defun w3-form-format-image (&rest args)
  (car (nth 2 args)))
  
(fset 'w3-form-format- 'w3-form-format-text)
(fset 'w3-form-format-unknown 'w3-form-format-text)

(defun w3-do-text-entry (formobj zone)
  (let ((data (list formobj zone (current-buffer)))
	(buff (get-buffer-create (format "%d:%s" (nth 9 formobj)
					 (nth 3 formobj)))))
    (switch-to-buffer-other-window buff)
    (indented-text-mode)
    (erase-buffer)
    (and (nth 5 formobj) (w3-insert (nth 5 formobj)))
    (setq w3-current-last-buffer data)
    (message "Press C-c C-c when finished with text entry.")
    (local-set-key "\C-c\C-c" 'w3-finish-text-entry)))

(defun w3-finish-text-entry ()
  (interactive)
  (if w3-current-last-buffer
      (let* ((formobj (nth 0 w3-current-last-buffer))
	     (zone (nth 1 w3-current-last-buffer))
	     (buff (nth 2 w3-current-last-buffer))
	     (actn (nth 1 formobj))
	     (type (nth 2 formobj))
	     (name (nth 3 formobj))
	     (deft (nth 4 formobj))
	     (valu (buffer-string))
	     (chkd (nth 6 formobj))
	     (size (nth 7 formobj))
	     (maxl (nth 8 formobj))
	     (ident (nth 9 formobj))
	     (options (nth 10 formobj))
	     (st nil)
	     (nd nil))
	(local-set-key "\C-c\C-c" 'undefined)
	(kill-buffer (current-buffer))
	(condition-case ()
	    (delete-window)
	  (error nil))
	(if (not (and buff (bufferp buff) (buffer-name buff)))
	    (message "Could not find the form buffer for this text!")
	  (switch-to-buffer buff)
	  (if buffer-read-only (toggle-read-only))
	  (setq st (w3-zone-start zone)
		nd (w3-zone-end zone))
	  (w3-delete-zone zone)
	  (w3-add-zone st nd w3-node-style
		       (list 'w3form actn type name deft valu chkd
			     size maxl ident options) t)))
    (if (not buffer-read-only) (toggle-read-only))
    nil))

(defun w3-do-form-entry (formobj zone)
;;; Read in a form entry field.
;;;FORMOBJ is the data returned by w3-zone-at, and contains all the information
;;;        about the entry area (size, type, value, etc)
;;;ZONE is the actual zone object.  This should be able to be passed to
;;;     w3-delete-zone."
  (let* ((actn (nth 1 formobj))
	 (type (nth 2 formobj))
	 (name (nth 3 formobj))
	 (deft (nth 4 formobj))
	 (valu (nth 5 formobj))
	 (chkd (nth 6 formobj))
	 (size (nth 7 formobj))
	 (maxl (nth 8 formobj))
	 (ident (nth 9 formobj))
	 (options (nth 10 formobj))
	 (id (nth 11 formobj))
	 (st (w3-zone-start zone))
	 (nd (w3-zone-end zone))
	 (submit-it nil)
	 (formatfun (intern (concat "w3-form-format-" (downcase type)))))
    (if (not (member type '("SUBMIT" "IMAGE")))
	(progn
	  (if (equal "TEXTAREA" type)
	      (progn
		(if (not buffer-read-only) (toggle-read-only))
		(w3-do-text-entry formobj zone)))
	  (save-excursion
	    (if (not (fboundp formatfun))
		(setq formatfun 'w3-form-format-unknown))
	    (if buffer-read-only (toggle-read-only))
	    (cond
	     ((equal "CHECKBOX" type) (setq chkd (not chkd)))
	     ((equal "RADIO" type) nil)
	     ((equal "TEXTAREA" type) nil)
	     ((equal "RESET" type) (w3-revert-form ident))
	     (t (setq valu
		      (w3-read-correct-format type name options
					      ident valu maxl))))
	    (if (and maxl (not (consp maxl)) (> (length valu) maxl))
		(progn
		  (setq valu (substring valu 0 maxl))
		  (message "Truncated to %d chars (%s)" maxl valu)))
	    (cond
	     ((equal "RESET" type) nil)
	     ((equal "RADIO" type) (w3-set-radio-button zone))
	     ((equal "TEXTAREA" type) nil)
	     (t
	      (w3-delete-zone zone)
	      (delete-region st nd)
	      (goto-char st)
	      (w3-insert (funcall formatfun valu size chkd))
	      (w3-add-zone st (point) w3-node-style
			   (list 'w3form actn type name deft valu chkd
				 size maxl ident options id) t)
	      (set-buffer-modified-p nil)
	      (if (not buffer-read-only) (toggle-read-only))
	      (if w3-running-FSF19 (setq w3-zones-list (w3-only-links)))
 	      (if (boundp 'MULE) (w3-mule-attribute-zones w3-zones-list))
	      ))
	    (cond
	     ((null name) (setq submit-it nil))
	     ((string-match "^isindex$" name) (setq submit-it 'isindex))
	     ((string-match "^internal-gopher$" name) (setq submit-it 'gopher))
	     ((string-match "^internal-wais$" name) (setq submit-it 'wais))
	     ((equal (length (w3-zones-matching ident)) 1)
	      (setq submit-it t)))))
      (let* ((name (cdr chkd))
	     (local-hidden-forms
	      (if (string= type "IMAGE")
		  (append (list
			   (list 'w3form actn "HIDDEN" (concat name ".x") "0"
				 "0" nil nil nil ident nil)
			   (list 'w3form actn "HIDDEN" (concat name ".y") "0"
				 "0" nil nil nil ident nil))
			  w3-hidden-forms))))
	(setq w3-submit-button (if (string= type "IMAGE") nil zone))
	(w3-submit-form ident nil actn)))
    (if submit-it (w3-submit-form ident submit-it actn))))

(defun w3-zones-matching (actn &optional raw)
  ;; Return a list of data entry zones in form number ACTN
  ;; With optional second argument raw, don't grab the data of the zone, but
  ;; return the actual zone."
  (let* ((big (w3-all-zones))
	 (data nil)
	 (result nil))
    ;; Gag Puke Retch
    ;; Sort the list so truly _IDIOTIC_ and _BRAIN DEAD_ people who don't
    ;; know how to write decent form interface scripts and rely on the order
    ;; of the elements being submitted don't screw us over.
    (setq big (sort big (function
			 (lambda (x y)
			   (< (w3-zone-start x) (w3-zone-start y))))))
    (while big
      (setq data (w3-zone-data (car big)))
      (if (and (eq (nth 0 data) 'w3form) ; Its a form field
	       (equal (nth 9 data) actn) ; Its in our form
	       (not (string= (nth 2 data) "IMAGE")) ; Don't want images
	       (not (string= (nth 2 data) "RESET")) ; Don't want resets
	       )
	  (setq result (cons (if raw (car big) data) result)))
      (setq big (cdr big)))
    (if raw
	nil
      (setq big (or (and (boundp 'local-hidden-forms)
			 (symbol-value 'local-hidden-forms))
		    w3-hidden-forms))
      (while big
	(setq data (car big))
	(if (and (eq (nth 0 data) 'w3form) (equal (nth 9 data) actn))
	    (setq result (cons data result)))
	(setq big (cdr big))))
    result))

(defun w3-revert-form (actn)
  (save-excursion
    (let* ((zones (w3-zones-matching actn t))
	   actn data type name deft valu chkd size maxl idnt strt end cur
	   options id formatfun
	   )
      (if buffer-read-only (toggle-read-only))
      (mapcar
       (function
	(lambda (cur)
	  (setq data (w3-zone-data cur)
		actn (nth 1 data)
		type (nth 2 data)
		name (nth 3 data)
		deft (nth 4 data)
		valu (nth 5 data)
		chkd (nth 6 data)
		size (nth 7 data)
		maxl (nth 8 data)
		idnt (nth 9 data)
		options (nth 10 data)
		id (nth 11 data)
		strt (w3-zone-start cur)
		end  (w3-zone-end cur)
		formatfun (intern (concat "w3-form-format-" (downcase type))))
	  (if (not (fboundp formatfun))
	      (setq formatfun 'w3-form-format-unknown))
	  (cond
	   ((or (member type '("SUBMIT" "RESET"))) nil)
	   (t
	    (if (member type '("RADIO" "CHECKBOX"))
		(setq chkd deft)
	      (setq valu deft))
	    (w3-delete-zone cur)
	    (delete-region strt end)
	    (goto-char strt)
	    (w3-insert (funcall formatfun valu size chkd))
	    (w3-add-zone strt (point) w3-node-style
			 (list 'w3form actn type name deft valu chkd
			       size maxl idnt options id) t))))) zones)
	  (if (not buffer-read-only) (toggle-read-only)))
    (if w3-running-FSF19
	(setq w3-zones-list (w3-only-links)))
    (if (boundp 'MULE) (w3-mule-attribute-zones w3-zones-list))
    ))

(defun w3-form-encode-make-mime-part (id data separator)
  (concat separator "\nContent-id: " id
	  "\nContent-length: " (length data)
	  "\n\n" data))

(defun w3-form-encode-multipart/x-www-form-data (formobjs isindex-query)
  ;; Create a multipart form submission.
  ;; Returns a cons of two strings.  Car is the separator used.
  ;; cdr is the body of the MIME message."
  (let ((separator "---some-separator-for-www-form-data"))
    (cons separator
	  (mapconcat
	   (function
	    (lambda (formobj)
	      (cond
	       ((and (member (nth 2 formobj) '("CHECKBOX" "RADIO"))
		     (nth 6 formobj))
		(w3-form-encode-make-mime-part (or (nth 3 formobj)
						   (nth 2 formobj)
						   "unknown")
					       (nth 5 formobj) separator))
	       ((and (member (nth 2 formobj) '("CHECKBOX" "RADIO"))
		     (not (nth 6 formobj)))
		"")
	       ((member (nth 2 formobj)
                        '("RESET" "SUBMIT" "CHECKBOX" "RADIO"))
                (let ((submit-button-data
                       (if w3-submit-button
                           (w3-zone-data w3-submit-button))))
                  (if (and submit-button-data
                           (nth 3 submit-button-data))
                      (prog1
                          (w3-form-encode-make-mime-part
                           (nth 3 submit-button-data)
                           (nth 5 submit-button-data)
                           separator)
                        (setq w3-submit-button nil))
                    "")))
	       ((and (string= (nth 2 formobj) "OPTION")
		     (assoc (nth 5 formobj) (nth 6 formobj)))
		(w3-form-encode-make-mime-part (or (nth 3 formobj)
						   (nth 2 formobj)
						   "unknown")
					       (cdr (assoc (nth 5 formobj)
							   (nth 6 formobj)))
					       separator))
	       ((string= (nth 2 formobj) "FILE")
		(let ((dat nil)
		      (fname (nth 5 formobj)))
		  (save-excursion
		    (set-buffer (get-buffer-create " *w3-temp*"))
		    (erase-buffer)
		    (setq dat
			  (condition-case ()
			      (mm-insert-file-contents fname)
			    (error (concat "Error accessing " fname)))))
		  (w3-form-encode-make-mime-part (or (nth 3 formobj)
						     (nth 2 formobj)
						     "unknown")
						 dat separator)))
	       (t
		(w3-form-encode-make-mime-part (or (nth 3 formobj)
						   (nth 2 formobj)
						   "unknown")
					       (nth 5 formobj)
					       separator)))))
	   formobjs "\n"))))

(fset 'w3-form-encode-multipart/form-data
      'w3-form-encode-multipart/x-www-form-data)

(defun w3-form-encode (result &optional isindex-query enctype)
  "Create a string suitably encoded for a URL request."
  (let ((func (intern (concat "w3-form-encode-" enctype))))
    (if (fboundp func) (funcall func result isindex-query))))

(defun w3-form-encode-text/plain (result &optional isindex-query)
  (let ((query ""))
    (setq query
	  (mapconcat
	   (function
	    (lambda (formobj)
	      (cond
	       ((and (member (nth 2 formobj) '("CHECKBOX" "RADIO"))
		     (nth 6 formobj))
		(concat "\n" (or (nth 3 formobj) (nth 2 formobj)) " "
			(nth 5 formobj)))
	       ((member (nth 2 formobj)
                        '("RESET" "SUBMIT" "CHECKBOX" "RADIO"))
                (let ((submit-button-data
                       (if w3-submit-button
                           (w3-zone-data w3-submit-button))))
                  (if (and submit-button-data (nth 3 submit-button-data))
                      (prog1
                          (concat "\n" (nth 3 submit-button-data) " "
                                  (nth 5 submit-button-data))
                        (setq w3-submit-button nil))
                    "")))
	       ((string= (nth 2 formobj) "TEXTAREA")
		(concat "\n" (or (nth 3 formobj) (nth 2 formobj)) " "
			(mapconcat
			 (function
			  (lambda (x)
			    (if (= x ?\n) "," (char-to-string x))))
			 (nth 5 formobj) "")))
	       ((and (string= (nth 2 formobj) "OPTION")
		     (assoc (nth 5 formobj) (nth 6 formobj)))
		(concat "\n" (or (nth 3 formobj) (nth 2 formobj)) " "
			(cdr (assoc (nth 5 formobj) (nth 6 formobj)))))
	       (t
		(concat "\n" (or (nth 3 formobj) (nth 2 formobj)) " "
			(nth 5 formobj)))))) result ""))
    (if (string= query "") nil
      (setq query (substring query 1 nil)))
    query))

(defun w3-form-encode-application/x-gopher-query (result &optional isindex)
  (concat "\t" (nth 5 (car result))))

(defun w3-form-encode-xwfu (chunk)
  "Escape characters in a string for application/x-www-form-urlencoded.
Blasphemous crap because someone didn't think %20 was good enough for encoding
spaces.  Die Die Die."
  (if (and (boundp 'MULE) chunk)
      (setq chunk (code-convert-string 
		   chunk *internal* url-mule-retrieval-coding-system)))
  (mapconcat
   (function
    (lambda (char)
      (cond
       ((= char ?  ) "+")
       ((or (> char ?z)
	    (< char ?-)
	    (and (< char ?a)
		 (> char ?Z))
	    (and (< char ?@)
		 (> char ?:)))
	(if (< char 16)
	    (upcase (format "%%0%x" char))
	  (upcase (format "%%%x" char))))
       (t
	(char-to-string char))))) chunk ""))

(defun w3-form-encode-application/x-www-form-urlencoded (result &optional isindex-query)
  (let ((query ""))
    (cond
     ((eq isindex-query 'isindex)	; Isindex handling by hypertext
      (while result
	(if (equal (downcase (or (nth 3 (car result)) "")) "isindex")
	    (setq query (url-hexify-string (nth 5 (car result)))
		  result nil))
	(setq result (cdr result))))
     (t					; Normal submission of form
					; This is a little convoluted, but
					; gets only checkboxes that are set
					; and ignores submit & reset buttons
      (setq query
	    (mapconcat
	     (function
	      (lambda (formobj)
		(cond
		 ((and (member (nth 2 formobj) '("CHECKBOX" "RADIO"))
		       (nth 6 formobj))
		  (concat "&" (or (nth 3 formobj) (nth 2 formobj)) "="
			  (w3-form-encode-xwfu (nth 5 formobj))))
		 ((and (member (nth 2 formobj) '("CHECKBOX" "RADIO"))
		       (not (nth 6 formobj))) "")
		 ((member (nth 2 formobj)
                          '("RESET" "SUBMIT" "CHECKBOX" "RADIO"))
                  (let ((submit-button-data
                         (if w3-submit-button
                             (w3-zone-data w3-submit-button))))
                    (if (and submit-button-data (nth 3 submit-button-data))
                        (prog1
                            (concat "&"
                                    (nth 3 submit-button-data)
                                    "="
                                    (w3-form-encode-xwfu
                                     (nth 5 submit-button-data)))
                          (setq w3-submit-button nil))
                      "")))
		 ((and (string= (nth 2 formobj) "OPTION")
		       (assoc (nth 5 formobj) (nth 6 formobj)))
		  (concat "&" (or (nth 3 formobj) (nth 2 formobj)) "="
			  (w3-form-encode-xwfu
			   (cdr (assoc (nth 5 formobj) (nth 6 formobj))))))
		 ((string= (nth 2 formobj) "FILE")
		  (let ((dat nil)
			(fname (nth 5 formobj)))
		    (save-excursion
		      (set-buffer (get-buffer-create " *w3-temp*"))
		      (erase-buffer)
		      (setq dat
			    (condition-case ()
				(progn
				  (mm-insert-file-contents fname)
				  (buffer-string))
			      (error (concat "Error accessing " fname)))))
		    (concat "&" (or (nth 3 formobj) (nth 2 formobj) "unknown")
			    "=" (w3-form-encode-xwfu dat))))
		 (t
		  (concat "&" (or (nth 3 formobj) (nth 2 formobj)) "="
			  (w3-form-encode-xwfu (nth 5 formobj)))))))
	     result ""))
      (if (string= "" query) ""
	(setq query (substring query 1 nil)))))
    query))

(defun w3-form-encode-application/gopher-ask-block (result)
  (let ((query ""))
    ;;; gopher+ will expect all the checkboxes/etc, even if they are
    ;;; not turned on.  Should still ignore RADIO boxes that are not
    ;;; active though.
  (while result
    (if (and (not (and (string= (nth 2 (car result)) "RADIO")
		       (not (nth 6 (car result)))))
	     (not (member (nth 2 (car result)) '("SUBMIT" "RESET"))))
	(setq query (format "%s\r\n%s" query (nth 5 (car result)))))
    (setq result (cdr result)))
  (concat query "\r\n.\r\n")))

(defun w3-submit-form (ident isindex &optional actn)
  ;; Submit form entry fields matching ACTN as their action identifier.
  (let* ((result (reverse (w3-zones-matching ident)))
	 (enctype (cdr (assq 'enctype actn)))
	 (query (w3-form-encode result isindex enctype))
	 (themeth (upcase (cdr (assq 'method actn))))
	 (theurl (cdr (assq 'action actn))))
    (if (and (string= "GET" themeth)
	     (string-match "\\([^\\?]*\\)\\?" theurl))
	(setq theurl (url-match theurl 1)))
    (cond
     ((eq isindex 'gopher) (w3-fetch (concat theurl query)))
     ((eq isindex 'wais)
      (url-perform-wais-query url-current-server url-current-port
			     url-current-file
			     (if (equal (substring query 0 14)
					"internal-wais=")
				 (substring query 14) query))
      (w3-sentinel))
     ((string= "GOPHER-ASK" themeth)
      (setq query (w3-form-encode-ask-block result))
      (w3-fetch (concat theurl (url-hexify-string (concat "\t+\t1\n+-1\r\n"
							 query)))))
     ((or (string= "POST" themeth)
	  (string= "PUT" themeth))
      (if (consp query)
	  (setq enctype (concat enctype "; separator=\""
				(substring (car query) 3 nil)
				"\"")
		query (cdr query)))
      (let ((url-request-method themeth)
	    (url-request-data query)
	    (url-request-extra-headers
	     (cons (cons "Content-type" enctype) url-request-extra-headers)))
	(w3-fetch theurl)))
     ((string= "GET" themeth)
      (let ((theurl (concat theurl "?" query)))
	(w3-fetch theurl)))
     (t
      (w3-warn 'html (format "Unknown submit method: %s" themeth))
      (let ((theurl (concat theurl "?" query)))
	(w3-fetch theurl))))))

(defun w3-matching-radios (ext)
  ;; Return a list of all zones containing radio buttons with the same name
  ;; as that in EXT.
  (let* ((big (w3-all-zones))
	 (idnt (nth 9 (w3-zone-data ext)))
	 (name (nth 3 (w3-zone-data ext)))
	 data cur result)
    (mapcar
     (function
      (lambda (cur)
	(setq data (w3-zone-data cur))
	(if (and
	     (eq (nth 0 data) 'w3form)
	     (equal (nth 9 data) idnt)
	     (equal (nth 3 data) name))
	    (setq result (cons cur result))))) big)
    result))

(defun w3-set-radio-button (ext)
  ;; Set the radio button at EXT to be on.  Will automatically
  ;; toggle other radio butons with the same name to be off.
  (save-excursion
    (let* ((result (w3-matching-radios ext))
	   (idnt (nth 9 (w3-zone-data ext)))
	   (name (nth 3 (w3-zone-data ext)))
	   actn type deft valu chkd size maxl strt end data options id)
      (while result
	(setq data (w3-zone-data (car result))
	      actn (nth 1 data)
	      type (nth 2 data)
	      name (nth 3 data)
	      deft (nth 4 data)
	      valu (nth 5 data)
	      chkd (nth 6 data)
	      size (nth 7 data)
	      maxl (nth 8 data)
	      idnt (nth 9 data)
	      options (nth 10 data)
	      id (nth 11 data)
	      strt (w3-zone-start (car result))
	      end (w3-zone-end (car result)))
	(cond
	 ((and chkd (not (w3-zone-eq
			  ext (car result)))) ; Not supposed to be chkd
	  (w3-delete-zone (car result))	      ; but is.
	  (goto-char strt)
	  (delete-region strt end)
	  (setq chkd nil)
	  (w3-insert (funcall 'w3-form-format-radio valu size chkd))
	  (w3-add-zone strt (point) w3-node-style
		       (list 'w3form actn type name deft valu chkd size maxl
			     idnt options id) t))
	 ((and (not chkd) (w3-zone-eq
			   ext (car result))) ; Supposed to be chkd
	  (w3-delete-zone (car result))       ; but isn't.
	  (goto-char strt)
	  (delete-region strt end)
	  (setq chkd t)
	  (w3-insert (funcall 'w3-form-format-radio valu size chkd))
	  (w3-add-zone strt (point) w3-node-style
		       (list 'w3form actn type name deft valu chkd size maxl
			     idnt options id) t))
	 (t nil)) ; not supposed to be checked, and isn't
	(setq result (cdr result))))
    (if (not buffer-read-only) (toggle-read-only))
    (if w3-running-FSF19 (setq w3-zones-list (w3-only-links)))
    (if (boundp 'MULE) (w3-mule-attribute-zones w3-zones-list))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Type checking for FORMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Date checking, taken from edb.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst weekday-alist
 '(("Sunday" . 0) ("Monday" . 1) ("Tuesday" . 2) ("Wednesday" . 3)
   ("Thursday" . 4) ("Friday" . 5) ("Saturday" . 6)
   ("Tues" . 2) ("Thurs" . 4)
   ("Sun" . 0) ("Mon" . 1) ("Tue" . 2) ("Wed" . 3)
   ("Thu" . 4) ("Fri" . 5) ("Sat" . 6)))

(defconst full-monthname-alist
  '(("January" . 1) ("February" . 2) ("March" . 3) ("April" . 4)
    ("May" . 5) ("June" . 6) ("July" . 7) ("August" . 8)
    ("September" . 9) ("October" . 10) ("November" . 11) ("December" . 12)))

(defconst monthabbrev-alist
  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4) ("May" . 5) ("Jun" . 6)
    ("Jul" . 7) ("Aug" . 8) ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12))
  )

(defconst monthname-alist
  (append monthabbrev-alist
	  full-monthname-alist
	  '(("Sept" . 9))))

(defconst monthname-regexp
  (concat "\\("
	  (mapconcat (function car)
		     monthname-alist
		     "\\|")
	  "\\)\\.?"))

(defconst weekday-regexp
  (concat "\\("
	  (mapconcat (function car)
		     weekday-alist
		     "\\|")
	  "\\)\\.?"))

(defconst monthnumber-regexp "\\(0?[1-9]\\|1[0-2]\\)")
(defconst monthnumber-regexp-two-char "\\(0[1-9]\\|1[0-2]\\)")

(defconst monthday-regexp "\\(0?[1-9]\\|[12][0-9]\\|3[01]\\)")
(defconst monthday-regexp-two-char "\\([0-2][0-9]\\|3[01]\\)")

(defconst full-year-regexp "[0-2][0-9][0-9][0-9]")
(defconst short-year-regexp "[0-9][0-9]")

(defconst year-regexp (concat "\\(" full-year-regexp
			      "\\|" short-year-regexp "\\)"))

(defconst elt-separator-regexp "[ -.,/']+")

(defconst date-regexps
  (list
   ;; MMDDYY
   (cons (concat monthname-regexp
		 elt-separator-regexp
		 monthday-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(4 nil 1 2))
   (cons (concat monthnumber-regexp
		 elt-separator-regexp
		 monthday-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(4 1 nil 2))
   ;; DDMMYY
   (cons (concat monthday-regexp
		 elt-separator-regexp
		 monthname-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(4 nil 2 1))
   (cons (concat "\\("
		 monthday-regexp
		 elt-separator-regexp
		 "\\)?"
		 monthname-regexp
		 elt-separator-regexp
		 year-regexp)
	 '(4 nil 3 2))
   (cons (concat monthday-regexp
		 elt-separator-regexp
		 monthnumber-regexp
		 elt-separator-regexp
		 "\\(" full-year-regexp "\\)")
	 '(3 2 nil 1))
   ;; YYMMDD
   ;; Using year-regexp instead of full-year-regexp is ambiguous (consider
   ;; 11-11-11), but we already tried MMDDYY and it failed.
   (cons (concat year-regexp
		 elt-separator-regexp
		 monthname-regexp
		 elt-separator-regexp
		 monthday-regexp)
	 '(1 nil 2 3))
   (cons (concat year-regexp
		 elt-separator-regexp
		 monthnumber-regexp
		 elt-separator-regexp
		 monthday-regexp)
	 '(1 2 nil 3))
   ;; YYMMDD, no separators
   ;; This is ambiguous.
   (cons (concat year-regexp
		 monthnumber-regexp-two-char "?"
		 monthday-regexp-two-char "?")
	 '(1 2 nil 3))
   ;; WWMMDDYY
   (cons (concat weekday-regexp
		 elt-separator-regexp
		 monthname-regexp
		 elt-separator-regexp
		 monthday-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(5 nil 2 3))
   ;; WWDDMMYY
   (cons (concat weekday-regexp
		 elt-separator-regexp
		 monthday-regexp
		 elt-separator-regexp
		 monthname-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(5 nil 3 2))
   ;; ctime
   (cons (concat
	  weekday-regexp
	  " "
	  monthname-regexp
	  "  ?"
	  monthday-regexp
	  ;; time of day
	  " [0-9:]+ "
	  "\\(" full-year-regexp "\\)")
	 '(4 nil 2 3))
   )
  "Assoc list of regexps and match locators.
A match locator is a list of four numbers indicating which submatch of the
regexp contains the year, month number, month name, and day of the month.
The list elements may be nil if that information is not available.")

(defun w3-datep (date-string)
  (let ((regexp-alist date-regexps)
	result)
    (if (zerop (length date-string))	;if empty string,
	(setq result t)			;empty date is kosher
      ;; regexp-alist is nulled if a match is found
      (progn
	(while regexp-alist
	  (if (string-match (concat "^" (car (car regexp-alist)) "$")
			    date-string)
	      (setq regexp-alist nil
		    result t)
	    ;; string-match failed
	    (setq regexp-alist (cdr regexp-alist))))))
    result))

(defun w3-intp (str)
  (string-match "^[0-9]+$" str))

(defun w3-floatp (str)
  (let (x y)
    (if (string-match "^\\([0-9]+\\)\\.\\([0-9]+\\)$" str)
	(progn
	  (setq x (substring str (match-beginning 1) (match-end 1))
		y (substring str (match-beginning 2) (match-end 2)))
	  (and (w3-intp x) (w3-intp y)))
      (w3-intp str))))

(defun w3-urlp (str)
  (string-match url-nonrelative-link str))

(defun w3-optionp (val)
  (if (null val)
      (progn
	(message "Please make a selection from the menu")
	nil)
    t))

(defun w3-textp (str) t)		; don't care whats in a text field
(fset 'w3-p 'w3-textp)			; for default of "" to be text
(fset 'w3-passwordp 'w3-textp)		; don't care whats in a paswd either
(fset 'w3-textareap 'w3-textp)		; try this - might work
(fset 'w3-rangep 'w3-textp)		; Range is already checked in rcf

(defun w3-filep (fname)
  (and (file-exists-p fname) (file-readable-p fname)))

(defun w3-read-correct-format (type name options num value maxl)
  ;; Read in a FORMS entry with type TYPE, and do typechecking
  ;; will not exit the function until correct type has been entered.
  (let ((func (intern (concat "w3-" (downcase type) "p")))
	(valu value) exitp)
    (while (not exitp)
      (cond
       ((or (equal "TEXT" type)
	    (equal "" type))
	(setq valu (read-string "Enter text: " valu)))
       ((equal "FILE" type)
	(setq valu (expand-file-name (read-file-name "Send file: " "~/"
						     (or valu "/nonexistent")
						     t (or valu "~/")))))
       ((or (equal "FLOAT" type)
	    (equal "INT" type))
	(setq valu (read-string "Enter numeric value: " valu)))
       ((equal "PASSWORD" type)
	(setq valu (funcall url-passwd-entry-func "Enter password:" valu)))
       ((equal "OPTION" type)
	(if (or (and (boundp 'last-input-event)
		     (listp last-input-event)
		     (fboundp 'w3-x-popup-menu))
		(and (boundp 'last-input-event)
		     (fboundp 'button-event-p)
		     (button-event-p last-input-event)
		     (fboundp 'w3-x-popup-menu)))
	    (setq valu (w3-x-popup-menu last-input-event
					(list "WWW"
					      (cons "Select An Item"
						    options))))
	  (setq valu
		(let* ((completion-ignore-case t)
		       (prompt
			(concat "Please choose (default: " valu "): "))
		       (dat (completing-read prompt options nil t)))
		  (if (string= dat "") valu dat))))
	(if (consp valu) (setq valu (car valu))))
       ((equal "RANGE" type)
	(let ((done nil)
	      (min (car maxl))
	      (max (cdr maxl))
	      (tmp ))
	  (setq tmp (min min max)
		max (max min max)
		min tmp)
	  (while (not done)
	    (setq valu (string-to-int
			(read-string (concat "Value (" min " - " max "): ")))
		  done (and (<= valu max) (>= valu min)))
	    (if (not done)
		(progn
		  (message "Please enter a number between %d and %d!" min
			   max)
		  (sit-for 2))))
	  (setq valu (int-to-string valu))))
       ((equal "DATE" type)
	(setq valu (read-string "Enter date: " valu)))
       ((equal "URL" type)
	(setq valu (read-string "Enter valid URL: " valu)))
       (t
	(setq valu (read-string "Enter text: " valu))))
      (if (not (fboundp func)) (setq func 'w3-textp))
      (if (funcall func valu)
	  (setq exitp t)
	(progn
	  (message "Wrong format for type %s, try again." (downcase type))
	  (sit-for 2))))
    valu))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code for printing out roman numerals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-decimal-to-roman (n)
  ;; Convert from decimal to roman numerals
  (let ((curmod 1000)
	(str "")
	(j 7)
	i2 k curcnt)
    (while (>= curmod 1)
      (if (>= n curmod)
	  (progn
	    (setq curcnt (/ n curmod)
		  n (- n (* curcnt curmod)))
	    (if (= 4 (% curcnt 5))
		(setq i2 (+ j (if (> curcnt 5) 1 0))
		      str (format "%s%c%c" str
				  (aref w3-roman-characters (1- j))
				  (aref w3-roman-characters i2)))
	      (progn
		(if (>= curcnt 5)
		    (setq str (format "%s%c" str (aref w3-roman-characters j))
			  curcnt (- curcnt 5)))
		(setq k 0)
		(while (< k curcnt)
		  (setq str (format "%s%c" str
				    (aref w3-roman-characters (1- j)))
			k (1+ k)))))))
      (setq curmod (/ curmod 10)
	    j (- j 2)))
    str))

(defun w3-decimal-to-alpha (n)
  ;; Convert from decimal to alphabetical (a, b, c, ..., aa, ab,...)
  (cond
   ((< n 1) (char-to-string ?Z))
   ((<= n 26) (char-to-string (+ ?A (1- n))))
   (t (concat (char-to-string (+ ?A (1- (/ n 27))))
	      (w3-decimal-to-alpha (% n 26))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for formatting nested lists in html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-expand-list (data)
  ;; Expand a list that has been hidden.
  (let ((buffer-read-only nil))
    (w3-unhide-zone (nth 1 data) (nth 2 data))))

(defun w3-rehide-list (data)
  ;; Hide a list that was viewable.
  (let ((buffer-read-only nil))
    (w3-hide-zone (nth 1 data) (nth 2 data))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for compatibility with XMosaic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parse out the Mosaic documents-menu file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-parse-docs-menu ()
  ;; Parse the Mosaic documents menu
  (let ((tmp-menu (append '((separator)) w3-starting-documents
			  '((separator))))
	real-menu x y name url)
    (if (or (not (file-exists-p w3-documents-menu-file))
	    (not (file-readable-p w3-documents-menu-file)))
	(message "No documents menu found... continuing.")
      (save-excursion
	(set-buffer (get-buffer-create " *w3-temp*"))
	(erase-buffer)
	(mm-insert-file-contents w3-documents-menu-file)
	(goto-char (point-min))
	(while (not (eobp))
	  (if (not (looking-at "-+$"))
	      (setq x (progn (beginning-of-line) (point))
		    y (progn (end-of-line) (point))
		    name (prog1
			     (buffer-substring x y)
			   (delete-region x (min (1+ y) (point-max))))
		    x (progn (beginning-of-line) (point))
		    y (progn (end-of-line) (point))
		    url (prog1
			    (buffer-substring x y)
			  (delete-region x (min (1+ y) (point-max))))
		    tmp-menu (if (rassoc url tmp-menu) tmp-menu
			       (cons (cons name url) tmp-menu)))
	    (setq tmp-menu (cons '(separator) tmp-menu))
	    (delete-region (point-min) (min (1+ (progn (end-of-line)
						       (point)))
					    (point-max)))))
	(kill-buffer (current-buffer))))
    (if (equal (car (car tmp-menu)) "") (setq tmp-menu (cdr tmp-menu)))
    (while tmp-menu
      (setq real-menu (cons (if (equal 'separator (car (car tmp-menu)))
				"--------"
			      (vector (car (car tmp-menu))
				      (list 'w3-fetch
					    (if (listp (cdr (car tmp-menu)))
						(car (cdr (car tmp-menu)))
					      (cdr (car tmp-menu)))) t))
			    real-menu)
	    tmp-menu (cdr tmp-menu)))
    (setq w3-navigate-menu (append w3-navigate-menu real-menu
				   (list "-----")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private annotation support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-parse-personal-annotations ()
  ;; Read in personal annotation file
  (if (and
       (file-exists-p (format "%s/LOG" w3-personal-annotation-directory))
       (file-readable-p (format "%s/LOG" w3-personal-annotation-directory)))
      (save-excursion
	(setq w3-personal-annotations nil);; nuke the old list
	(let ((start nil)
	      (end nil)
	      (txt nil)
	      (url nil)
	      (num nil))
	  (set-buffer (get-buffer-create " *panno*"))
	  (erase-buffer)
	  (mm-insert-file-contents
	   (format "%s/LOG" w3-personal-annotation-directory))
	  (goto-char (point-min))
	  (w3-replace-regexp "\n+" "\n")
	  (goto-char (point-min))
	  ;; nuke the header lines
	  (delete-region (point-min) (progn (forward-line 2) (point)))
	  (cond
	   ((eobp) nil)			; Empty LOG file
	   (t
	    (if (/= (char-after (1- (point-max))) ?\n)
		(save-excursion
		  (goto-char (point-max))
		  (w3-insert "\n")))
	    (while (not (eobp))
	      (setq start (point)
		    end (prog2 (end-of-line) (point) (forward-char 1))
		    txt (buffer-substring start end)
		    url (substring txt 0 (string-match " " txt))
		    num (url-split
			 (substring txt (1+ (string-match " " txt)) nil)
			 "[ \t]"))
	      (while num
		(setq w3-personal-annotations
		      (cons
		       (list url
			     (list (car (car num))
				   (w3-grok-annotation-format
				    (car (car num)))))
		       w3-personal-annotations)
		      num (cdr num))))))
	  (kill-buffer " *panno*")))))

(defun w3-grok-annotation-format (anno)
  ;; Grab the title from an annotation
  (let ((fname  (format "%s/PAN-%s.html"
			w3-personal-annotation-directory anno)))
    (save-excursion
      (set-buffer (get-buffer-create " *annotmp*"))
      (erase-buffer)
      (if (file-exists-p fname)
	  (mm-insert-file-contents fname))
      (goto-char (point-min))
      (prog1
	  (if (re-search-forward "<title>\\(.*\\)</title>" nil t)
	      (buffer-substring (match-beginning 1) (match-end 1))
	    (concat "Annotation on "
		    (current-time-string (nth 5 (file-attributes fname)))))
	(kill-buffer " *annotmp*")))))

(defun w3-is-personal-annotation (url)
  ;; Is URL a personal annotation?
  (string-match "file:/.*/PAN-.*\\.html" url))

(defun w3-delete-personal-annotation ()
  "Delete a personal annotation."
  (interactive)
  (if (w3-is-personal-annotation (url-view-url t))
      (let ((num nil)
	    (annotated-url nil)
	    (anno w3-personal-annotations))
	(string-match "file:/.*/PAN-\\(.*\\)\\.html" (url-view-url t))
	(setq num (substring (url-view-url t) (match-beginning 1)
			     (match-end 1)))
	(while anno
	  (if (equal num (car (car (cdr (car anno)))))
	      (setq annotated-url (car (car anno))))
	  (setq anno (cdr anno)))
	(if annotated-url
	    (save-excursion
	      (set-buffer (get-buffer-create " *annotmp*"))
	      (erase-buffer)
	      (mm-insert-file-contents (format "%s/LOG"
					    w3-personal-annotation-directory))
	      (replace-regexp (format "[ \t]+\\b%s\\b[ \t]*" num) " ")
	      (goto-char (point-min))
	      (delete-matching-lines (format "^%s +$" annotated-url))
	      (let ((make-backup-files nil)
		    (version-control nil)
		    (require-final-newline t))
		(write-region (point-min) (point-max)
			      (format "%s/LOG"
				      w3-personal-annotation-directory)))
	      (kill-buffer " *annotmp*")
	      (setq anno w3-personal-annotations
		    w3-personal-annotations nil)
	      (while anno
		(if (not (string= num (car (car (cdr (car anno))))))
		    (setq w3-personal-annotations
			  (cons (car anno) w3-personal-annotations)))
		(setq anno (cdr anno)))
	      (delete-file (format "%s/PAN-%s.html"
				   w3-personal-annotation-directory num)))
	  (message "Couldn't find url that this is annotating!")))
    (message "This isn't a personal annotation.")))

(defun w3-personal-annotation-add ()
  "Add an annotation to this document."
  (interactive)
  (let ((url (url-view-url t))
	(buf (get-buffer-create "*Personal Annotation*"))
	(title (read-string "Title: "
			    (format "Annotation by %s on %s"
				    (user-real-login-name)
				    (current-time-string)))))
    (set-buffer buf)
    (if w3-mutable-windows (pop-to-buffer buf) (switch-to-buffer buf))
    (erase-buffer)
    (if (and w3-annotation-mode (fboundp w3-annotation-mode))
	(funcall w3-annotation-mode)
      (message "%S is undefined, using %s" w3-annotation-mode
	       default-major-mode)
      (funcall default-major-mode))
    (w3-annotation-minor-mode 1)
    (setq w3-current-annotation (cons url title))
    (insert "<html>\n"
	    " <head>\n"
	    "  <title>" (w3-insert-entities-in-string title) "</title>"
	    " </head>\n"
	    "  <h1>" (w3-insert-entities-in-string title) "</h1>\n"
	    "  <p>\n"
	    "   <address>" (w3-insert-entities-in-string (user-full-name))
            (if (stringp url-personal-mail-address)
                (concat " <" (w3-insert-entities-in-string
			      url-personal-mail-address) ">")
              "")
	    "</address>\n"
	    "   <address>" (current-time-string) "</address>\n"
	    "  </p>\n"
	    "  <pre>\n")
    (save-excursion
      (insert "\n\n\n  </pre>\n"
	      "</html>"))
    (message "Hit C-cC-c to send this annotation.")))

(defun w3-annotation-minor-mode (&optional arg)
  "Minimal minor mode for entering annotations.  Just rebinds C-cC-c to
finish the annotation."
  (interactive "P")
  (cond
   ((null arg) (setq w3-annotation-minor-mode (not w3-annotation-minor-mode)))
   ((= 0 arg)  (setq w3-annotation-minor-mode nil))
   (t          (setq w3-annotation-minor-mode t)))
  (cond
   ((or w3-running-FSF19 w3-running-xemacs))
   (t (local-set-key "\C-c\C-c" 'w3-personal-annotation-finish)))
  )

(defun w3-annotation-find-highest-number ()
  ;; Find the highest annotation number in this buffer
  (let (x)
    (goto-char (point-min))
    (while (re-search-forward "[^ \t\n]*[ \t]\\(.*\\)" nil t)
      (setq x (nconc (mapcar (function (lambda (x) (string-to-int (car x))))
			     (url-split (buffer-substring (match-beginning 1)
							 (match-end 1))
				       "[ \t]")) x)))
    (if (not x) (setq x '(0)))
    (1+ (car (sort x '>)))))

(defun w3-personal-annotation-finish ()
  "Finish doing a personal annotation."
  (interactive)
  (cond
   ((or w3-running-FSF19 w3-running-xemacs))
   (t (local-set-key "\C-c\C-c" 'undefined)))
  (if (or (not w3-personal-annotation-directory)
	  (not (file-exists-p w3-personal-annotation-directory))
	  (not (file-directory-p w3-personal-annotation-directory)))
      (error "No personal annotation directory!")
    (let ((url (car w3-current-annotation))
	  (txt (buffer-string))
	  (title (cdr w3-current-annotation))
	  (fname nil)
	  (num nil))
      (save-excursion
	(not-modified)
	(kill-buffer (current-buffer))
	(set-buffer (get-buffer-create " *annotmp*"))
	(erase-buffer)
	(if (file-exists-p		; Insert current LOG file if
					; it exists.
	     (format "%s/LOG" w3-personal-annotation-directory))
	    (mm-insert-file-contents
	     (format "%s/LOG" w3-personal-annotation-directory))
	  (progn			; Otherwise, create a file
	    (goto-char (point-min))	; that conforms to first
					; annotation format from NCSA
	    (w3-insert "ncsa-mosaic-personal-annotation-log-format-1\n")
	    (w3-insert "Personal\n")))
	(goto-char (point-min))
	(setq num (int-to-string (w3-annotation-find-highest-number))
	      fname (format "%s/PAN-%s.html"
			    w3-personal-annotation-directory num))
	(goto-char (point-min))
	(if (re-search-forward (regexp-quote url) nil t)
	    (progn
	      (end-of-line)
	      (w3-insert " "))
	  (goto-char (point-max))
	  (w3-insert "\n" url " "))
	(w3-insert num)
	(let ((make-backup-files nil)
	      (version-control nil)
	      (require-final-newline t))
	  (write-region (point-min) (point-max)
			(format "%s/LOG" w3-personal-annotation-directory))
	  (erase-buffer)
	  (w3-insert w3-annotation-marker txt)
	  (write-region (point-min) (point-max) fname))
	(setq w3-personal-annotations
	      (cons (list url (list num title)) w3-personal-annotations))))))

(defun w3-annotation-add ()
  "Add an annotation to the current document."
  (interactive)
  (w3-personal-annotation-add))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to pass files off to external viewers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-start-viewer (fname cmd &optional view)
  "Start a subprocess, named FNAME, executing CMD
If third arg VIEW is non-nil, show the output in a buffer when
the subprocess exits."
  (if view (save-excursion
	     (set-buffer (get-buffer-create view))
	     (erase-buffer)))
  (let ((proc
	 (start-process fname view (or shell-file-name
				       (getenv "ESHELL")
				       (getenv "SHELL")
				       "/bin/sh") "-c" cmd)))
    proc))

(defun w3-viewer-filter (proc string)
  ;; A process filter for asynchronous external viewers
  (let ((buff (get-buffer-create (url-generate-new-buffer-name
				  (symbol-name
				   (read (nth 2 (process-command proc))))))))
    (save-excursion
      (set-buffer buff)
      (erase-buffer)
      (insert string)
      (set-process-buffer proc buff)
      (set-process-filter proc nil))))

(defun w3-viewer-sentinel (proc string)
  ;; Delete any temp files left from a viewer process.
  (let ((fname (process-name proc))
	(buffr (process-buffer proc)))
    (if (and (file-exists-p fname)
	     (file-writable-p fname))
	(delete-file fname))
    (if buffr
	(if w3-mutable-windows
	    (pop-to-buffer buffr)
	  (switch-to-buffer buffr)))))

(defun w3-notify-when-ready (buff)
  "Notify the user when BUFF is ready.
See the variable `w3-notify' for the different notification behaviors."
  (if (stringp buff) (setq buff (get-buffer buff)))
  (cond
   ((null buff) nil)
   ((eq w3-notify 'newframe)
    ;; Since we run asynchronously, perhaps while Emacs is waiting for input,
    ;; we must not leave a different buffer current.
    ;; We can't rely on the editor command loop to reselect
    ;; the selected window's buffer.
    (save-excursion
      (set-buffer buff)
      (make-frame)))
   ((eq w3-notify 'bully)
    (pop-to-buffer buff)
    (delete-other-windows))
   ((eq w3-notify 'aggressive)
    (pop-to-buffer buff))
   ((eq w3-notify 'friendly)
    (display-buffer buff 'not-this-window))
   ((eq w3-notify 'polite)
    (beep)
    (message "W3 buffer %s is ready." (buffer-name buff)))
   ((eq w3-notify 'quiet)
    (message "W3 buffer %s is ready." (buffer-name buff)))
   (t (message ""))))

(defun w3-pass-to-viewer ()
  ;; Pass a w3 buffer to a viewer
  (set-buffer url-working-buffer)
  (let* ((info  url-current-mime-viewer) 	   ; All the MIME viewer info
	 (view (cdr-safe (assoc "viewer" info))) ; How to view this file
	 (url (url-view-url t))
	 (fmt  (cdr-safe (assoc "nametemplate" info)))) ; Template for name
    (cond
     (fmt nil)
     ((cdr-safe (assoc "type" info))
      (setq fmt (mm-type-to-file (cdr-safe (assoc "type" info))))
      (if fmt (setq fmt (concat "%s" (car fmt)))
	(setq fmt (concat "%s" (url-file-extension url-current-file))))))
    (if (null view)
	(setq view 'indented-text-mode))
    (cond
     ((symbolp view)
      (if (not (memq view '(w3-prepare-buffer w3-print w3-source
					      w3-default-local-file
					      mm-multipart-viewer)))
	  (let ((bufnam (url-generate-new-buffer-name
			 (file-name-nondirectory
			  (or url-current-file "Unknown")))))
	    (if (string= bufnam "")
		(setq bufnam (url-generate-new-buffer-name
			      (url-view-url t))))
	    (rename-buffer bufnam)
	    (set-buffer-modified-p nil)
	    (buffer-enable-undo)
	    (funcall view)
	    (w3-notify-when-ready bufnam))
	(funcall view)))
     ((stringp view)
      (let ((fname (url-generate-unique-filename fmt)) proc)
	(if (url-file-directly-accessible-p (url-view-url t))
	    (make-symbolic-link url-current-file fname t)
	  (if (boundp 'MULE)
	      (write-region (point-min) (point-max) fname nil nil *noconv*)
	    (write-region (point-min) (point-max) fname)))
	(if (get-buffer url-working-buffer)
	    (kill-buffer url-working-buffer))
	(if (string-match "%s" view)
	    (setq view (concat (substring view 0 (match-beginning 0))
			       fname (substring view (match-end 0)))))
	(if (string-match "%u" view)
	    (setq view (concat (substring view 0 (match-beginning 0))
			       url
			       (substring view (match-end 0)))))
	(message "Passing to viewer %s " view)
	(setq proc (w3-start-viewer fname view))
	(set-process-filter proc 'w3-viewer-filter)
	(set-process-sentinel proc 'w3-viewer-sentinel)))
     ((listp view)
      (set-buffer-modified-p nil)
      (buffer-enable-undo)
      (eval view))
     (t
      (message "Unknown viewer specified: %s" view)
      (w3-notify-when-ready url-working-buffer)))))

(defun w3-save-binary-file ()
  "Save a buffer to disk - this is used when `w3-dump-to-disk' is non-nil"
  (interactive)
  (let ((x (read-file-name "Filename to save as: "
			   (or mm-download-directory "~/")
			   (concat (or mm-download-directory "~/")
				   (url-basepath (or url-current-file "") t))
			   nil
			   (url-basepath (or url-current-file "") t))))
    (save-excursion
      ;; more fixes from the MULE guys
      (if w3-dump-to-disk
	  (let (jka-compr-compression-info-list
		jam-zcat-filename-list)
	    (if (boundp 'MULE)
		(let ((mc-flag t))
		  (write-file x *noconv*))
	      (write-file x)))
	(if (boundp 'MULE)
	    (let ((mc-flag t))
	      (write-file x *noconv*))
	  (write-file x)))
      (kill-buffer (current-buffer)))))

(defun w3-build-url (protocol)
  "Build a url for PROTOCOL, return it as a string"
  (interactive (list (cdr (assoc (completing-read
				  "Protocol: "
				  w3-acceptable-protocols-alist nil t)
				 w3-acceptable-protocols-alist))))
  (let (user host port file)
    (cond
     ((null protocol) (error "Protocol is unknown to me!"))
     ((string= protocol "news")
      (setq host (read-string "Enter news server name, or blank for default: ")
	    port (read-string "Enter port number, or blank for default: ")
	    file (read-string "Newgroup name or Message-ID: ")))
     ((string= protocol "mailto") (setq file (read-string "E-mail address: ")))
     ((string= protocol "http")
      (setq host (read-string "Enter server name: ")
	    port (read-string "Enter port number, or blank for default: ")
	    file (read-string "Remote file: "))
      (and (string= "" port) (setq port nil))
      (and (string= "" host) (error "Must specify a remote machine!")))
     ((string= protocol "file")
      (if (funcall url-confirmation-func "Local file?")
	  (setq file (read-file-name "Local File: " nil nil t))
	(setq user (read-string "Login as user (blank=anonymous): ")
	      host (read-string "Remote machine name: "))
	(and (string= user "") (setq user "anonymous"))
	(and (string= host "") (error "Must specify a remote machine!"))
	(setq file (read-file-name "File: " (format "/%s@%s:" user host)
				   nil t)
	      file (substring file (length (format "/%s@%s:" user host))))))
     ((or (string= protocol "telnet")
	  (string= protocol "tn3270"))
      (setq user (read-string "Login as user (blank=none): ")
	    host (read-string "Remote machine name: ")
	    port (read-string "Port number (blank=23): "))
      (and (string= "" port) (setq port nil))
      (and (string= "" user) (setq user nil))
      (and (string= "" host) (error "Must specify a host machine!")))
     ((string= protocol "gopher")
      (setq host (read-string "Enter server name: ")
	    port (read-string "Enter port number, or blank for default: ")
	    file (read-string "Remote file: "))
      (and (string= "" port) (setq port nil))
      (and (string= "" host) (error "Must specify a remote machine!"))))
    (message "%s:%s%s"
	     protocol
	     (if (null host) "" (concat "//" host
					(if (null port) "" (concat ":" port))))
	     (if (= ?/ (string-to-char file)) file (concat "/" file)))))

;;;###autoload
(defun w3-open-local (fname)
  "Find a local file, and interpret it as a hypertext document.
It will prompt for an existing file or directory, and retrieve it as a
hypertext document.  If it is a directory, and url-use-hypertext-dired
is non-nil, then an HTML directory listing is created on the fly.
Otherwise, dired-mode is used to visit the buffer."
  (interactive "FLocal file: ")
  (if (not w3-setup-done) (w3-do-setup))
  (w3-fetch (concat "file:" fname)))

;;;###autoload
(defun w3-fetch-other-frame (&optional url)
  "Attempt to follow the hypertext reference under point in a new frame.
With prefix-arg P, ignore viewers and dump the link straight
to disk."
  (interactive (list (w3-read-url-with-default)))
  (cond
   ((and (fboundp 'make-frame)
	 (fboundp 'select-frame)
	 (not (eq (device-type) 'tty)))
    (let ((frm (make-frame)))
      (select-frame frm)
      (w3-fetch url)))
   (t (w3-fetch url))))

(defun w3-url-completion-function (string predicate function)
  (if (not w3-setup-done) (w3-do-setup))
  (cond
   ((null function)
    (cond
     ((get 'url-gethash 'sysdep-defined-this)
      ;; Cheat!  If we know that these are the sysdep-defined version
      ;; of hashtables, they are an obarray.
      (try-completion string url-global-history-hash-table predicate))
     ((url-hashtablep url-global-history-hash-table)
      (let ((list nil))
	(url-maphash (function (lambda (key val)
				 (setq list (cons (cons key val) list))))
		     url-global-history-hash-table)
	(try-completion string (nreverse list) predicate)))
     (t nil)))
   ((eq function t)
    (cond
     ((get 'url-gethash 'sysdep-defined-this)
      ;; Cheat!  If we know that these are the sysdep-defined version
      ;; of hashtables, they are an obarray.
      (all-completions string url-global-history-hash-table predicate))
     ((url-hashtablep url-global-history-hash-table)
      (let ((stub (concat "^" (regexp-quote string)))
	    (retval nil))
	(url-maphash
	 (function
	  (lambda (url time)
	    (if (string-match stub url)
		(setq retval (cons url retval)))))
	 url-global-history-hash-table)
	retval))
     (t nil)))
   ((eq function 'lambda)
    (and (url-hashtablep url-global-history-hash-table)
	 (url-gethash string url-global-history-hash-table)
	 t))))

(defun w3-read-url-with-default ()
  (url-do-setup)
  (let* ((completion-ignore-case t)
	 (default
	   (if (eq major-mode 'w3-mode)
	       (if (and current-prefix-arg (w3-view-this-url t))
		   (w3-view-this-url t)
		 (url-view-url t))
	     (url-get-url-at-point)))
	 (url nil))
    (setq url
	  (completing-read "URL: "  'w3-url-completion-function
			   nil nil default))
    (if (string= url "")
	(setq url (if (eq major-mode 'w3-mode)
		      (if (and current-prefix-arg (w3-view-this-url t))
			  (w3-view-this-url t)
			(url-view-url t))
		    (url-get-url-at-point))))
    url))

;;;###autoload
(defun w3-fetch (&optional url)
  "Retrieve a document over the World Wide Web.
The World Wide Web is a global hypertext system started by CERN in
Switzerland in 1991.

The document should be specified by its fully specified
Uniform Resource Locator.  The document will be parsed, printed, or
passed to an external viewer as appropriate.  Variable
`mm-mime-info' specifies viewers for particular file types."
  (interactive (list (w3-read-url-with-default)))
  (if (not w3-setup-done) (w3-do-setup))
  (if (boundp 'w3-working-buffer)
      (setq w3-working-buffer url-working-buffer))
  (if (and (boundp 'command-line-args-left)
	   command-line-args-left
	   (string-match url-nonrelative-link (car command-line-args-left)))
      (setq url (car command-line-args-left)
	    command-line-args-left (cdr command-line-args-left)))
  (if (equal url "") (error "No document specified!"))
  ;; In the common case, this is probably cheaper than searching.
  (while (= (string-to-char url) ? )
    (setq url (substring url 1)))
  (if (= (string-to-char url) ?#)
      (w3-relative-link url)
    (let ((x (url-view-url t))
	  (lastbuf (current-buffer))
	  (buf (url-buffer-visiting url)))
      (if (not w3-setup-done) (w3-do-setup))
      (and x (or (string= "file:nil" x) (string= "" x))
	   (setq x nil))
      (if (or (not buf)
	      (cond
	       ((not (equal (downcase (or url-request-method "GET")) "get")) t)
	       ((memq w3-reuse-buffers '(no never reload)) t)
	       ((memq w3-reuse-buffers '(yes reuse always)) nil)
	       (t
		(if (and w3-reuse-buffers (not (eq w3-reuse-buffers 'ask)))
		    (progn
		      (ding)
		      (message
		       "Warning: Invalid value for variable w3-reuse-buffers: %s"
		       (prin1-to-string w3-reuse-buffers))
		      (sit-for 2)))
		(not (funcall url-confirmation-func
			      (format "Reuse URL in buffer %s? "
				      (buffer-name buf)))))))
	  (let ((cached (url-retrieve url)))
	    (if w3-track-last-buffer
		(setq w3-last-buffer (get-buffer url-working-buffer)))
	    (if (get-buffer url-working-buffer)
		(cond
		 ((and url-be-asynchronous (string-match "^http:" url)
		       (not cached))
		  (save-excursion
		    (set-buffer url-working-buffer)
		    (if x
			(w3-add-urls-to-history x (url-view-url t)))
		    (setq w3-current-last-buffer lastbuf)))
		 (t
		  (w3-add-urls-to-history x url)
		  (w3-sentinel lastbuf)))))
	(if w3-track-last-buffer 
	    (setq w3-last-buffer buf))
	(let ((w3-notify (if (memq w3-notify '(newframe bully aggressive))
			     w3-notify
			   'aggressive)))
	  (w3-notify-when-ready buf))
	(if (string-match "#\\(.*\\)" url)
	    (progn
	      (push-mark (point) t)
	      (w3-find-specific-link (url-match url 1))))
	(message "Reusing URL.  To reload, type %s."
		 (substitute-command-keys "\\[w3-reload-document]"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; History for forward/back buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-node-history nil "History for forward and backward jumping")

(defun w3-plot-course ()
  "Show a map of where the user has been in this session of W3. !!!!NYI!!!"
  (interactive)
  (error "Sorry, w3-plot-course is not yet implemented."))

(defun w3-forward-in-history ()
  "Go forward in the history from this page"
  (interactive)
  (let* ((thisurl (url-view-url t))
	 (node (assoc (if (string= "" thisurl) (current-buffer) thisurl)
		      w3-node-history))
	 (url (cdr node))
	 (w3-reuse-buffers 'yes))
    (cond
     ((null url) (error "No forward found for %s" thisurl))
     ((and (bufferp url) (buffer-name url))
      (switch-to-buffer url))
     ((stringp url)
      (w3-fetch url))
     ((bufferp url)
      (setq w3-node-history (delete node w3-node-history))
      (error "Killed buffer in history, removed."))
     (t
      (error "Something is very wrong with the history!")))))

(defun w3-backward-in-history ()
  "Go backward in the history from this page"
  (interactive)
  (let* ((thisurl (url-view-url t))
	 (node (rassoc (if (string= thisurl "") (current-buffer) thisurl)
			  w3-node-history))
	 (url (car node))
	 (w3-reuse-buffers 'yes))
    (cond
     ((null url) (error "No backward found for %s" thisurl))
     ((and (bufferp url) (buffer-name url))
      (switch-to-buffer url))
     ((stringp url)
      (w3-fetch url))
     ((bufferp url)
      (setq w3-node-history (delete node w3-node-history))
      (error "Killed buffer in history, removed."))
     (t
      (error "Something is very wrong with the history!")))))

(defun w3-add-urls-to-history (referer url)
  "REFERER is the url we followed this link from.  URL is the link we got to."
  (let ((node (assoc referer w3-node-history)))
    (if node
	(setcdr node url)
      (setq w3-node-history (cons (cons referer url) w3-node-history)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-executable-exists-in-path (exec &optional path)
  (let ((paths (if (consp path)
		   path
		 (mm-string-to-tokens (or path
					  (getenv "PATH")
					  (concat
					   "/usr/bin:/bin:/usr/local/bin:"
					   "/usr/bin/X11:"
					   (expand-file-name "~/bin"))) ?:)))
	(done nil))
    (while (and paths (not done))
      (if (file-exists-p (expand-file-name exec (car paths)))
	  (setq done t))
      (setq paths (cdr paths)))
    done))

(defun w3-document-information (&optional buff)
  "Display information on the document in buffer BUFF"
  (interactive)
  (setq buff (or buff (current-buffer)))
  (save-excursion
    (set-buffer buff)
    (let* ((url (url-view-url t))
	   (cur-links w3-current-links)
	   (title (buffer-name))
	   (lastmod (or (cdr-safe (assoc "last-modified"
					 url-current-mime-headers))
			(and (member url-current-type '("file" "ftp"))
			     (nth 7 (url-file-attributes url)))))
	   (hdrs url-current-mime-headers))
      (set-buffer (get-buffer-create "Document Info"))
      (erase-buffer)
      (cond
       ((stringp lastmod) nil)
       ((equal '(0 . 0) lastmod) (setq lastmod ""))
       ((consp lastmod) (setq lastmod (current-time-string lastmod)))
       (t (setq lastmod "")))
      (insert "        Title: " title   "\n"
	      "     Location: " url     "\n")
      (if lastmod (insert "Last Modified: " lastmod "\n"))
      (if hdrs
	  (let* ((maxlength (car (sort (mapcar (function (lambda (x)
							   (length (car x))))
					       hdrs)
				       '>)))
		 (fmtstring (format "%%%ds: %%s" maxlength)))
		
	    (insert
	     (make-string (1- (window-width)) ?-)
	     "\nMetaInformation:\n"
	     (mapconcat
	      (function
	       (lambda (x)
		 (if (/= (length (car x)) 0)
		     (format fmtstring
			     (capitalize (car x))
			     (if (numberp (cdr x))
				 (int-to-string (cdr x))
			       (cdr x))))))
	      (sort hdrs
		    (function (lambda (x y) (string-lessp (car x) (car y)))))
	      "\n") "\n")))
      (if cur-links
	  (progn
	    (insert (make-string (1- (window-width)) ?-)
		    "Document-defined link relations:\n")
	    (mapcar
	     (function
	      (lambda (x)
		(insert (car x) ":\n")
		(mapcar (function
			 (lambda (x)
			   (insert (format "  %15s -- %s\n" (car x) (cdr x)))))
			(cdr x))))
	     cur-links)))
      (goto-char (point-min))
      (display-buffer "Document Info"))))

(defun w3-truncate-menu-item (string)
  (if (<= (length string) w3-max-menu-width)
      string
    (concat (substring string 0 w3-max-menu-width) "$")))

(defun w3-use-starting-documents ()
  "Use the list of predefined starting documents from w3-starting-documents"
  (interactive)
  (let ((w3-hotlist w3-starting-documents))
    (w3-use-hotlist)))

(defun w3-show-starting-documents ()
  "Show the list of predefined starting documents from w3-starting-documents"
  (interactive)
  (if (not w3-setup-done) (w3-do-setup))
  (w3-fetch "www://auto/starting-points"))

(defun w3-insert-formatted-url (p)
  "Insert a formatted url into a buffer.  With prefix arg, insert the url
under point."
  (interactive "P")
  (let (buff str)
    (cond
     (p
      (setq p (w3-view-this-url t))
      (or p (error "No url under point"))
      (setq str (format "<A HREF=\"%s\">%s</A>" p
			(read-string "Link text: "
				     (nth 3 (w3-zone-data
					     (w3-zone-at (point))))))))
     (t
      (setq str (format "<A HREF=\"%s\">%s</A>" (url-view-url t)
			(read-string "Link text: " (buffer-name))))))
    (setq buff (read-buffer "Insert into buffer: " nil t))
    (if buff
	(save-excursion
	  (set-buffer buff)
	  (w3-insert str))
      (message "Cancelled."))))

(defun w3-first-n-items (l n)
  "Return the first N items from list L"
  (let ((x 0)
	y)
    (if (> n (length l))
	(setq y l)
      (while (< x n)
	(setq y (nconc y (list (nth x l)))
	      x (1+ x))))
    y))

(defun w3-breakup-menu (menu-desc max-len)
  (if (> (length menu-desc) max-len)
      (cons (cons "More..." (w3-first-n-items menu-desc max-len))
	    (w3-breakup-menu (nthcdr max-len menu-desc) max-len))
    menu-desc))

;;;###autoload
(defun w3-maybe-follow-link-mouse (e)
  "Maybe follow a hypertext link under point.
If there is no link under point, this will try using
url-get-url-at-point"
  (interactive "e")
  (save-excursion
    (mouse-set-point e)
    (w3-maybe-follow-link)))

;;;###autoload
(defun w3-maybe-follow-link ()
  "Maybe follow a hypertext link under point.
If there is no link under point, this will try using
url-get-url-at-point"
  (interactive)
  (require 'w3)
  (if (not w3-setup-done) (w3-do-setup))
  (let* ((zn  (w3-zone-at (point)))
         (url1 (and zn (w3-zone-data zn)))
         (url2 (url-get-url-at-point)))
    (cond
      (url1 (w3-follow-link))
      ((and url2 (string-match url-nonrelative-link url2)) (w3-fetch url2))
      (t (message "w3-maybe-follow-link got confused.")))))

;;;###autoload
(defun w3-follow-url-at-point-other-frame (&optional pt)
  "Follow the URL under PT, defaults to link under (point)"
  (interactive "d")
  (let ((url (url-get-url-at-point pt)))
    (and url (w3-fetch-other-frame url))))

;;;###autoload
(defun w3-follow-url-at-point (&optional pt)
  "Follow the URL under PT, defaults to link under (point)"
  (interactive "d")
  (let ((url (url-get-url-at-point pt)))
    (and url (w3-fetch url))))

;;;###autoload
(defun w3-batch-fetch ()
  "Fetch all the URLs on the command line and save them to files in
the current directory.  The first argument after the -f w3-batch-fetch
on the command line should be a string specifying how to save the
information retrieved.  If it is \"html\", then the page will be
unformatted when it is written to disk.  If it is \"text\", then the
page will be formatted before it is written to disk.  If it is
\"binary\" it will not mess with the file extensions, and just save
the data in raw binary format.  If none of those, the default is
\"text\", and the first argument is treated as a normal URL."
  (if (not w3-setup-done) (w3-do-setup))
  (if (not noninteractive)
      (error "`w3-batch-fetch' is to be used only with -batch"))
  (let ((fname "")
        (curname "")
	(x 0)
	(args command-line-args-left)
	(w3-strict-width 80)
	(w3-delimit-emphasis nil)
	(w3-delimit-links nil)
	(retrieval-function 'w3-fetch)
	(file-format "text")
	(header "")
	(file-extn ".txt"))
    (setq file-format (downcase (car args)))
    (cond
     ((string= file-format "html")
      (message "Saving all text as raw HTML...")
      (setq retrieval-function 'url-retrieve
	    file-extn ".html"
	    header "<BASE HREF=\"%s\">"
	    args (cdr args)))
     ((string= file-format "binary")
      (message "Saving as raw binary...")
      (setq retrieval-function 'url-retrieve
	    file-extn ""
	    args (cdr args)))
     ((string= file-format "text")
      (setq header "Text from: %s\n---------------\n")
      (message "Saving all text as formatted...")
      (setq args (cdr args)))
     (t
      (setq header "Text from: %s\n---------------\n")
      (message "Going with default, saving all text as formatted...")))
    (while args
      (funcall retrieval-function (car args))
      (goto-char (point-min))
      (if buffer-read-only (toggle-read-only))
      (insert (format header (car args)))
      (setq fname (url-basepath url-current-file t))
      (if (string= file-extn "") nil
	(setq fname (url-file-extension fname t)))
      (if (string= (url-strip-leading-spaces fname) "")
	  (setq fname "root"))
      (setq curname fname)
      (while (file-exists-p (concat curname file-extn))
	(setq curname (concat fname x)
	      x (1+ x)))
      (setq fname (concat curname file-extn))
      (write-region (point-min) (point-max) fname)
      (setq args (cdr args)))))

(defun w3-fix-spaces (x)
  "Remove spaces/tabs at the beginning of a string,
and convert newlines into spaces."
  (url-convert-newlines-to-spaces
   (url-strip-leading-spaces
    (url-eat-trailing-space x))))

(defun w3-reload-all-files ()
  "Reload all w3 files"
  (interactive)
  (setq w3-setup-done nil
	url-setup-done nil
	w3-hotlist nil
	url-mime-accept-string nil)
  (let ((x '(w3 w3-mule w3-e19 mm url w3-xemac w3-toolbar font)))
    (while x
      (setq features (delq (car x) features)
	    x (cdr x)))
    (require 'w3))
  (w3-do-setup)
  (url-do-setup)
  )

(defun w3-source-document-at-point ()
  "View source to the document pointed at by link under point"
  (interactive)
  (w3-source-document t))

(defun w3-my-safe-copy-face (old new locale)
  (let ((fore (face-foreground old))
	(back (face-background old))
	(bpxm (face-background-pixmap old))
	(font (face-font old))
	(font-spec (get old 'font-specification)))
    (if (color-specifier-p fore)
	(setq fore (color-name fore)))
    (if (color-specifier-p back)
	(setq back (color-name back)))
    (if (font-specifier-p font)
	(setq font (font-name font)))
    (set-face-foreground new fore locale)
    (set-face-background new back locale)
    (set-face-background-pixmap new bpxm locale)
    (set-face-font new (or font-spec font) locale)
    new))

(defun w3-source-document (under)
  "View this document's source"
  (interactive "P")
  (let* ((url (if under (w3-view-this-url) (url-view-url t)))
	 (fil (if under nil url-current-file))
	 (tag '$html-source)		; For the stylesheet info
	 (args nil)			; For the stylesheet info
	 (face nil)			; For the stylesheet info
	 (src
	  (cond
	   ((or (null url) (string= url "file:nil"))
	    (error "Not a w3 buffer!"))
	   ((and under (null url)) (error "No link at point!"))
	   ((and (not under) w3-current-source) w3-current-source)
	   (t
	    (prog2
		(url-retrieve url)
		(buffer-string)
	      (setq fil (or fil url-current-file))
	      (kill-buffer (current-buffer))))))
	 (tmp (url-generate-new-buffer-name url)))
    (if (and url (get-buffer url)
	     (funcall url-confirmation-func
		      (concat "Source for " url " found, reuse? ")))
	(progn
	  (if w3-mutable-windows (pop-to-buffer url) (switch-to-buffer url))
	  (setq url nil)))
    (if (not url) nil
      (setq face (or (and w3-current-stylesheet
			  (w3-get-default-style-info 'face))
		     'default))
      (set-buffer (get-buffer-create tmp))
      (insert src)
      (if (fboundp 'valid-specifier-locale-p)
	  (w3-my-safe-copy-face face 'default (current-buffer))
	(w3-add-zone (point-min) (point-max) face nil nil))
      (goto-char (point-min))
      (setq buffer-file-truename fil
	    buffer-file-name fil)
      ;; This taken out because it causes call-process to die a hideous
      ;; death and not let you do anything like M-| lpr in the source
      ;; buffers.
      ;; (setq default-directory (or (file-name-directory fil) "~/"))
      (set-auto-mode)
      (buffer-enable-undo)
      (set-buffer-modified-p nil)
      (if w3-mutable-windows (pop-to-buffer tmp) (switch-to-buffer tmp))))
  (run-hooks 'w3-source-file-hook))

(defun w3-mail-document-under-point ()
  "Mail the document pointed to by the hyperlink under point."
  (interactive)
  (w3-mail-current-document t))

(defun w3-mail-current-document (under &optional format)
  "Mail the current-document to someone"
  (interactive "P")
  (let* ((completion-ignore-case t)
	 (format (or format
		     (completing-read
		      "Format: "
		      '(("HTML Source")
			("Formatted Text")
			("PostScript")
			("LaTeX Source")
			)
		  nil t)))
	 (url (cond
	       ((stringp under) under)
	       (under (w3-view-this-url t))
	       (t (url-view-url t))))
	 (content-type "text/plain; charset=iso-8859-1")
	 (str
	  (save-excursion
	    (cond
	     ((and (equal "HTML Source" format) under)
	      (setq content-type "text/html; charset=iso-8859-1")
	      (let ((url-source t))
		(url-retrieve url)))
	     ((equal "HTML Source" format)
	      (setq content-type "text/html; charset=iso-8859-1")
	      (if w3-current-source
		  (let ((x w3-current-source))
		    (set-buffer (get-buffer-create url-working-buffer))
		    (erase-buffer)
		    (insert x))
		(url-retrieve url)))
	     ((and under (equal "PostScript" format))
	      (setq content-type "application/postscript")
	      (w3-fetch url)
	      (let ((ps-spool-buffer-name " *w3-temp*"))
		(if (get-buffer ps-spool-buffer-name)
		    (kill-buffer ps-spool-buffer-name))
		(w3-print-with-ps-print (current-buffer)
					'ps-spool-buffer-with-faces)
		(set-buffer ps-spool-buffer-name)))
	     ((equal "PostScript" format)
	      (let ((ps-spool-buffer-name " *w3-temp*"))
		(if (get-buffer ps-spool-buffer-name)
		    (kill-buffer ps-spool-buffer-name))
		(setq content-type "application/postscript")
		(w3-print-with-ps-print (current-buffer)
					'ps-spool-buffer-with-faces)
		(set-buffer ps-spool-buffer-name)))
	     ((and under (equal "Formatted Text" format))
	      (setq content-type "text/plain; charset=iso-8859-1")
	      (w3-fetch url))
	     ((equal "Formatted Text" format)
	      (setq content-type "text/plain; charset=iso-8859-1"))
	     ((and under (equal "LaTeX Source" format))
	      (setq content-type "application/x-latex; charset=iso-8859-1")
	      (url-retrieve url)
	      (w3-convert-html-to-latex))
	     ((equal "LaTeX Source" format)
	      (setq content-type "application/x-latex; charset=iso-8859-1")
	      (if w3-current-source
		  (let ((x w3-current-source))
		    (set-buffer (get-buffer-create url-working-buffer))
		    (erase-buffer)
		    (insert x))
		(url-retrieve url))
	      (w3-convert-html-to-latex)))
	    (buffer-string))))
    (cond
     ((and w3-mutable-windows (fboundp w3-mail-other-window-command))
      (funcall w3-mail-other-window-command))
     ((fboundp w3-mail-command)
      (funcall w3-mail-command))
     (w3-mutable-windows (mail-other-window))
     (t (mail)))
    (mail-subject)
    (insert format " from URL " url "\n"
	    "Mime-Version: 1.0\n"
	    "Content-transfer-encoding: 8bit\n"
	    "Content-type: " content-type)

    (re-search-forward mail-header-separator nil)
    (forward-char 1)
    (insert (if (equal "HTML Source" format)
		(format "<BASE HREF=\"%s\">" url) "")
	    str)
    (mail-to)))

(defun w3-internal-use-history (hist-item)
  ;; Go to the link in the history
  (let ((url (nth 0 hist-item))
	(buf (nth 1 hist-item))
	(pnt (nth 2 hist-item)))
    (cond
     ((null buf)			; Find a buffer with same url
      (let ((x (buffer-list))
	    (found nil))
	(while (and x (not found))
	  (save-excursion
	    (set-buffer (car x))
	    (setq found (string= (url-view-url t) url))
	    (if (not found) (setq x (cdr x)))))
	(cond
	 (found
	  (switch-to-buffer (car x))
	  (if (number-or-marker-p pnt) (goto-char pnt)))
	 (t
	  (w3-fetch url)))))
     ((buffer-name buf)			; Reuse the old buffer if possible
      (switch-to-buffer buf)
      (if (number-or-marker-p pnt) (goto-char pnt))
      (if (and url (= ?# (string-to-char url)))	; Destination link
	  (progn
	    (goto-char (point-min))
	    (w3-find-specific-link (substring url 1 nil)))))
     (url (url-maybe-relative url))		; Get the link
     (t (message "Couldn't understand whats in the history.")))))

(defun w3-relative-link (url)
  (if (equal "#" (substring url 0 1))
      (progn
	(push-mark (point) t)
	(goto-char (point-min))
	(w3-find-specific-link (substring url 1 nil)))
    (w3-fetch (url-expand-file-name url))))

(defun w3-maybe-eval ()
  ;; Maybe evaluate a buffer of emacs lisp code
  (if (funcall url-confirmation-func "This is emacs-lisp code, evaluate it?")
      (eval-buffer (current-buffer))
    (emacs-lisp-mode)))

(defun w3-build-continuation ()
  ;; Build a series of functions to be run on this file
  (save-excursion
    (set-buffer url-working-buffer)
    (let ((cont w3-default-continuation)
	  (extn (url-file-extension url-current-file)))
      (if (assoc extn url-uncompressor-alist)
	  (setq extn (url-file-extension
		      (substring url-current-file 0 (- (length extn))))))
      (if w3-source
	  (setq url-current-mime-viewer '(("viewer" . w3-source))))
      (if (not url-current-mime-viewer)
	  (setq url-current-mime-viewer
		(mm-mime-info (or url-current-mime-type
				  (mm-extension-to-mime extn)) nil 5)))
      (if url-current-mime-viewer
	  (setq cont (append cont '(w3-pass-to-viewer)))
	(setq cont (append cont (list w3-default-action))))
      cont)))

(defun w3-use-links ()
  "Select one of the <LINK> tags from this document and fetch it."
  (interactive)
  (and (not w3-current-links)
       (error "No links defined for this document."))
  (let* ((completion-ignore-case t)
	 (type (cond
		((= 0 (length w3-current-links))
		 (error "No links defined for this document."))
		((= 1 (length w3-current-links))
		 (car (car w3-current-links)))
		(t (completing-read "Type of relation: "
				    '(("Parent of") ("Child of"))))))
			      
	 (table (cdr-safe (assoc type w3-current-links))))
    (if (equal type "") (setq type "Parent of"))
    (if table
	(let ((url (cdr (assoc (completing-read (concat type ": ")
						table nil t) table))))
	  (if (string= url "")
	      nil
	    (w3-fetch url)))
      (error "No links found."))))

(defun w3-find-this-file ()
  "Do a find-file on the currently viewed html document if it is a file: or
ftp: reference"
  (interactive)
  (cond
   ((and (or (null url-current-type) (equal url-current-type "file"))
	 (eq major-mode 'w3-mode))
    (if w3-mutable-windows
	(find-file-other-window url-current-file)
      (find-file url-current-file)))
   ((equal url-current-type "ftp")
    (if w3-mutable-windows
	(find-file-other-window
	 (format "/%s@%s:%s" url-current-user url-current-server
		 url-current-file))
      (find-file
       (format "/%s@%s:%s" url-current-user url-current-server
	       url-current-file))))
   (t (message "Sorry, I can't get that file so you can alter it."))))

(defun w3-insert-this-url (pref-arg)
  "Insert the current url in another buffer, with prefix ARG,
insert URL under point"
  (interactive "P")
  (let ((thebuf (get-buffer (read-buffer "Insert into buffer: ")))
	(oldbuf (current-buffer))
	(url (if pref-arg (w3-view-this-url t) (url-view-url t))))
    (if (and url (not (equal "Not on a link!" url)))
	(progn
	  (set-buffer thebuf)
	  (w3-insert url)
	  (set-buffer oldbuf))
      (message "Not on a link!"))))

(defun w3-show-hotlist ()
  "View the hotlist in hypertext form"
  (interactive)
  (if (not w3-setup-done) (w3-do-setup))
  (if (not w3-hotlist)
      (error "Sorry, no hotlist is in memory.")
    (let ((x (url-buffer-visiting "www:/auto/hotlist")))
      (while x
	(kill-buffer x)
	(setq x (url-buffer-visiting "www:/auto/hotlist"))))
    (w3-fetch "www://auto/hotlist")))

(defun url-maybe-relative (url)
  "Take a url and either fetch it, or resolve relative refs, then fetch it"
  (cond
   ((not
     (string-match url-nonrelative-link url))
    (w3-relative-link url))
   (t (w3-fetch url))))

(defun w3-in-assoc (elt list)
  "Check to see if ELT matches any of the regexps in the car elements of LIST"
  (let (rslt)
    (while (and list (not rslt))
      (and (car (car list))
	   (stringp (car (car list)))
	   (not (string= (car (car list)) ""))
	   (string-match (car (car list)) elt)
	   (setq rslt (car list)))
      (setq list (cdr list)))
    rslt))

(defun w3-goto-last-buffer ()
  "Go to last WWW buffer visited"
  (interactive)
  (if w3-current-last-buffer
      (if w3-mutable-windows
	  (pop-to-buffer w3-current-last-buffer)
	(switch-to-buffer w3-current-last-buffer))
    (message "No previous buffer found.")))

(fset 'w3-replace-regexp 'url-replace-regexp)

;;;###autoload
(defun w3-preview-this-buffer ()
  "See what this buffer will look like when its formatted as HTML.
HTML is the HyperText Markup Language used by the World Wide Web to
specify formatting for text.  More information on HTML can be found at
ftp.w3.org:/pub/www/doc."
  (interactive)
  (w3-fetch (concat "www://preview/" (buffer-name))))

(defun w3-edit-source ()
  "Edit the html document just retrieved"
  (set-buffer url-working-buffer)
  (let ((ttl (format "Editing %s Annotation: %s"
		     (cond
		      ((eq w3-editing-annotation 'group) "Group")
		      ((eq w3-editing-annotation 'personal) "Personal")
		      (t "Unknown"))
		     (url-basepath url-current-file t)))
	(str (buffer-string)))
    (set-buffer (get-buffer-create ttl))
    (w3-insert str)
    (kill-buffer url-working-buffer)))

(defun w3-source ()
  "Show the source of a file"
  (let ((tmp (buffer-name (generate-new-buffer "Document Source"))))
    (set-buffer url-working-buffer)
    (kill-buffer tmp)
    (rename-buffer tmp)
    (set-buffer-modified-p nil)
    (buffer-enable-undo)
    (if w3-mutable-windows (pop-to-buffer tmp) (switch-to-buffer tmp))))

(defun w3-sentinel (&optional proc string)
  (set-buffer url-working-buffer)
  (if (or (stringp proc)
	  (bufferp proc)) (setq w3-current-last-buffer proc))
  (if (boundp 'after-change-functions)
      (remove-hook 'after-change-functions 'url-after-change-function))
  (if url-be-asynchronous
      (progn
	(url-clean-text)
	(cond
	 ((not (get-buffer url-working-buffer)) nil)
	 ((url-mime-response-p) (url-parse-mime-headers)))
	(if (not url-current-mime-type)
	    (setq url-current-mime-type (mm-extension-to-mime
					 (url-file-extension
					  url-current-file))))))
  (let ((x (w3-build-continuation))
	(done-mule-conversion nil))
    (while x
      (if (and (boundp 'MULE) (not (eq 'url-uncompress (car x)))
	       (not done-mule-conversion))
	  (progn
	    (w3-convert-code-for-mule url-current-mime-type)
	    (setq done-mule-conversion t)))
      (funcall (car x))
      (setq x (cdr x)))))

(defun w3-show-history-list ()
  "Format the url-history-list prettily and show it to the user"
  (interactive)
  (w3-fetch "www://auto/history"))

(defun w3-save-as (&optional type)
  "Save a document to the local disk"
  (interactive)
  (let* ((completion-ignore-case t)
	 (format (or type (completing-read
			   "Format: "
			   '(("HTML Source") ("Formatted Text")
			     ("LaTeX Source") ("Binary"))
			   nil t)))
	(fname (expand-file-name
		(read-file-name "File name: " default-directory)))
	(url (url-view-url t)))
    (cond
     ((equal "Binary" format)
      (if (not w3-current-source)
	  (let ((url-be-asynchronous nil))
	    (url-retrieve url))))
     ((equal "HTML Source" format)
      (if (not w3-current-source)
	  (let ((url-be-asynchronous nil))
	    (url-retrieve url))		; Get the document if necessary
	(let ((txt w3-current-source))
	  (set-buffer (get-buffer-create url-working-buffer))
	  (insert txt)))
      (goto-char (point-min))
      (insert (format "<BASE HREF=\"%s\">\n" url)))
     ((or (equal "Formatted Text" format)
	  (equal "" format))
      nil)				; Do nothing - we have the text already
     ((equal "LaTeX Source" format)
      (if (not w3-current-source)
	  (let ((url-be-asynchronous nil))
	    (url-retrieve url))		; Get the file
	(let ((txt w3-current-source))
	  (set-buffer (get-buffer-create url-working-buffer))
	  (insert txt)))
      (w3-convert-html-to-latex)))	; Convert to LaTeX
    (write-region (point-min) (point-max) fname)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to parse out <A> tags and replace it with a hyperlink zone
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-popup-info (&optional url)
  "Show information about the link under point. (All SGML attributes)"
  (interactive)
  (let* ((ext (w3-zone-at (point)))
	 (dat (and ext (w3-zone-data ext))))
    (setq url (or url (nth 2 dat))
	  dat (nth 4 dat))
    (if url
	 (save-excursion
	   (set-buffer (get-buffer-create "*Header Info*"))
	   (erase-buffer)
	   (if (and dat (listp dat))
	       (insert
		"Link attributes:\n"
		(make-string (1- (window-width)) ?-) "\n"
		(mapconcat
		 (function
		  (lambda (info)
		    (format "%20s :== %s" (car info) (or (cdr info) "On"))))
		 dat "\n")
		"\n" (make-string (1- (window-width)) ?-) "\n"))
	   (insert (save-excursion (url-popup-info url)))
	   (goto-char (point-min))
	   (display-buffer (current-buffer) t))
      (message "No URL to get information on!"))))

(fset 'w3-document-information-this-url 'w3-popup-info)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for logging of bad HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-reconstruct-tag (tagname desc)
  (concat "<" tagname " "
	  (mapconcat
	   (function (lambda (x)
		       (if (cdr x)
			   (concat (car x) "=\"" (cdr x) "\"")
			 (car x)))) desc " ") ">"))

(defun w3-debug-if-found (regexp type desc)
  (and w3-debug-html
       (save-excursion
	 (if (re-search-forward regexp nil t)
	     (w3-log-bad-html type desc)))))

(defun w3-log-bad-html (type desc)
  ;; Log bad HTML to the buffer specified by w3-debug-buffer
  (if w3-debug-html
      (save-excursion
	(set-buffer (get-buffer-create w3-debug-buffer))
	(goto-char (point-max))
	(insert (make-string (1- (window-width)) w3-horizontal-rule-char) "\n")
	(cond
	 ((stringp type) (insert type "\n" desc "\n"))
	 ((eq type 'bad-quote)
	  (insert "Unterminated quoting character in SGML attribute value.\n"
		  desc "\n"))
	 ((eq type 'no-quote)
	  (insert "Unquoted SGML attribute value.\n" desc "\n"))
	 ((eq type 'no-textarea-end)
	  (insert "Unterminated <textarea> tag.\n"
		  (w3-reconstruct-tag "textarea" desc) "\n"))
	 ((eq type 'bad-link-tag)
	  (insert "Must specify either REL or REV with a <link> tag.\n"
		  (w3-reconstruct-tag "link" desc) "\n"))
	 ((eq type 'no-a-end)
	  (insert "Unterminated <a> tag.\n"
		  (w3-reconstruct-tag "a" desc) "\n"))
	 ((eq type 'no-form-end)
	  (insert "Unterminated <form> tag.\n"
		  (w3-reconstruct-tag "form" desc) "\n"))
	 ((eq type 'bad-base-tag)
	  (insert "Malformed <base> tag.\n"
		  (w3-reconstruct-tag "base" desc) "\n"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to handle formatting an html buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-insert-entities-in-string (str)
  "Remove special characters in STR and replace them with HTML[+] entities"
  (mapconcat
   (function
    (lambda (x)
      (let ((y (rassoc (char-to-string x) w3-html-entities)))
	(or (car-safe y) (char-to-string x))))) str ""))

(defun w3-insert-headers ()
  ;; Insert some HTTP/1.0 headers if necessary
  (url-lazy-message "Inserting HTTP/1.0 headers...")
  (let ((hdrs (if (eq t w3-show-headers) (mapcar 'car url-current-mime-headers)
		w3-show-headers))
	x y)
    (goto-char (setq y (point-max)))
    (while hdrs
      (if (setq x (w3-in-assoc (car hdrs) url-current-mime-headers))
	  (w3-insert "<LI> <B>" (car x) "</B>: " (w3-insert-entities-in-string
						  (if (numberp (cdr x))
						      (int-to-string (cdr x))
						    (cdr x)))))
      (setq hdrs (cdr hdrs)))
    (if (= y (point-max))
	nil
      (w3-insert "</UL>")
      (goto-char y)
      (url-lazy-message "Inserting HTTP/1.0 headers... done.")
      (w3-insert "<HR><UL>"))))

(defun w3-add-delayed-mpeg (src st &optional width height)
  ;; Add a delayed mpeg for the current buffer.
  (setq w3-delayed-movies (cons (list src
				      (set-marker (make-marker) st)
				      width height)
				w3-delayed-movies))
  (w3-handle-text (concat "[MPEG(" (url-basepath src t) ")]"))
  (w3-add-zone st (point) nil (list 'w3mpeg src st)))

(defun w3-add-delayed-graphic (src st align alt)
  ;; Add a delayed image for the current buffer.
  (setq st (set-marker (make-marker) st)
	w3-delayed-images (cons (list src st align alt)
				w3-delayed-images))
  (w3-handle-text alt)
  (if (string= alt "") nil
    (w3-add-zone st (point) nil (list 'w3delayed src st align alt))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shared graphics routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-convert-graphic-to-useable-format (buf fname xbm)
  "Convert the image data in buffer BUF into a format useable by
lemacs or epoch.  Second arg FNAME is the filename to redirect output
into.  If third arg XBM is t, convert it to an Xbitmap, otherwise
convert it to an XPM (recommended, as they can do color).  Returns a
filename containing the bitmap specification"
  (save-excursion
    (set-buffer buf)
    (setq buffer-file-name nil)
    (let (converter)
      (if (not url-current-mime-type)
	  (setq url-current-mime-type (mm-extension-to-mime
				       (url-file-extension url-current-file))))
      (setq converter (assoc url-current-mime-type w3-graphic-converter-alist))
      (if (not converter)
	  (message "Cannot convert %s to www/present!" url-current-mime-type)
	(message "Converting %s (%s)..."
		 (url-basepath url-current-file t) url-current-mime-type)
	(shell-command-on-region
	 (point-min) (point-max)
	 (concat (format (cdr converter)
			 (concat
			  (cond
			   ((null w3-color-use-reducing) "")
			   ((eq w3-color-filter 'ppmquant)
			    (concat "ppmquant " (int-to-string
						 (* w3-color-max-red
						    w3-color-max-green
						    w3-color-max-blue))
				    " | "))
			   ((eq w3-color-filter 'ppmdither)
			    (concat
			     (if w3-ppmdither-is-buggy
				 "pnmdepth 255 | "
			       "")
			     "ppmdither -red "
			     (int-to-string w3-color-max-red)
			     " -green "
			     (int-to-string w3-color-max-green)
			     " -blue "
			     (int-to-string w3-color-max-blue)
			     " | "))
			   ((stringp w3-color-filter)
			    (concat w3-color-filter " | "))
			   (t ""))
			  (if xbm w3-ppmtoxbm-command w3-ppmtoxpm-command)))
		 "> " fname) t)))))

(defun w3-load-flavors ()
  ;; Load the correct zone/font info for each flavor of emacs
  (cond
   ((and w3-running-xemacs (eq system-type 'ms-windows)) (require 'w3-wemac))
   (w3-running-xemacs (require 'w3-xemac))
   (w3-running-epoch  (require 'w3-epoch))
   (w3-running-FSF19  (require 'w3-e19))
   (t                 (require 'w3-emacs)))
  (if (boundp 'MULE) (require 'w3-mule))
  (condition-case ()
      (require 'w3-site-init)
    (error nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Automatic bug submission.                                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-submit-bug ()
  "Submit a bug on Emacs-w3"
  (interactive)
  (require 'reporter)
  (and (yes-or-no-p "Do you really want to submit a bug on Emacs-w3? ")
       (let ((url (url-view-url t))
	     (beta-version (featurep 'w3-beta))
	     (vars '(window-system
		     window-system-version
		     beta-version
		     system-type
		     ange-ftp-version
		     url-gateway-method
		     efs-version
		     ange-ftp-version
		     url-version
		     url-be-asynchronous
		     url)))
	 (if (and url (string= url "file:nil")) (setq url nil))
	 (mapcar
	  (function
	   (lambda (x)
	     (if (not (and (boundp x) (symbol-value x)))
		 (setq vars (delq x vars))))) vars)
	 (reporter-submit-bug-report w3-bug-address
				     (concat "WWW v" w3-version-number " of "
					     w3-version-date)
				     vars
				     nil nil
				     "Description of Problem:"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for searching						    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-nuke-spaces-in-search (x)
  "Remove spaces from search strings . . ."
  (let ((new ""))
    (while (not (equal x ""))
      (setq new (concat new (if (= (string-to-char x) 32) "+"
			      (substring x 0 1)))
	    x (substring x 1 nil)))
    new))

(defun w3-search ()
  "Perform a search, if this is a searchable index."
  (interactive)
  (or w3-current-isindex
      (error "Not a searchable index (via <isindex>)"))
  (let* (querystring			; The string to send to the server
	 (data
	  (cond
	   ((null w3-current-isindex)
	    (let ((rels (mapcar
			 (function
			  (lambda (data)
			    (if (assoc "rel" data) data)))
			 w3-current-links))
		  val)
	      (while rels
		(if (string-match "useindex"
				  (or (cdr (assoc "rel" (car rels))) ""))
		    (setq val (cdr (assoc "href" (car rels)))
			  rels nil))
		(setq rels (cdr rels)))
	      (cons val "Search on (+ separates keywords): ")))
	   ((eq w3-current-isindex t)
	    (cons (url-view-url t) "Search on (+ separates keywords): "))
	   ((consp w3-current-isindex)
	    w3-current-isindex)
	   (t nil)))
	 index)
    (if (null data) (error "Not a searchable index!"))
    (setq index (car data))
    (setq querystring (w3-nuke-spaces-in-search (read-string (cdr data))))
    (if (string-match "\\(.*\\)\\?.*" index)
	(setq index (url-match index 1)))
    (w3-fetch
     (concat index (if (= ?? (string-to-char (substring index -1 nil)))
		       "" "?") querystring))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto documentation, etc                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-help ()
  "Print documentation on w3 mode."
  (interactive)
  (w3-fetch "about:"))

(defun w3-version ()
  "Show the version # of W3 in the minibuffer"
  (interactive)
  (message "WWW %s, URL %s, MM %s" w3-version-number url-version mm-version))

;;;###autoload
(defun w3 ()
  "Retrieve the default World Wide Web home page.
The World Wide Web is a global hypertext system started by CERN in
Switzerland in 1991.

The home page is specified by the variable w3-default-homepage.  The
document should be specified by its fully specified Uniform Resource
Locator.  The document will be parsed as HTML (if appropriate) and
displayed in a new buffer."
  (interactive)
  (if (not w3-setup-done) (w3-do-setup))
  (if (and w3-track-last-buffer
	   (bufferp w3-last-buffer)
	   (buffer-name w3-last-buffer))
      (progn
	(switch-to-buffer w3-last-buffer)
	(message "Reusing buffer.  To reload, type %s."
		 (substitute-command-keys "\\[w3-reload-document]")))
    (cond
     ((null w3-default-homepage) (call-interactively 'w3-fetch))
     ((not (stringp w3-default-homepage))
      (error "Invalid setting for w3-default-homepage: %S"
	     w3-default-homepage))
     ((not (string-match ".*:.*" w3-default-homepage))
      (w3-fetch (concat "file:" w3-default-homepage)))
     (t
      (w3-fetch w3-default-homepage)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Leftover stuff that didn't quite fit into url.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun w3-generate-error (type data)
  ;; Generate an HTML error buffer for error TYPE with data DATA.
  (cond
   ((equal type "nofile")
    (let ((error (save-excursion
		  (set-buffer (get-buffer-create " *url-error*"))
		  (buffer-string))))
      (if (string= "" error)
	  (setq error
		(format (concat "The file %s could not be found.  "
				"Either it does not exist, or it "
				"is unreadable.") data)))
      (insert "<html>\n <head>\n"
	    "  <title>Error</title>\n"
	    " </head>\n <body>\n"
	    "  <h1>Error accessing " data "</h1>\n"
	    "  <hr>\n  <p>"
	    error
	    "\n  </p>\n")))
   ((equal type "nobuf")
    (insert "<title>Error</title>\n"
	    "<H1>No buffer " data " found</h1>\n"
	    "<HR>\n"
	    "The buffer " data " could not be found.  It has either\n"
	    "been killed or renamed.\n"))
   ((equal type "nohist")
    (insert "<TITLE>Error</TITLE>\n"
	    "<H1>No history items found.</H1>\n"
	    "<HR>\n"
	    "There is no history list available at this time.  Either\n"
	    "you have not visited any nodes, or the variable <i>\n"
	    "url-keep-history</i> is nil.\n"))
   )
  (insert "<hr>\n"
	  "If you feel this is a bug, <a href=\"mailto:"
	  w3-bug-address "\">send mail to " w3-bug-address
	  "</a>\n<hr>"))

(defun w3-generate-auto-html (type)
  ;; Generate one of several automatic html pages
  (setq url-current-mime-type "text/html"
	url-current-mime-headers '(("content-type" . "text/html")))
  (cond
   ((equal type "hotlist")
    (let ((tmp (reverse w3-hotlist)))
      (insert "<html>\n\t<head>\n\t\t"
	      "<title> Hotlist </title>\n\t</head>\n"
	      "\t<body>\n\t\t<div1>\n\t\t\t<h1>Hotlist from " w3-hotlist-file
	      "</h1>\n\t\t\t<ol>\n")
      (while tmp
	(insert  "\t\t\t\t<li> <a href=\"" (car (cdr (car tmp)))
		 "\">" (w3-insert-entities-in-string
			(car (car tmp))) "</a></li>\n")
	(setq tmp (cdr tmp)))
      (insert "\n\t\t\t</ol>\n\t\t</div1>\n\t</body>\n</html>"))
    (cond ((boundp 'MULE)
	   (make-variable-buffer-local 'w3-mime-list-for-code-conversion)
	   (setq w3-mime-list-for-code-conversion nil))))
   ((equal type "starting-points")
    (let ((tmp w3-starting-documents))
      (insert "<html>\n\t<head>\n\t\t"
	      "<title> Starting Points </title>\n\t</head>\n"
	      "\t<body>\n\t\t<div1>\n\t\t\t<h1>Starting Point on the Web"
	      "</h1>\n\t\t\t<ol>\n")
      (while tmp
	(insert (format "\t\t\t\t<li> <a href=\"%s\">%s</a></li>\n"
			(car (cdr (car tmp)))
			(car (car tmp))))
	(setq tmp (cdr tmp)))
      (insert "\n\t\t\t</ol>\n\t\t</div1>\n\t</body>\n</html>")))
   ((equal type "history")
    (if (not url-history-list)
	(url-retrieve "www://error/nohist")
      (insert "<html>\n\t<head>\n\t\t"
	      "<title> History List For This Session of W3</title>"
	      "\n\t</head>\n\t<body>\n\t\t<div1>\n\t\t\t<h1>"
	      "History List For This Session of W3</h1>\n\t\t\t<ol>\n")
      (url-maphash
       (function
	(lambda (url desc)
	  (insert (format "\t\t\t\t<li> <a href=\"%s\">%s</a>\n"
			  url (w3-insert-entities-in-string desc)))))
       url-history-list)
      (insert "\n\t\t\t</ol>\n\t\t</div1>\n\t</body>\n</html>")))))

(defun w3-internal-url (url)
  ;; Handle internal urls (previewed buffers, etc)
  (string-match "www:/+\\([^/]+\\)/\\(.*\\)" url)
  (let ((type (url-match url 1))
	(data (url-match url 2)))
    (set-buffer (get-buffer-create url-working-buffer))
    (setq url-current-type "www"
	  url-current-server type
	  url-current-file data)
    (cond
     ((equal type "preview")		; Previewing a document
      (if (get-buffer data)		; Buffer still exists
	  (insert-buffer data)		; Insert the document
	(url-retrieve (concat "www://error/nobuf/" data))))
     ((equal type "error")		; Error message
      (if (string-match "\\([^/]+\\)/\\(.*\\)" data)
	  (w3-generate-error (url-match data 1) (url-match data 2))
	(w3-generate-error data "")))
     ((equal type "auto")		; Hotlist or help stuff
      (w3-generate-auto-html data)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stuff for good local file handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-ff (file)
  "Find a file in any window already displaying it, otherwise just as
display-buffer, and using this function"
  (if (not (eq 'tty (device-type)))
      (let ((f (window-frame (display-buffer (find-file-noselect file)))))
	(set-mouse-position f 1 0)
	(raise-frame f)
	(unfocus-frame))
    (display-buffer (find-file-noselect file))))

(defun w3-default-local-file()
  "Use find-file to open the local file"
  (w3-ff url-current-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode definition							    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-force-reload-document ()
  "Reload the current document.  Take it from the network, even if
cached and in local mode."
  (let ((url-standalone-mode nil))
    (w3-reload-document)))

(defun w3-reload-document ()
  "Reload the current document"
  (interactive)
  (let ((tmp (url-view-url t))
	(pnt (point))
	(window-start (progn
			(move-to-window-line 0)
			(point)))
	(url-request-extra-headers '(("Pragma" . "no-cache"))))
    (kill-buffer (current-buffer))
    (w3-fetch tmp)
    (goto-char pnt)
    (set-window-start (selected-window) (min window-start (point-max)))))

(defun w3-leave-buffer ()
  "Bury this buffer, but don't kill it."
  (interactive)
  (let ((x w3-current-last-buffer))
    (bury-buffer nil)
    (if (and (bufferp x) (buffer-name x))
	(if w3-mutable-windows (pop-to-buffer x) (switch-to-buffer x)))))

(defun w3-quit ()
  "Quit WWW mode"
  (interactive)
  (let ((x w3-current-last-buffer))
    (and (fboundp 'w3-mpeg-kill-processes) (w3-mpeg-kill-processes))
    (kill-buffer (current-buffer))
    (if (and (bufferp x) (buffer-name x))
	(if w3-mutable-windows (pop-to-buffer x) (switch-to-buffer x)))))

(defun w3-view-this-url (&optional no-show)
  "View the URL of the link under point"
  (interactive)
  (let* ((ext (w3-zone-at (point)))
	 (data (and ext (w3-zone-data ext))))
    (cond
     ((eq (car data) 'w3)
      (if (not no-show)
	  (if (nth 2 data)
	      (message "%s" (url-truncate-url-for-viewing (nth 2 data))))
	(nth 2 data)))
     ((eq (car data) 'w3form)
      (if (not no-show)
	  (message "Form entry (name=%s, type=%s)" (nth 3 data)
		   (if (equal "" (nth 2 data)) "TEXT" (nth 2 data))) nil))
     ((eq (car data) 'w3graphic)
      (if (not no-show) (message "Inlined image (%s)" (nth 1 data)) nil))
     (t (if (not no-show) (message "No link at point.")
	  nil)))))

(defun w3-load-delayed-images ()
    "Load inlined images that were delayed, if necessary.
This function searches through `w3-delayed-images' and fetches the
appropriate picture for each point in the buffer and inserts it."
  (interactive)
  (and (fboundp 'w3-insert-graphic)
       (let ((buffer-read-only nil))
	 (mapcar (function (lambda (data) (apply 'w3-insert-graphic data)))
		 (nreverse w3-delayed-images))))
  (setq w3-delayed-images nil))

(defun w3-save-this-url ()
  "Save url under point in the kill ring"
  (interactive)
  (w3-save-url t))

(defun w3-save-url (under-pt)
  "Save current url in the kill ring"
  (interactive "P")
  (let ((x (cond
	    ((stringp under-pt) under-pt)
	    (under-pt (w3-view-this-url t))
	    (t (url-view-url t)))))
    (if x
	(progn
	  (setq kill-ring (cons x kill-ring))
	  (setq kill-ring-yank-pointer kill-ring)
	  (message "Stored URL in kill-ring.")
	  (if (fboundp 'w3-store-in-x-clipboard)
	      (w3-store-in-x-clipboard x)))
      (error "No URL to store."))))

(fset 'w3-end-of-document 'end-of-buffer)
(fset 'w3-start-of-document 'beginning-of-buffer)

(defun w3-scroll-up (&optional lines)
  "Scroll forward in View mode, or exit if end of text is visible.
No arg means whole window full.  Arg is number of lines to scroll."
  (interactive "P")
  (if (and (pos-visible-in-window-p (point-max))
	   ;; Allow scrolling backward at the end of the buffer.
	   (or (null lines)
	       (> lines 0)))
      nil
    (let ((view-lines (1- (window-height))))
      (setq lines
	    (if lines (prefix-numeric-value lines)
	      view-lines))
      (if (>= lines view-lines)
	  (scroll-up nil)
	(if (>= (- lines) view-lines)
	    (scroll-down nil)
	  (scroll-up lines)))
      (cond ((pos-visible-in-window-p (point-max))
	     (goto-char (point-max))
	     (recenter -1)))
      (move-to-window-line -1)
      (beginning-of-line))))

(defun w3-mail-document-author ()
  "Send mail to the author of this document, if possible."
  (interactive)
  (let ((x w3-current-links)
	(y nil)
	(found nil))
    (setq found (cdr-safe (assoc "reply-to" url-current-mime-headers)))
    (if (and found (not (string-match url-nonrelative-link found)))
	(setq found (concat "mailto:" found)))
    (while (and x (not found))
      (setq y (car x)
	    x (cdr x)
	    found (cdr-safe (assoc "made" y))))
    (if found (w3-fetch found)
      (error "Cannot find the 'made' link for this document, sorry."))))

(defun w3-kill-emacs-func ()
  "Routine called when exiting emacs.  Do miscellaneous clean up."
  (and (eq url-keep-history t)
       url-global-history-hash-table
       (url-write-global-history))
  (message "Cleaning up w3 storage...")
  (let ((x (nconc
	    (and (file-exists-p w3-temporary-directory)
		 (directory-files w3-temporary-directory t "url-tmp.*"))
	    (and (file-exists-p url-temporary-directory)
		 (directory-files url-temporary-directory t
				  (concat "url"
					  (int-to-string
					   (user-real-uid)) ".*")))
	    (and (file-exists-p url-temporary-directory)
		 (directory-files url-temporary-directory t "url-tmp.*")))))
    (while x
      (condition-case ()
	  (delete-file (car x))
	(error nil))
      (setq x (cdr x))))
  (message "Cleaning up w3 storage... done.")
  (and w3-old-kill-emacs-hook (funcall w3-old-kill-emacs-hook)))

(cond
 ((fboundp 'display-warning)
  (fset 'w3-warn 'display-warning))
 ((fboundp 'warn)
  (defun w3-warn (class message &optional level)
    (if (and (eq class 'html)
	     (not w3-debug-html))
	nil
      (warn "(%s/%s) %s" class (or level 'warning) message))))
 (t
  (defun w3-warn (class message &optional level)
    (if (and (eq class 'html)
	     (not w3-debug-html))
	nil
      (save-excursion
	(set-buffer (get-buffer-create "*W3-WARNINGS*"))
	(goto-char (point-max))
	(save-excursion
	  (insert (format "(%s/%s) %s\n" class (or level 'warning) message)))
	(display-buffer (current-buffer)))))))

(defun w3-do-setup ()
  "Do setup - this is to avoid conflict with user settings when W3 is
dumped with emacs."
  (url-do-setup)
  (url-register-protocol 'about 'w3-about 'url-identity-expander)
  (url-register-protocol 'www 'w3-internal-url 'url-identity-expander)
	       
  (setq w3-netscape-configuration-file
	(cond
	 (w3-netscape-configuration-file
	  w3-netscape-configuration-file)
	 ((memq system-type '(ms-dos ms-windows))
	  (expand-file-name "~/NETSCAPE.CFG"))
	 (t (expand-file-name "~/.MCOM-preferences"))))

  (if (eq w3-user-colors-take-precedence 'guess)
      (progn
	(setq w3-user-colors-take-precedence (and
					      (not (eq (device-type) 'tty))
					      (not (eq (device-class) 'mono))))
	(w3-warn
	 'html
	 "Disabled document color specification because of mono display.")))

  (setq w3-default-stylesheet
	(cond
	 (w3-default-stylesheet w3-default-stylesheet)
	 ((memq system-type '(ms-dos ms-windows os2))
	  (expand-file-name "~/w3.sty"))
	 (t
	  (expand-file-name "~/.w3.sty"))))

  (if (not (string-match url-nonrelative-link w3-default-stylesheet))
      (if (and (file-exists-p w3-default-stylesheet)
	       (file-readable-p w3-default-stylesheet))
	  (setq w3-default-stylesheet (concat "file:"
					      (if (= ?/
						     (string-to-char
						      w3-default-stylesheet))
						  ""
						"/") w3-default-stylesheet))
	(setq w3-default-stylesheet nil)))
  
  (if w3-default-stylesheet
      (setq w3-user-stylesheet
	    (w3-parse-arena-style-sheet w3-default-stylesheet)))

  (if (and w3-use-netscape-configuration-file
	   w3-netscape-configuration-file
	   (fboundp 'w3-read-netscape-config))
      (w3-read-netscape-config w3-netscape-configuration-file))
      
  (setq w3-default-configuration-file
	(cond
	 (w3-default-configuration-file w3-default-configuration-file)
	 ((memq system-type '(ms-dos ms-windows))
	  (expand-file-name "~/w3.ini"))
	 (t (expand-file-name "~/.w3"))))

  (if (and w3-default-configuration-file
	   (file-exists-p w3-default-configuration-file)
	   (file-readable-p w3-default-configuration-file))
      (load-file w3-default-configuration-file))

  (if (not (assq 'w3-annotation-minor-mode minor-mode-alist))
      (setq minor-mode-alist (cons '(w3-annotation-minor-mode " Annotating")
				   minor-mode-alist)))
  (if (and (boundp 'minor-mode-map-alist)
	   (not (assq 'w3-annotation-minor-mode minor-mode-map-alist)))
      (setq minor-mode-map-alist (cons (cons 'w3-annotation-minor-mode
					     w3-annotation-minor-mode-map)
				       minor-mode-map-alist)))
  (setq url-package-version w3-version-number
	url-package-name "Emacs-W3")

  (w3-load-flavors)
  (w3-setup-version-specifics)
  ; Create the fonts, etc in windowing systems
  (w3-create-faces)

  (if (and (not w3-delay-image-loads)
	   (fboundp 'w3-insert-graphic)
	   (not (w3-executable-exists-in-path "ppmtoxpm"))
	   (not (or
		 (w3-executable-exists-in-path "pbmtoxbm")
		 (w3-executable-exists-in-path "ppmtoxbm"))))
      (w3-warn
       'image
       (concat
	"Could not find some vital ppm utilities in exec-path.\n"
	"This probably means that you will be unable to view any\n"
	"inlined images other than X-Bitmaps or X-Pixmaps, which are\n"
	"now rarely used on the World Wide Web.\n\n"
	"If you do not have the PPM utilities from either the PBMPLUS\n"
	"or NETPBM distributions installed on your machine, then\n"
	"please set the variable `w3-delay-image-loads' to t with a\n"
	"line like:\n\n"
	"\t(setq w3-delay-image-loads t)\n\n"
	"in your ~/.emacs file.  Or as an alternative, please modify\n"
	"the `w3-graphic-converter-alist' variable to disallow all images\n"
	"but XBMs and XPMs, like so:\n\n"
	"\t(setq w3-graphic-converter-alist\n"
	"\t\t'(\n"
	"\t\t\t(\"image/x-xbitmap\" . \"cat \")\n"
	"\t\t\t(\"image/xbitmap\" . \"cat \")\n"
	"\t\t\t(\"image/xbm\" . \"cat \")\n"
	"\t\t\t(\"image/x-xpixmap\" . \"cat \")\n"
	"\t\t))\n\n"
	"You can find the NETPBM utilities in:\n"
	"\tftp://ftp.cs.indiana.edu/pub/elisp/w3/images/\n"
	)))

  (if (eq w3-color-use-reducing 'guess)
      (setq w3-color-use-reducing
	    (cond
	     ((eq (device-type) 'tty) nil)
	     ((fboundp 'device-class)
	      (not (and (memq (device-class) '(TrueColor true-color))
			(<= 16 (or (device-bitplanes) 0)))))
	     (t t))))
		   
  (cond
   ((memq system-type '(ms-dos ms-windows))
    (setq w3-documents-menu-file (or w3-documents-menu-file
				     (expand-file-name "~/mosaic.mnu"))
	  w3-hotlist-file (or w3-hotlist-file
			      (expand-file-name "~/mosaic.hot"))
	  w3-personal-annotation-directory (or w3-personal-annotation-directory
					       (expand-file-name
						"~/mosaic.ann"))))
   ((memq system-type '(axp-vms vax-vms))
    (setq w3-documents-menu-file
	  (or w3-documents-menu-file
	      (expand-file-name "decw$system_defaults:documents.menu"))
	  w3-hotlist-file (or w3-hotlist-file
			      (expand-file-name "~/mosaic.hotlist-default"))
	  w3-personal-annotation-directory
	  (or w3-personal-annotation-directory
	      (expand-file-name "~/mosaic-annotations/"))))
   (t 
    (setq w3-documents-menu-file
	  (or w3-documents-menu-file
	      (expand-file-name "/usr/local/lib/mosaic/documents.menu"))
	  w3-hotlist-file (or w3-hotlist-file
			      (expand-file-name "~/.mosaic-hotlist-default"))
	  w3-personal-annotation-directory
	  (or w3-personal-annotation-directory
	      (expand-file-name "~/.mosaic-personal-annotations")))))
  
  ; Set up delimiting based on device-type and value of
  ; w3-emacs19-hack-faces-p
  (if (eq w3-delimit-emphasis 'guess)
      (setq w3-delimit-emphasis
	    (and (not w3-running-xemacs)
		 (not w3-running-epoch)
		 (not (and w3-running-FSF19
			   (or
			    (memq (device-type) '(x ns pm))
			    w3-emacs19-hack-faces-p))))))

  (if (eq w3-delimit-links 'guess)
      (setq w3-delimit-links
	    (and (not w3-running-xemacs)
		 (not w3-running-epoch)
		 (not (and w3-running-FSF19
			   (or (memq (device-type) '(x ns pm))
			       w3-emacs19-hack-faces-p))))))

  ; Set up a hook that will save the history list when
  ; exiting emacs
  (if (or w3-running-xemacs w3-running-FSF19)
      (add-hook 'kill-emacs-hook 'w3-kill-emacs-func)
    (setq w3-old-kill-emacs-hook kill-emacs-hook
	  kill-emacs-hook 'w3-kill-emacs-func))

  (mm-parse-mailcaps)
  (mm-parse-mimetypes)

  ; Load in the hotlist if they haven't set it already
  (or w3-hotlist (w3-parse-hotlist))

  ; Load in their personal annotations if they haven't set them already
  (or w3-personal-annotations (w3-parse-personal-annotations))

  ; Set the default home page, honoring their defaults, then
  ; the standard WWW_HOME, then default to the documentation @ IU
  (or w3-default-homepage
      (setq w3-default-homepage
	    (or (getenv "WWW_HOME")
		"http://www.cs.indiana.edu/elisp/w3/docs.html")))

  ; Set up the documents menu
  (w3-parse-docs-menu)

  ; Set up the entity definition for PGP and PEM authentication

  (run-hooks 'w3-load-hook)
  (setq w3-setup-done t))

(defun w3-mark-link-as-followed (ext dat)
  ;; Mark a link as followed, by removing the old extent EXT, and replacing
  ;; it with a new extent with the w3-visited-node-style face.
  (if (not w3-emacs19-hack-faces-p)
      (let ((st (w3-zone-start ext))
	    (nd (w3-zone-end ext)))
	(w3-delete-zone ext)
	(w3-add-zone st nd w3-visited-node-style dat t)
	(if w3-delimit-links
	    (progn
;;;      (goto-char nd)
;;;      (delete-region nd (- nd (length (car w3-link-end-delimiter))))
;;;      (insert (cdr w3-link-end-delimiter))
;;;      (goto-char st)
;;;      (delete-region st (+ st (length (car w3-link-start-delimiter))))
;;;      (insert (cdr w3-link-start-delimiter))
	  )))))

(defun w3-download-url (url)
  (let ((url-be-asynchronous nil)
	(url-inhibit-uncompression t))
    (url-retrieve url)
    (if (get-buffer url-working-buffer)
	(w3-save-binary-file))))

;;;###autoload
(defun w3-follow-link-other-frame (&optional p)
  "Attempt to follow the hypertext reference under point in a new frame.
With prefix-arg P, ignore viewers and dump the link straight
to disk."
  (cond
   ((and (fboundp 'make-frame)
	 (fboundp 'select-frame))
    (let ((frm (make-frame)))
      (select-frame frm)
      (w3-follow-link p)))
   (t (w3-follow-link p))))

;;;###autoload
(defun w3-follow-link (&optional p)
  "Attempt to follow the hypertext reference under point.
With prefix-arg P, ignore viewers and dump the link straight
to disk."
  (interactive "P")
  (let* ((ext (w3-zone-at (point)))
	 (dat (and ext (w3-zone-data ext))))
    (cond
     ((null dat) (message "No link, form entry, or image at point."))
     ((and (or p w3-dump-to-disk) (eq (car dat) 'w3))
      (if (stringp (nth 2 dat))
	  (w3-download-url (nth 2 dat))))
     ((eq (car dat) 'w3)
      (let ((buffer-read-only nil))
	(w3-mark-link-as-followed ext dat))
      (if (stringp (nth 2 dat)) (w3-fetch (nth 2 dat)) (message "No link.")))
     ((eq (car dat) 'w3form) (w3-do-form-entry dat ext))
     ((eq (car dat) 'w3graphic) (w3-fetch (nth 1 dat)))
     ((eq (car dat) 'w3expandlist) (w3-expand-list dat))
     ((eq (car dat) 'w3delayed)
      (apply 'w3-load-single-delayed-graphic
	     (w3-zone-start ext) (w3-zone-end ext) (cdr dat))
      (w3-delete-zone ext))
     ((eq (car dat) 'w3mpeg)
      (apply 'w3-load-single-delayed-mpeg
	     (w3-zone-start ext) (w3-zone-end ext) (cdr dat)))
     (t (message "Confused about what type of link is at point: %s" (car dat)))
     )))

(defun w3-complete-link ()
  "Choose a link from the current buffer and follow it"
  (interactive)
  (let (links-alist
	link-at-point
	choice
	(completion-ignore-case t))
    (setq link-at-point (w3-zone-at (point))
	  link-at-point (and
			 link-at-point
			 (eq 'w3 (car-safe (w3-zone-data link-at-point)))
			 (nth 2 (w3-zone-data link-at-point))
			 (w3-fix-spaces
			  (buffer-substring (w3-zone-start link-at-point)
					    (w3-zone-end link-at-point)))))
    (w3-map-links (function
		   (lambda (data st nd arg)
		     (if (and (nth 2 data)
			      (not (equal "" (nth 2 data))))
			 (setq links-alist (cons
					    (cons
					     (w3-fix-spaces
					      (buffer-substring st nd))
					     (nth 2 data)) links-alist))))))
    (if (not links-alist) (error "No links in current document."))
    (setq links-alist (sort links-alist (function
					 (lambda (x y)
					   (string< (car x) (car y))))))
    ;; Destructively remove duplicate entries from links-alist.
    (let ((remaining-links links-alist))
      (while remaining-links
	(if (equal (car remaining-links) (car (cdr remaining-links)))
	    (setcdr remaining-links (cdr (cdr remaining-links)))
	  (setq remaining-links (cdr remaining-links)))))
    (setq choice (completing-read
		  (if link-at-point
		      (concat "Link (default "
			      (if (< (length link-at-point) 20)
				  link-at-point
				(concat
				 (substring link-at-point 0 17) "..."))
			      "): ")
		    "Link: ") links-alist nil t))
    (if (string= choice "")
	(w3-follow-link)
      (w3-fetch (cdr (assoc choice links-alist))))))

(defun w3-mode ()
  "Mode for viewing HTML documents.  If called interactively, will
display the current buffer as HTML.

Current keymap is:
\\{w3-mode-map}"
  (interactive)
  (or w3-setup-done (w3-do-setup))
  (if (interactive-p)
      (w3-preview-this-buffer)
    (let ((tmp (mapcar (function (lambda (x) (cons x (symbol-value x))))
		       w3-persistent-variables)))
      (kill-all-local-variables)
      (use-local-map w3-mode-map)
      (setq major-mode 'w3-mode)
      (setq mode-name "WWW")
      (mapcar (function (lambda (x) (set-variable (car x) (cdr x)))) tmp)
      (run-hooks 'w3-mode-hook)
      (w3-mode-version-specifics)
      (setq url-current-passwd-count 0
	    mode-line-format w3-modeline-format)
      (if (and w3-current-isindex (equal url-current-type "http"))
	  (setq mode-line-process "-Searchable")))))

(require 'mm)
(require 'url)
(require 'url-hash)
(require 'w3-beta)
(require 'w3-parse)
(require 'w3-draw)
(require 'w3-style)
(require 'w3-print)
(require 'w3-about)
(require 'w3-hot)
(provide 'w3)
