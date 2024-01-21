;;; w3-beta.el,v --- Misc functions for emacs-w3's new display engine
;; Author: wmperry
;; Created: 1995/10/15 23:01:25
;; Version: 1.49
;; Keywords: help, hypermedia, comm

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

(defun w3-parse-header-link-items ()
  ;; Parse `url-current-mime-headers' and look for any <link> items
  (let ((items url-current-mime-headers)
	(node nil)
	(url nil)
	(type nil)
	(args nil)
	(title nil)
	(label nil))
    (while items
      (setq node (car items)
	    items (cdr items))
      (if (string= (car node) "link")
	  (progn
	    (setq args (mm-parse-args (cdr node))
		  type (if (assoc "rel" args) "rel" "rev")
		  label (cdr-safe (assoc type args))
		  title (cdr-safe (assoc "title" args))
		  url (car-safe (rassoc nil args)))
	    (if (string-match "^<.*>$" url)
		(setq url (substring url 1 -1)))
	    (and url label type
		 (w3-handle-link (list (cons "href" url)
				       (cons type label)
				       (cons "title" title)))))))))
     
(defun w3-refresh-buffer (&rest args)
  "Redraw the current buffer - this does not refetch or reparse the current
document, but uses the stored parse data."
  (interactive)
  (let ((buffer-read-only nil))
    (if (get-buffer url-working-buffer)
	(kill-buffer url-working-buffer))
    (erase-buffer)
    (rename-buffer url-working-buffer)
    (setq w3-delayed-images nil
	  w3-current-links nil
	  w3-delayed-movies nil)
    (w3-parse-header-link-items)
    (w3-draw-html w3-current-parse)
    (w3-show-buffer)))

(defun w3-prepare-buffer (&rest args)
  ;; The text/html viewer - does all the drawing and displaying of the buffer
  ;; that is necessary to go from raw HTML to a good presentation.
  (let ((active-minibuffer-window
	 (if (minibuffer-window-active-p (minibuffer-window))
	     (minibuffer-window)))
	(pop-up-windows nil))
    (if active-minibuffer-window
	(let* ((current-buffer (current-buffer))
	       (window (get-buffer-window current-buffer t)))
	  (cond (window
		 (and (fboundp 'select-frame)
		      (fboundp 'window-frame)
		      (select-frame (window-frame window)))
		 (select-window window))
		((and (fboundp 'selected-frame)
		      (fboundp 'window-frame)
		      (eq (selected-frame) (window-frame (minibuffer-window))))
		 ;; on minibuffer-only-frame
		 (select-frame (previous-frame))
		 (select-window (frame-first-window (selected-frame))))
		((fboundp 'frame-first-window)
		 (select-window (frame-first-window))))
	  (set-buffer current-buffer)))
    (let* ((source (buffer-string))
	   (parse (w3-preparse-buffer (current-buffer)))
	   (buff (car parse)))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer))
      (set-buffer buff)
      (setq w3-current-source source
	    w3-current-parse (cdr parse))
      (w3-parse-header-link-items)
      (save-excursion
	(goto-char (point-max))
	(w3-handle-paragraph)
	(w3-mode)
	(w3-handle-annotations)
	(w3-handle-headers)
	(if (boundp 'MULE) (w3-mule-attribute-zones w3-zones-list))
	(set-buffer-modified-p nil)
	(setq buffer-read-only t))
      (switch-to-buffer (current-buffer))
      (or active-minibuffer-window
	  (let ((window nil)
		(pop-up-windows nil))
	    (display-buffer (current-buffer))
	    (if (or w3-running-FSF19 w3-running-xemacs)
		(setq window (get-buffer-window (current-buffer) t))
	      (setq window (get-buffer-window (current-buffer))))
	    (select-window window)
	    (if (and (fboundp 'select-frame)
		     (fboundp 'window-frame))
		(select-frame (window-frame window)))))
      ;; from MULE contributors
      (and w3-auto-image-alt (w3-show-invisible-href))
      (goto-char (point-min))
      (w3-show-buffer)
      (if url-keep-history
	  (let ((url (url-view-url t)))
	    (if (not (url-hashtablep url-history-list))
		(setq url-history-list (url-make-hashtable 131)))
	    (url-puthash url (buffer-name) url-history-list)
	    (if (fboundp 'w3-shuffle-history-menu)
		(w3-shuffle-history-menu)))))
    (cond (active-minibuffer-window
	   (select-window active-minibuffer-window)
	   (sit-for 0)))))

;;; from MULE contributors
(defun w3-show-graphics ()
  "Displaying inlined image indicator by text."
  (interactive)
  (let ((w3-auto-image-alt (or w3-auto-image-alt t)))
    (w3-refresh-buffer))
  (goto-char (point-min)))

(defun w3-handle-headers ()
  ;; Insert any headers the user wants to see into the current buffer.
  (let ((show w3-show-headers)
	(cur nil)
	(hdrs nil)
	(tag 'ol)
	(header nil)
	(w3-last-fill-pos (point-max))
	(val nil)
	(first t))
    (goto-char (point-max))
    (if (eq show t) (setq show '(".*")))
    (while show
      (setq cur (car show)
	    show (cdr show)
	    hdrs url-current-mime-headers)
      (while hdrs
	(setq header (car (car hdrs))
	      val (cdr (car hdrs))
	      hdrs (cdr hdrs))
	(if (numberp val) (setq val (int-to-string val)))
	(if (and (/= 0 (length header))
		 (string-match cur header))
	    (progn
	      (if first
		  (progn
		    (w3-handle-hr)
		    (w3-handle-list-opening '(("value" . 1)))
		    (setq tag 'li
			  first nil)))
	      (w3-handle-list-item)
	      (w3-handle-text (concat (capitalize header)
				      ": " val))))))
    (if (not first)			; We showed some headers
	(setq tag '/ol
	      tag (w3-handle-list-ending)))))

(defun w3-handle-annotations ()
  ;; Insert personal annotations into the current buffer
  (let ((annos (w3-fetch-personal-annotations))
	(tag nil))
    (if (not annos)
	nil				; No annotations
      (goto-char (cond
		  ((eq w3-annotation-position 'bottom) (point-max))
		  ((eq w3-annotation-position 'top) (point-min))
		  (t (message "Bad value for w3-annotation-position")
		     (point-max))))
      (w3-handle-div '((align . "center")))
      (w3-handle-hr '((width . "50%")))
      (setq tag 'h3)
      (w3-handle-header)
      (w3-handle-text "Personal Annotations")
      (setq tag '/h3)
      (w3-handle-header-end)
      (setq tag 'ol)
      (w3-handle-list-opening)
      (while annos
	(w3-handle-list-item)
	(w3-handle-hyperlink (list (cons 'href (car (car annos)))))
	(w3-handle-text (cdr (car annos)))
	(w3-handle-hyperlink-end)
	(setq annos (cdr annos)))
      (w3-handle-list-ending)
      (w3-handle-hr '((width . "50%")))
      (w3-handle-/div)
      )))

(defun w3-fetch-personal-annotations ()
  ;; Grab any personal annotations for the current url
  (let ((url  (url-view-url t))
	(anno w3-personal-annotations)
	(annolist nil))
    (if (assoc url anno)
	(while anno
	  (if (equal (car (car anno)) url)
	      (setq annolist
		    (cons
		     (cons
		      (format "file:%s%s/PAN-%s.html"
			      (if (= ?/ (string-to-char
					 w3-personal-annotation-directory)) ""
				"/")
			      w3-personal-annotation-directory
			      (car (car (cdr (car anno)))))
		      (car (cdr (car (cdr (car anno))))))
		     annolist)))
	  (setq anno (cdr anno))))
    annolist))

(defvar w3-netscape-FAT-file "index"
  "*Filename in a netscape cache directory.")

(defvar w3-netscape-FAT-tag
  "MCOM-Cache-file-allocation-table-format-1"
  "*The line at the beginning of a netscape cache file.")

(defmacro w3-skip-word ()
  (skip-chars-forward "^ \t\n\r")
  (skip-chars-forward " \t"))

(defun w3-read-netscape-config (&optional fname)
  "Read in a netscape-style configuration file."
  (interactive "fNetscape configuration file: ")
  (if (not (and (file-exists-p fname)
		(file-readable-p fname)))
      (error "Could not read %s" fname))
  (let ((results nil)
	(tag nil)
	(val nil)
	(var nil)
	(save-pos nil))
    (save-excursion
      (set-buffer (get-buffer-create " *w3-tmp*"))
      (erase-buffer)
      (mm-insert-file-contents fname)
      (goto-char (point-min))
      (skip-chars-forward "^ \t\r\n")	; Skip tag line
      (skip-chars-forward " \t\r\n")	; Skip blank line(s)
      (while (not (eobp))
	(setq save-pos (point))
	(skip-chars-forward "^:")
	(upcase-region save-pos (point))
	(setq tag (buffer-substring save-pos (point)))
	(skip-chars-forward ":\t ")
	(setq save-pos (point))
	(skip-chars-forward "^\r\n")
	(setq val (if (= save-pos (point))
		      nil
		    (buffer-substring save-pos (point))))
	(cond
	 ((null val) nil)
	 ((string-match "^[0-9]+$" val)
	  (setq val (string-to-int val)))
	 ((string= "false" (downcase val))
	  (setq val nil))
	 ((string= "true" (downcase val))
	  (setq val t))
	 (t nil))
	(skip-chars-forward " \t\n\r")
	(setq results (cons (cons tag val) results))))
    (while results
      (setq tag (car (car results))
	    val (cdr (car results))
	    var (cdr-safe (assoc tag w3-netscape-variable-mappings))
	    results (cdr results))
      (cond
       ((eq var 'w3-delay-image-loads) (set var (not val)))
       (var (set var val))
       (t nil)))))
      
(defun w3-import-netscape-cache (dir)
  "Read in a Netscape-file cache directory and convert it to the Emacs-w3
format."
  (interactive "DNetscape cache directory: ")
  (let ((fname (expand-file-name w3-netscape-FAT-file dir))
	(netscape-name nil)		; Netscape cache name
	(url nil)			; Original URL
	(type nil)			; Content-type of URL
	(length nil)			; Content-length of URL
	(save-pos nil)			; Temporary point storage
	(w3-name nil)			; Emacs-w3 cached name
	(w3-hdrs nil)			; Header file
	)
    (if (not (and (file-exists-p fname) (file-readable-p fname)))
	(error "%s is not readable..." w3-netscape-FAT-file))
    (set-buffer (get-buffer-create " *w3-tmp*"))
    (erase-buffer)
    (mm-insert-file-contents fname)
    (goto-char (point-min))
    (if (not (looking-at (concat "^" w3-netscape-FAT-tag "\r*$")))
	(error "%s is not a netscape FAT table..." w3-netscape-FAT-file))
    (forward-line 1)			; Skip tag line
    (while (not (eobp))
      (w3-skip-word)  (w3-skip-word) (w3-skip-word)
      (setq save-pos (point))
      (w3-skip-word)
      (setq netscape-name (expand-file-name
			   (w3-fix-spaces (buffer-substring save-pos (point)))
			   dir)
	    save-pos (point))
      (w3-skip-word)
      (setq url (w3-fix-spaces (buffer-substring save-pos (point)))
	    save-pos (point))
      (w3-skip-word)
      (setq type (w3-fix-spaces (buffer-substring save-pos (point)))
	    save-pos (point))
      (w3-skip-word)
      (setq length (w3-fix-spaces (buffer-substring save-pos (point))))
      (skip-chars-forward " \t\r\n")
      (setq w3-name (url-create-cached-filename url)
	    w3-hdrs (url-generic-parse-url url)
	    w3-hdrs (format "(setq url-current-content-length \"%s\"
      url-current-mime-type \"%s\"
      url-current-type \"%s\"
      url-current-user \"%s\"
      url-current-server \"%s\"
      url-current-port \"%s\"
      url-current-file \"%s\"
      url-current-mime-headers '((\"content-type\" . \"%s\")
				 (\"content-length\" . \"%s\")))"
			    length type
			    (url-type w3-hdrs)
			    (url-user w3-hdrs)
			    (url-host w3-hdrs)
			    (url-port w3-hdrs)
			    (url-filename w3-hdrs)
			    type length))
      (condition-case ()
	  (make-directory (url-basepath w3-name) t)
	(error nil))
      (write-region w3-hdrs nil (concat (url-file-extension w3-name t)
					".hdr"))
      (condition-case ()
	  (copy-file netscape-name w3-name t)
	(error nil)))))

(if (not (fboundp 'abs))
    (defun abs (arg)
      "Return the absolute value of ARG."
      (if (< 0 arg) (- arg) arg)))

(defvar w3-directory "/usr/local/lib/emacs/site-lisp/w3")

(defun w3-install-latest ()
  "Install the latest version of the W3 world wide web browser."
  (interactive)
  (let* (
	 (sorted-tar-files
	  ;; This sort fails when the length of the version number changes!
	  ;; But Bill P. hereby promises not to let that happen. :)
	  (sort (delq nil
		      (mapcar (function
			       (lambda (filename)
				 (let ((len (length filename)))
				   (and (> len 7)
					(string=
					 ".tar.gz"
					 (substring filename
						    (- len 7) len))
					filename))))
			      (file-name-all-completions
			       "w3-" "/anonymous@cs.indiana.edu:/pub/elisp/w3/")))
		(function string<)))
	 (tar-file
	  (nth (1- (length sorted-tar-files)) sorted-tar-files))
	 (version-string (substring tar-file 3 (- (length tar-file) 7)))
	 (new-directory
	  (concat "w3-" version-string))
	 (default-directory
	   (concat "~/emacs/site-lisp")))
    (if (file-exists-p (concat default-directory "/" new-directory))
	(error "W3 version %s has already been installed." version-string))
    ;; We don't use /tmp because it might be on a different filesystem, so it
    ;; couldn't just be renamed.
    (make-directory "w3-tmp")
    (copy-file (concat "/anonymous@cs.indiana.edu:/pub/elisp/w3/" tar-file)
	       "w3-tmp")
    ;; This doesn't work right without "default-directory" in the cd.
    (shell-command (concat "cd " default-directory "/w3-tmp; "
			   "gunzip -qc " tar-file " | tar xvf -"))
    (shell-command (concat "cd " default-directory "/w3-tmp/w3; cp w3.info* /usr/local/info"))
    (rename-file "w3-tmp/w3" new-directory)
    (delete-file (concat "w3-tmp/" tar-file))
    (delete-directory "w3-tmp")
    (delete-file "w3")			; get rid of the old link
    ;; This doesn't work right without the cd.
    (shell-command (concat "cd " default-directory "; ln -s " new-directory " w3" ))
    (shell-command (concat "cd " default-directory "/w3; etags *.el"))))

(provide 'w3-beta)
