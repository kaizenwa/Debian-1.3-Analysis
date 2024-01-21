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
;;; Structure for hotlists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (
;;;  ("name of item1" . "http://foo.bar.com/")    ;; A single item in hotlist
;;;  ("name of item2" . (                         ;; A sublist
;;;                      ("name of item3" . "http://www.ack.com/")
;;;                     ))
;;; )  ; end of hotlist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hotlist Handling Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-read-html-bookmarks (fname)
  "Import an HTML file into the Emacs-w3 format."
  (interactive "fBookmark file: ")
  (if (not (file-readable-p fname))
      (error "Can not read %s..." fname))
  (save-excursion
    (set-buffer (get-buffer-create " *bookmark-work*"))
    (erase-buffer)
    (mm-insert-file-contents fname)
    (let ((parse (w3-preparse-buffer (current-buffer) t))
	  (booklist nil))
      booklist)))

(defun w3-draw-air-hotlist-as-html (hotlist-data)
  ;; Draw an AIR-Mosaic style hotlist as HTML.
  (set-buffer (get-buffer-create url-working-buffer))
  (erase-buffer)
  (insert "<html>\n\t<head>\n\t\t"
	  "<title> Hotlist </title>\n\t</head>\n"
	  "\t<body>\n\t\t<div1>\n\t\t\t<h1>Hotlist data"
	  "</h1>\n\t\t\t<ul>\n")
  (w3-draw-air-sublist hotlist-data)
  (insert "\t\t\t</ul>\n\t\t</div1>\n\t</body>\n</html>\n"))

(defun w3-draw-air-sublist (data)
  ;; Draw a sublist of an AIR Mosaic style hotlist
  (let ((ttl (car data)))
    (setq data (cdr (cdr data)))
    (insert "<ul>\n"
	    " <li> " ttl "\n"
	    "  <ul>\n")
    (while data
      (cond
       ((and (listp (car data)) (null (nth 1 (car data))))
	(w3-draw-air-sublist (car data)))
       ((listp (car data))
	(insert "    <li> <a href=\""
		(nth 1 (car data)) "\">"
		(nth 0 (car data)) "</a></li>\n"))
       (t 'undefined))
      (setq data (cdr data)))
    (insert "  </ul>\n"
	    " </li>\n"
	    "</ul>\n")))

(defun w3-parse-air-hotlist (&optional fname)
  ;; Read in an AIR-Mosaic style hotlist and parse it.
  (if (not fname) (setq fname w3-hotlist-file))
  (setq w3-hotlist nil)
  (if (not (and (file-exists-p fname) (file-readable-p fname)))
      (message "%s does not exist!" fname)
    (save-excursion
      (set-buffer (get-buffer-create " *w3-temp*"))
      (erase-buffer)
      (mm-insert-file-contents fname)
      (goto-char (point-min))
      (if (not (looking-at "^Hotlist\r*$"))
	  (error "%s is not in hotlist format!" fname))
      (url-replace-regexp "^[ \t]*{[ \t\r]*$" "(")
      (url-replace-regexp "^[ \t]*}[ \t\r]*$" ")")
      (url-replace-regexp "^Hotlist\r*$" "(")
      (url-replace-regexp "^[ \t]*\\(Sublist\\|Item\\)[ \t\r]*$"
			  ")(")
      (goto-char (point-min))
      (catch 'ack
	(while (not (eobp))
	  (beginning-of-line)
	  (if (not (looking-at "^[ \t]*[()]+[ \t\r]*$"))
	      (progn
		(skip-chars-forward " \t\r")
		(insert "\"")
		(end-of-line)
		(skip-chars-backward " \t\r")
		(insert "\"")))
	  (condition-case ()
	      (next-line 1)
	    (error (throw 'ack t)))))
      (goto-char (point-max))
      (insert "\n)")
      (let ((dat nil))
	(goto-char (point-min))
	(condition-case ()
	    (setq dat (read (current-buffer)))
	  (error nil))
	dat))))

(defun w3-hotlist-apropos (regexp)
  "Show hotlist entries matching REGEXP."
  (interactive "sW3 Hotlist Apropos (regexp): ")
  (or w3-setup-done (w3-do-setup))
  (let ((save-buf (get-buffer "Hotlist")) ; avoid killing this
	(w3-hotlist
	 (apply
	  'nconc
	  (mapcar
	   (function
	    (lambda (entry)
	      (if (or (string-match regexp (car entry))
		      (string-match regexp (car (cdr entry))))
		  (list entry))))
	   w3-hotlist))))
    (if (not w3-hotlist)
	(message "No w3-hotlist entries match \"%s\"" regexp)
      (and save-buf (save-excursion
		      (set-buffer save-buf)
		      (rename-buffer (concat "Hotlist during " regexp))))
      (unwind-protect
	  (progn
	    (w3-show-hotlist)
	    (rename-buffer (concat "Hotlist \"" regexp "\""))
	    (setq url-current-file (concat "hotlist/" regexp)))
	(and save-buf (save-excursion
			(set-buffer save-buf)
			(rename-buffer "Hotlist")))
	))))

(defun w3-hotlist-refresh ()
  "Reload the default hotlist file into memory"
  (interactive)
  (w3-parse-hotlist)
  (if (fboundp 'w3-add-hotlist-menu) (w3-add-hotlist-menu)))

(defun w3-delete-from-alist (x alist)
  ;; Remove X from ALIST, return new alist
  (if (eq (assoc x alist) (car alist)) (cdr alist)
    (delq (assoc x alist) alist)))

(defun w3-hotlist-delete ()
  "Deletes a document from your hotlist file"
  (interactive)
  (save-excursion
    (if (not w3-hotlist) (message "No hotlist in memory!")
      (if (not (file-exists-p w3-hotlist-file))
	  (message "Hotlist file %s does not exist." w3-hotlist-file)
	(let* ((completion-ignore-case t)
	       (title (car (assoc (completing-read "Delete Document: "
						   w3-hotlist nil t)
				  w3-hotlist)))
	       (case-fold-search nil)
	       (buffer (get-buffer-create " *HOTW3*")))
	  (and (string= title "") (error "No document specified."))
	  (set-buffer buffer)
	  (erase-buffer)
	  (mm-insert-file-contents w3-hotlist-file)
	  (goto-char (point-min))
	  (if (re-search-forward (concat "^" (regexp-quote title) "\r*$")
				 nil t)
	      (progn
		(previous-line 1)
		(beginning-of-line)
		(delete-region (point) (progn (forward-line 2) (point)))
		(write-file w3-hotlist-file)
		(setq w3-hotlist (w3-delete-from-alist title w3-hotlist))
		(kill-buffer (current-buffer)))
	    (message "%s was not found in %s" title w3-hotlist-file))))))
  (and (fboundp 'w3-add-hotlist-menu) (funcall 'w3-add-hotlist-menu)))

(defun w3-hotlist-rename-entry (title)
  "Rename a hotlist item"
  (interactive (list (let ((completion-ignore-case t))
		       (completing-read "Rename entry: " w3-hotlist nil t))))
  (cond					; Do the error handling first
   ((string= title "") (error "No document specified!"))
   ((not w3-hotlist) (error "No hotlist in memory!"))
   ((not (file-exists-p (expand-file-name w3-hotlist-file)))
    (error "Hotlist file %s does not exist." w3-hotlist-file))
   ((not (file-readable-p (expand-file-name w3-hotlist-file)))
    (error "Hotlist file %s exists, but is unreadable." w3-hotlist-file)))
  (save-excursion
    (let ((obj (assoc title w3-hotlist))
	  (used (mapcar 'car w3-hotlist))
	  (buff (get-buffer-create " *HOTW3*"))
	  (new nil)
	  )
      (while (or (null new) (member new used))
	(setq new (read-string "New name: ")))
      (set-buffer buff)
      (erase-buffer)
      (mm-insert-file-contents (expand-file-name w3-hotlist-file))
      (goto-char (point-min))
      (if (re-search-forward (regexp-quote title) nil t)
	  (progn
	    (previous-line 1)
	    (beginning-of-line)
	    (delete-region (point) (progn (forward-line 2) (point)))
	    (w3-insert (format "%s %s\n%s\n" (nth 1 obj) (current-time-string)
			    new))
	    (setq w3-hotlist (cons (list new (nth 1 obj))
				   (w3-delete-from-alist title w3-hotlist)))
	    (write-file w3-hotlist-file)
	    (kill-buffer (current-buffer))
	    (if (and w3-running-FSF19 (not (eq 'tty (device-type))))
		(progn
		  (delete-menu-item '("Go"))
		  (w3-build-FSF19-menu))))
	(message "%s was not found in %s" title w3-hotlist-file))))
  (and (fboundp 'w3-add-hotlist-menu) (funcall 'w3-add-hotlist-menu)))

(defun w3-hotlist-append (fname)
  "Append a hotlist to the one in memory"
  (interactive "fAppend hotlist file: ")
  (let ((x w3-hotlist))
    (w3-parse-hotlist fname)
    (setq w3-hotlist (nconc x w3-hotlist))
    (and (fboundp 'w3-add-hotlist-menu) (funcall 'w3-add-hotlist-menu))))

(defun w3-parse-hotlist (&optional fname)
  "Read in the hotlist specified by FNAME"
  (if (not fname) (setq fname w3-hotlist-file))
  (setq w3-hotlist nil)
  (if (not (file-exists-p fname))
      (message "%s does not exist!" fname)
    (let* ((old-buffer (current-buffer))
	   (buffer (get-buffer-create " *HOTW3*"))
	   cur-link
	   cur-alias)
      (set-buffer buffer)
      (erase-buffer)
      (mm-insert-file-contents fname)
      (goto-char (point-min))
      (while (re-search-forward "^\n" nil t) (replace-match ""))
      (goto-line 3)
      (while (not (eobp))
	(re-search-forward "^[^ ]*" nil t)
	(setq cur-link (buffer-substring (match-beginning 0) (match-end 0)))
	(setq cur-alias (buffer-substring (progn
					    (forward-line 1)
					    (beginning-of-line)
					    (point))
					  (progn
					    (end-of-line)
					    (point))))
	(if (not (equal cur-alias ""))
	    (setq w3-hotlist (cons (list cur-alias cur-link) w3-hotlist))))
      (kill-buffer buffer)
      (and (fboundp 'w3-add-hotlist-menu) (funcall 'w3-add-hotlist-menu))
      (set-buffer old-buffer))))

;;;###autoload
(defun w3-use-hotlist ()
  "Possibly go to a link in your W3/Mosaic hotlist.
This is part of the emacs World Wide Web browser.  It will prompt for
one of the items in your 'hotlist'.  A hotlist is a list of often
visited or interesting items you have found on the World Wide Web."
  (interactive)
  (if (not w3-setup-done) (w3-do-setup))
  (if (not w3-hotlist) (message "No hotlist in memory!")
    (let* ((completion-ignore-case t)
	   (url (car (cdr (assoc
			   (completing-read "Goto Document: " w3-hotlist nil t)
			   w3-hotlist)))))
      (if (string= "" url) (error "No document specified!"))
      (w3-fetch url))))

(defun w3-hotlist-add-document-at-point (pref-arg)
  "Add the document pointed to by the hyperlink under point to the hotlist."
  (interactive "P")
  (let ((url (w3-view-this-url t))
	(title "nil"))
    (or url (error "No link under point."))
    (setq title (nth 3 (w3-zone-data (w3-zone-at (point)))))
    (w3-hotlist-add-document pref-arg title url)
    (and (fboundp 'w3-add-hotlist-menu) (funcall 'w3-add-hotlist-menu))))

(defun w3-hotlist-add-document (pref-arg &optional the-title the-url)
  "Add this documents url to the hotlist"
  (interactive "P")
  (save-excursion
    (let* ((buffer (get-buffer-create " *HOTW3*"))
	   (title (or the-title
		      (and pref-arg (read-string "Title: "))
		      (buffer-name)))
	   (url (or the-url (url-view-url t))))
      (if (rassoc (list url) w3-hotlist)
	  (error "That item already in hotlist, use w3-hotlist-rename-entry."))
      (set-buffer buffer)
      (erase-buffer)
      (setq w3-hotlist (cons (list title url) w3-hotlist)
	    url (url-unhex-string url))
      (if (not (file-exists-p w3-hotlist-file))
	  (progn
	    (message "Creating hotlist file %s" w3-hotlist-file)
	    (w3-insert "ncsa-xmosaic-hotlist-format-1\nDefault\n\n")
	    (backward-char 1))
	(progn
	  (mm-insert-file-contents w3-hotlist-file)
	  (goto-char (point-max))
	  (backward-char 1)))
      (w3-insert "\n" (url-hexify-string url) " " (current-time-string)
		 "\n" title)
      (write-file w3-hotlist-file)
      (kill-buffer (current-buffer))))
      (and (fboundp 'w3-add-hotlist-menu) (funcall 'w3-add-hotlist-menu)))

(provide 'w3-hot)
