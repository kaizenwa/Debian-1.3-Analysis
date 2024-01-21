;;; w3-srch.el,v --- Searching functions for emacs-w3
;; Author: wmperry
;; Created: 1995/06/25 01:00:09
;; Version: 1.11
;; Keywords: matching, help, comm, hypermedia

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

(defvar w3-allow-searching-of
  '("text/plain" "text/html" "text/x-setext"
    "application/x-troff-man" "application/x-troff-me"
    "application/x-troff-ms" "application/rtf"
    "text/richtext" "application/x-wais-source"
    "application/tex" "application/texinfo"
    "application/x-troff")
  "*A list of MIME content types that it is Ok for the automatic
search to descend to.")

(defun w3-do-search (term &optional base hops-limit restriction)
  "Recursively descend all the child links of the current document for TERM.
TERM may be a string, in which case it is treated as a regular expression,
and re-search-forward is used, or a symbol, in which case it is funcalled
with 1 argument, the current URL being searched.

BASE is the URL to start searching from.

HOPS-LIMIT is the maximum number of nodes to descend before they
search dies out.

RESTRICTION is a regular expression or function to call with one
argument, a URL that could be searched.  If RESTRICTION returns
non-nil, then the url is added to the queue, otherwise it is
discarded.  This is useful for restricting searching to either
certain tyes of URLs (only search ftp links), or restricting searching
to one domain (only search stuff in the indiana.edu domain).

For use in functions passed to w3-do-search:
QUEUE is the queue of links to be searched
HOPS is the current number of hops from the root document
RESULTS is an assoc list of (URL . RETVAL), where RETVAL is the value
returned from previous calls to the TERM function (or point if searching
for a regexp"
  (let ((x))
    (or base (setq base (url-view-url t)))
    (if (setq x (url-buffer-visiting base))
	(set-buffer x)
      (w3-fetch base))
    (w3-search-internal term hops-limit restriction)))

(defun w3-normalize-url (url)
  "Normalize a URL, removing all '#' references from it, etc."
  (cond
   ((null url) nil)
   ((string-match "#\\(.*\\)" url) (url-match url 1))
   (t url)))
  
(defun w3-search-internal (term &optional hops-limit restriction)
  "Recursively descend all the child links of the current document for TERM.
TERM may be a string, in which case it is treated as a regular expression,
and re-search-forward is used, or a symbol, in which case it is funcalled
with 1 argument, the current URL being searched.

HOPS-LIMIT is the maximum number of nodes to descend before they
search dies out.

RESTRICTION is a regular expression or function to call with one
argument, a URL that could be searched.  If RESTRICTION returns
non-nil, then the url is added to the queue, otherwise it is
discarded.  This is useful for restricting searching to either
certain tyes of URLs (only search ftp links), or restricting searching
to one domain (only search stuff in the indiana.edu domain).

For use in functions passed to w3-do-search:
QUEUE is the queue of links to be searched
HOPS is the current number of hops from the root document
RESULTS is an assoc list of (URL . RETVAL), where RETVAL is the value
returned from previous calls to the TERM function (or point if searching
for a regexp"
  (setq hops-limit (or hops-limit 5))
  (let ((queue '())
	(visited '())
	(results nil)
	(hops 0))

    ;; Search initial page and stick it in the results list
    (goto-char (point-min))
    (cond
     ((stringp term)
      (setq results (cons (url-view-url t) (re-search-forward term nil t))))
     ((symbolp term)
      (setq results (cons (url-view-url t) (funcall term (url-view-url t))))))

    ;; Build the initial queue of just the links on this page that are
    ;; deemed searchable
    (w3-map-links
     (function
      (lambda (x st nd y)
	(if (and
	     (member (nth 8 (url-file-attributes (nth 2 x)))
			w3-allow-searching-of)
	     (cond
	      ((null (nth 2 x)) nil)
	      ((stringp restriction) (string-match restriction (nth 2 x)))
	      ((symbolp restriction) (funcall restriction (nth 2 x)))
	      (t t)))
	    (setq queue (nconc queue (list (w3-normalize-url (nth 2 x)))))))))

    (while queue
      (let ((x (car queue)) y)
	(setq visited (cons x visited))
	(if (setq y (url-buffer-visiting x))
	    (set-buffer y)
	  (url-retrieve x))
	(cond
	 ((equal (or url-current-mime-type
		     (mm-extension-to-mime (w3-file-extension
					    url-current-file))) "text/html")
	  (w3-prepare-buffer t)
	  (w3-map-links
	   (function
	    (lambda (link-data st nd searching-func)
	      (let* ((url (w3-normalize-url (nth 2 link-data)))
		     (info (and
			    (cond
			     ((null url) nil)
			     ((stringp restriction)
			      (string-match restriction url))
			     ((symbolp restriction)
			      (funcall restriction url))
			     (t t))
			    (url-file-attributes url)))
		     (num-children 0))
		(cond
		 ((null info)
		  (message "Skipping %s (not searchable)" url) nil)
		 ((member (nth 8 info) w3-allow-searching-of)
		  (if (< hops hops-limit)
		      (w3-map-links	; Count the child links
		       (function	; and add them to the queue to 
			(lambda (lnk st nd arg) ; be serviced
			  (setq num-children (1+ num-children))
			  (if (or
			       (member url visited) 	; already seen it
			       (member url queue)) 	; planning on seeing it
			      nil
			    (setq queue (nconc queue (list url))))))))
		  (goto-char (point-min))
		  (cond
		   ((stringp term)
		    (setq results (cons (cons url
					      (re-search-forward term nil t))
					results)))
		   ((symbolp term)
		    (setq results (cons (cons url (funcall term url))
					results)))
		   (t
		    (error "TERM must be a regular expression or symbol."))))
		 (t (message "Skipping %s (why?)" url))))))))
	 (t
	  (goto-char (point-min))
	  (cond
	   ((stringp term)
	    (setq results (cons (cons x (re-search-forward term nil t))
				results)))
	   ((symbolp term)
	    (setq results (cons (cons x (funcall term x)) results)))))))
      (setq queue (cdr queue)))
    results))

(provide 'w3-srch)
