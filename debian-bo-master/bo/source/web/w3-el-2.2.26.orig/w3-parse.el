;;; w3-parse.el,v --- Generalized html/sgml parsing support for emacs-w3
;; Author: wmperry
;; Created: 1995/10/28 02:42:43
;; Version: 1.81
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
;;; The parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)

(defmacro w3-can-safely-ignore (p1 p2 swallow)
  (` (cond
      ((= (, p1) (, p2)) t)
      ((/= (, swallow) 0) nil)
      ((= (abs (- (, p2) (, p1))) 1)
       (memq (or (char-after (, p1)) ?\n) '(?\r ?\t ? )))
      (t
       (let (done)
	 (while (and (< (, p1) (, p2)) (not done))
	   (if (memq (or (char-after (, p1)) ?\n) '(?\n ?\r ?\t ? ))
	       nil
	     (setq done t))
	   (setq (, p1) (1+ (, p1))))
	 (not done))))))

(condition-case ()
    (require 'w3-10646)
  (error (fset 'w3-resolve-numeric-entity 'char-to-string)))

(defun w3-nuke-entities-in-region (st &optional nd)
  (if (null st)
      nil
    (save-restriction
      (let (entity entity-pos)
	(narrow-to-region st nd)
	(if (not (boundp 'MULE))
	    (subst-char-in-region st nd ?\222 ?'))
	(goto-char (point-min))
	(catch 'entity-exit
	  (while (not (eobp))
	    (skip-chars-forward "^&")
	    (setq entity-pos (point))
	    (if (eobp)
		(throw 'entity-exit nil)
	      (forward-char 1))
	    (cond
	     ((eobp) (setq entity "&"))
	     ((= (char-after (point)) ?#)
	      (forward-char 1)
	      (setq entity
		    (condition-case ()
			(let ((x (read (current-buffer))))
			  (if (symbolp x) (setq x (string-to-int
						   (symbol-name x))))
			  (w3-resolve-numeric-entity x))
		      (error nil)))
	      (cond
	       ((boundp 'MULE) nil)
	       ((string= entity "\231")
		(setq entity (get 'w3-entities 'trade)))
	       ((string= entity "\222")
		(setq entity (get 'w3-entities 'rsquo)))
	       (t nil)))
	     ((memq (char-after (point)) '(?  ?\t ?\n ?\r ?.))
	      (setq entity "&"))
	     (t
	      (setq entity (get 'w3-entities (condition-case ()
						 (read (current-buffer))
					       (error nil))))))
	    (if entity
		(progn
		  (if (and (not (eobp))
			   (= (char-after (point)) ?\;))
		      (delete-region entity-pos (1+ (point)))
		    (delete-region entity-pos (point)))
		  (insert entity))))))
      (goto-char (if (< st nd) (point-max) (point-min))))))

(defun w3-preparse-buffer (&optional buff nodraw)
  "Do a preliminary parse of an HTML buffer BUFF.
BUFF defaults to `url-working-buffer'.

This returns the parsed HTML a list suitable for use by w3-draw-html."
  (set-buffer (or buff url-working-buffer))
  (setq buff (current-buffer))
  (set-syntax-table w3-parse-args-syntax-table)
  (if (fboundp 'sera-to-fidel-marker) 
      (let ((sera-being-called-by-w3 t))
	(sera-to-fidel-marker)))
  (goto-char (point-min))
  (if (not nodraw)
      (let ((buf (get-buffer-create (url-generate-new-buffer-name
				     "Untitled")))
	    (info (mapcar (function (lambda (x) (cons x (symbol-value x))))
			  w3-persistent-variables)))
	(setq w3-draw-buffer buf)
	(save-excursion
	  (set-window-buffer (selected-window) buf)
	  (set-buffer buf)
	  (erase-buffer)
	  (setq w3-last-fill-pos (point)
		fill-column (- (or w3-strict-width (window-width))
			       w3-right-border)
		fill-prefix "")
	  (mapcar (function (lambda (x) (set (car x) (cdr x)))) info)
	  (w3-init-state))))
  (let (ptree
	tag
	args
	last-pos
	expendable
	(swallow-newlines 0)
        (gc-cons-threshold (if (> w3-gc-cons-threshold-multiplier 0)
                               (* w3-gc-cons-threshold-multiplier
                                  gc-cons-threshold)
                             gc-cons-threshold))
	ctr)
    (setq last-pos (point-min)
	  ctr 0)
    (goto-char (point-min))
    (catch 'w3exit
      (while (not (eobp))
        ;; Ignore tag start character < unless it is followed by tag name
        ;; or ! or / characters.  This handles bad HTML.
        (if (re-search-forward "<[A-Za-z!/]" nil 'move)
            (goto-char (match-beginning 0)))
	(setq expendable last-pos)
	(if (w3-can-safely-ignore expendable (point) swallow-newlines)
	    (if (/= last-pos (point))
		(progn
		  (if (not nodraw)
		      (w3-handle-single-tag 'text " "))
		  (setq ptree (cons (cons 'text " ") ptree))))
	  (if (/= swallow-newlines 0)
	      (subst-char-in-region last-pos (point) ?\r ? )
	    ;; (save-restriction
	    (narrow-to-region last-pos (point))
	    (goto-char (point-min))
	    (while (re-search-forward "[ \t\n\r]+" nil t)
	      (replace-match " "))
	    (goto-char (point-max))
	    (widen))
	  (w3-nuke-entities-in-region last-pos (point))
	  (setq ptree (cons (cons 'text (buffer-substring last-pos (point)))
			    ptree))
	  (if (not nodraw)
	      (w3-handle-single-tag 'text (cdr (car ptree)))))
	(setq last-pos (1+ (point)))
	(if (looking-at "<!--")
	    (progn
	      (forward-char 4)
	      (if (re-search-forward "--[ \t\n]*>" nil t)
		  (setq last-pos (point))
		(w3-warn 'html "Unterminated comment, attempting to cope.")
		(skip-chars-forward "^>")
		(if (eobp)
		    (throw 'w3-exit nil))
		(forward-char 1)))
	  (condition-case ()
	      (forward-sexp 1)
	    (error
	     (condition-case ()
		 (forward-char 1)
	       (error (throw 'w3exit nil)))
	     (skip-chars-forward "^<>")
	     (if (looking-at ">")
		 (forward-char 1))))
	  (url-lazy-message "Parsed %d of %d (%d%%)" (point)
		   (point-max) (url-percentage (point) (point-max)))
	  (condition-case ()
	      (narrow-to-region last-pos (1- (point)))
	    (error (throw 'w3exit nil)))
	  (setq last-pos (point))
	  (goto-char (point-min))
	  (skip-chars-forward "^ \t\n\r")
	  (downcase-region (point-min) (point))
	  (goto-char (point-min))
	  (setq tag (condition-case ()
			(read buff)
		      (error nil))
		args (if (< (point) (point-max))
			 (w3-parse-args (point) (point-max))))
	  (cond
	   ((null tag) nil)
	   ((eq tag 'plaintext)
	    (widen)
	    (skip-chars-forward "> \n")
	    (setq ptree (cons
			 (cons 'plaintext
			       (list
				(cons 'data
				      (buffer-substring (point) (point-max)))))
			 ptree))
	    (if (not nodraw)
		(w3-handle-single-tag (car (car ptree))
				      (cdr (car ptree))))
	    (throw 'w3exit nil))
	   ((eq tag 'style)
	    (let ((case-fold-search t))
	      (widen)
	      (skip-chars-forward "> \n")
	      (setq last-pos (point))
	      (if (search-forward "</style" nil t)
		  (goto-char (match-beginning 0))
		(w3-warn 'html "Unterminated <style> tag, coping..."))
	      (setq args (cons (cons 'data (buffer-substring last-pos
							      (point)))
			       args))))
	   ((eq tag 'xmp)
	    (let ((case-fold-search t))
	      (widen)
	      (skip-chars-forward ">\n")
	      (setq last-pos (point))
	      (if (search-forward "</xmp" nil t)
		  (goto-char (match-beginning 0))
		(w3-warn 'html "Unterminated <xmp> tag.")
		(goto-char (point-max)))
	      (setq tag 'xmp
		    args (list (cons 'data
				     (buffer-substring last-pos (point)))))))
	   ((eq tag 'listing)
	    (let ((case-fold-search t))
	      (widen)
	      (skip-chars-forward "> \n")
	      (setq last-pos (point))
	      (if (search-forward "</listing" nil t)
		  (goto-char (match-beginning 0))
		(w3-warn 'html "Unterminated <listing> tag.")
		(goto-char (point-max)))
	      (setq tag 'text
		    args (concat
			  (if (/= (or (char-after last-pos) ?\n) ?\n)
			      "\n" "")
			  (buffer-substring last-pos (point))))))
	   ((eq tag 'textarea)
	    (let ((case-fold-search t))
	      (widen)
	      (skip-chars-forward "> \n")
	      (setq last-pos (point))
	      (if (search-forward "</textarea" nil t)
		  (progn
		    (goto-char (match-beginning 0))
		    (skip-chars-backward " \n"))
		(w3-warn 'html "Unterminated <textarea> tag."))
	      (w3-nuke-entities-in-region last-pos (point))
	      (setq args (cons (cons 'data
				     (buffer-substring last-pos (point)))
			       args))))
	   ((memq tag '(pre lit))
	    (setq swallow-newlines (1+ swallow-newlines)))
	   ((eq tag 'embed)
	    (let* ((case-fold-search t)
		   (data (if (re-search-forward "</embed" nil t)
			     (progn
			       (goto-char (match-beginning 0))
			       (buffer-substring last-pos (point))))))
	      (if data (setq args (cons (cons 'data data) args)))))
	   ((memq tag '(/pre /lit))
	    (setq swallow-newlines (max (1- swallow-newlines) 0))))
	  (widen)
	  (forward-char 1)
	  (setq last-pos (point))
	  (if tag
	      (progn
		(or nodraw
		    (progn
                      (and w3-do-incremental-display
                           (= (% (setq ctr (1+ ctr)) 15) 0)
			  (w3-pause))
		      (w3-handle-single-tag tag args)))
		(setq ptree (cons (cons tag args) ptree)))))))
    (message "Done")
    (if nodraw
	(nreverse ptree)
      (cons w3-draw-buffer (nreverse ptree)))))

(defun w3-parse-args (st nd)
  "Return an assoc list of attribute/value pairs from an SGML-type string"
  (let (
	name				; From name=
	value				; its value
	results				; Assoc list of results
	name-pos			; Start of XXXX= position
	val-pos				; Start of value position
	)
    (save-restriction
      (narrow-to-region st nd)
      (goto-char (point-min))
      (while (not (eobp))
	(skip-chars-forward " \n\r\t")
	(setq name-pos (point))
	(skip-chars-forward "^ \n\r\t=")
	(downcase-region name-pos (point))
	(save-restriction
	  (narrow-to-region name-pos (point))
	  (goto-char name-pos)
	  (condition-case ()
	      (setq name (read (current-buffer)))
	    (error (setq name nil))))
	(skip-chars-forward " \t\r\n")
	(if (/= (or (char-after (point)) 0)  ?=) ; There is no value
	    (setq value nil)
	  (skip-chars-forward " \t\r\n=")
	  (setq val-pos (point)
		value
		(cond
		 ((or (= (or (char-after val-pos) 0) ?\")
		      (= (or (char-after val-pos) 0) ?'))
		  (let ((x (set-marker (make-marker)
				       (condition-case ()
					   (prog2
					       (forward-sexp 1)
					       (1- (point))
					     (skip-chars-forward "\""))
					 (error
					  (skip-chars-forward "^ \t\r\n")
					  (point))))))
		    (w3-nuke-entities-in-region (1+ val-pos) x)
		    (goto-char x)
		    (skip-chars-forward "\"")
		    (buffer-substring (1+ val-pos) x)))
		 (t
		  (buffer-substring val-pos
				    (progn
				      (skip-chars-forward "^ \t\r\n")
				      (point)))))))
	(setq results (cons (cons name value) results)))
      results)))

(provide 'w3-parse)
