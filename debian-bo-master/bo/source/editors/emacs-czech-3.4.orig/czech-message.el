;; @(#) czech-message.el -- Czech support for mail/news

;; @(#) $Id: czech-message.el,v 3.4 1997/02/26 20:42:14 pdm Exp $
;; @(#) $Keywords: i18n, Czech, encoding, mail, news, Gnus $
;; $KnownCompatibility: 19.34, XEmacs 19.14 $

;; This file is *NOT* part of GNU Emacs nor XEmacs.

;; Copyright (C) 1996, 1997 Milan Zamazal

;; Author:       Milan Zamazal <pdm@fi.muni.cz>
;; Maintainer:   Milan Zamazal <pdm@fi.muni.cz>
;; Requires:     czech.el, czech-convert.el
;; Remark:       Don't laugh too loudly while reading this file, please.

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, version 2 of the License.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs and/or this package.  If you did not, write to the
;; Free Software Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:

;; This supports Czech handling of mail and news.  Currently only Gnus are
;; supported.

;;; History:

;; So long, so very long...

;;; Code:


;;; *** Start ***

(require 'czech)
(require 'czech-convert)
(require 'rfc822)

(defconst cz-message-version "$Id: czech-message.el,v 3.4 1997/02/26 20:42:14 pdm Exp $"
  "Latest modification time and version number.")

(defvar cz-message-this-method 'default
  "Encoding of the current message.")
(make-variable-buffer-local 'cz-message-this-method)

(defvar cz-message-before-hook nil
  "Hooks run before Czech message is beeing prepared.")

(defvar cz-message-after-hook nil
  "Hooks run after Czech message has been prepared.")


;;; *** MIME setup ***

(defun cz-message-setup-mime ()
  "Sets correct values of some Gnus and tm variables."
  (setq default-mime-charset 'iso-8859-2)
  ;; Following lines are copied from gnus-mime.el in tm
  ;; (I had to because of defvar conflict between Gnus 5.3 and tm 7.78)
  (setq gnus-show-mime-method 'gnus-article-preview-mime-message)
  (setq gnus-decode-encoded-word-method 'gnus-article-decode-encoded-word)
  (setq gnus-parse-headers-hook
	'(gnus-set-summary-default-charset gnus-decode-rfc1522))
  ;; Announce that setup was done
  (provide 'cz-message-mime-ok))


;;; *** Main part ***

;;;###autoload
(defun cz-message-send (&optional arg)
  "Run `cz-message-prepare' and then `message-send'."
  (interactive "P")
  (run-hooks 'cz-message-before-hook)
  (cz-message-prepare)
  (run-hooks 'cz-message-after-hook)
  (message-send arg))

;;;###autoload
(defun cz-message-send-and-exit (&optional arg)
  "Run `cz-message-prepare' and then `message-send-and-exit'."
  (interactive "P")
  (run-hooks 'cz-message-before-hook)
  (if (cz-message-prepare)
      (progn
	(run-hooks 'cz-message-after-hook)
	(message-send-and-exit arg))
    (run-hooks 'cz-message-after-hook)))

(defun cz-message-prepare ()
  "Perform conversion on the current message buffer and ask for confirmation.
See variables `cz-gnus', `cz-message-method-default',
`cz-message-method-people', and `cz-message-news' for basic explanation, what
conversion will be performed."
  (let ((method (cz-message-show-method 'quietly)))
    ;; Handle methods
    (save-excursion
      (cond
       ((eq method 'ascii)
	(cz-convert-to-csech (point-min) (point-max))
	(cz-message 7 "Message converted to 7-bit ASCII."))
       ((eq method 'mime)
	(cz-message-convert-to-mime 7))
       ((eq method 'mime-8)
	(cz-message-convert-to-mime 8))
       ((eq method nil)
	(cz-message 7 "Message NOT converted."))
       (t (error "Unknown conversion method: %s" (symbol-name method)))))
    ;; Ask for confirmation
    (or (not cz-message-ask-on-send)
	(yes-or-no-p
	 (format "Is the %s correct? " (if (message-mail-p)
					   (if (message-news-p)
					       "MAIL and ARTICLE"
					     "MAIL")
					 "ARTICLE"))))))

(defun cz-message-convert-to-mime (bit-level)
  "Convert message to MIME.
BIT-LEVEL is either 7 or 8."
  (if (not (featurep 'cz-message-mime-ok))
      (cz-message-setup-mime))
  (if (not mime/editor-mode-flag) (mime/editor-mode))
  (mime-editor/toggle-transfer-level bit-level)
  (mime-editor/maybe-translate)
  (cz-message 7 (format "Message converted to %d-bit MIME." bit-level)))

(defun cz-message-method ()
  "Find conversion method of the current message.
Method is found according to values of miscellaneous variables, especially
`cz-gnus', `cz-message-method-default', `cz-message-method-people', and
`cz-message-news'."
  (if (message-news-p)		     ; if both news and mail, news is prefered
      (save-excursion
	(save-restriction
	  (message-narrow-to-headers)
	  (cz-message-find-method
	   (cz-message-rfc822-addresses "newsgroups" "followup-to")
	   cz-message-method-newsgroups
	   cz-message-method-news
	   nil)))
    (save-excursion
      (save-restriction
	(message-narrow-to-headers)
	(cz-message-find-method
	 (cz-message-rfc822-addresses "to" "cc" "bcc")
	 cz-message-method-people
	 cz-message-method-mail
	 cz-message-use-bbdb)))))

(defun cz-message-rfc822-addresses (&rest fields)
  "Auxiliary for getting addresses even containing 8-bit characters."
  (let ((orig (buffer-substring (point-min) (point-max)))
	string
	addresses)
    ;; Unaccent
    (cz-convert-to-csech (point-min) (point-max))
    ;; Find addresses
    (while fields
      (setq string (concat string (message-fetch-field (car fields)) ","))
      (setq fields (cdr fields)))
    (setq addresses (rfc822-addresses string))
    ;; Return accents
    (delete-region (point-min) (point-max))
    (insert orig)
    addresses))

(defun cz-message-find-method (recipients recip-method-list default use-bbdb)
  "Find conversion method of message according to given variables.
Returns one of the following symbols: `ascii', `mime', `mime-8', `plain'.
If USE-BBDB is non-`nil', use Insidious Big Brother Database
`mailtype' (user defined) field."
  (let (methods)
    (while recipients
      (setq methods (cons (cz-message-find-method-one (car recipients)
						      recip-method-list
						      default
						      use-bbdb)
			  methods))
      (setq recipients (cdr recipients)))
    ;; Select "the weakest" method
    (if (memq 'ascii methods) 'ascii
      (if (memq 'mime methods)
	  (if (memq 'plain methods) 'ascii
	    'mime)
	(if (memq 'mime-8 methods) 'mime-8
	  'plain)))))

(defun cz-message-find-method-one (recipient recip-method-list default
					     use-bbdb)
  (or
   ;; Try to find in bbdb
   (and use-bbdb
	(or (featurep 'bbdb) (load "bbdb" t))
	(cz-message-find-bbdb recipient))
   ;; Try to find in the list
   (cz-message-find-list recipient recip-method-list)
   ;; Find default value
   (progn
     (if (not default)
	 (setq default cz-message-method-default))
     (if (eq default 'plain)
	 nil
       default))))

(defun cz-message-find-list (recipient list)
  (let (method
	aux)
    (while (and (not method) list)
      (setq aux (cdr (car list)))	; skip type
      (while (and (not method) aux)
	(if (string-match (car aux) recipient)
	    (setq method (car (car list)))
	  (setq aux (cdr aux))))
      (setq list (cdr list)))
    method))

(defun cz-message-find-bbdb (recipient)
  (let ((record (bbdb-search-simple nil recipient))
	(aux))
    (and record
	 (setq aux (bbdb-record-getprop record 'mailtype))
	 (intern aux))))


;;; *** Miscellaneous***

(defvar cz-message-history nil
  "History for `cz-message-set-method'.")

;;;###autoload
(defun cz-message-set-method ()
  "Sets final encoding of the current message."
  (interactive)
  (let ((method (completing-read
		 "Message encoding: "
		 (if (eq cz-gnus t)
		     '(("mime" 1) ("ascii" 2) ("plain" 3) ("default" 4))
		   '(("ascii" 2) ("plain" 3) ("default" 4)))
		 nil nil nil
		 'cz-message-history)))
    (setq cz-message-this-method (intern method))))


;;;###autoload
(defun cz-message-show-method (&optional quietly)
  "Prints final encoding of the current message.

If an optional argument QUIETLY is non-`nil', no message is printed, only
symbol value is returned."
  (interactive)
  (let ((method (if (not (eq cz-message-this-method 'default))
		    cz-message-this-method
		  (cz-message-method))))
    ;; Check MIME availability
    (if (and (or (eq method 'mime) (eq method 'mime-8))
	     (not (eq cz-gnus t)))
	(progn
	  (if quietly
	      (cz-message 5 "MIME method requested but forbidden."))
	  (setq method 'ascii)))
    ;; If `plain', use 8-bit MIME if available
    (if (and (eq method 'plain) (and (not (member 'bad-sendmail cz-evils))))
	(setq method (if (eq cz-gnus t) 'mime-8 nil)))
    ;; Final work around
    (if quietly
	method
      (message (format "On the current message would be aplied method %s."
		       (cond
			((eq method 'ascii) "ASCII")
			((eq method 'mime) "MIME (7-bit)")
			((eq method 'mime-8) "MIME (8-bit)")
			((eq method 'plain) "plain")))))))


;;; *** Announce ***

(provide 'czech-message)


;;; czech-message.el ends here

