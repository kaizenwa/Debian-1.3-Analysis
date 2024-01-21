;;; tm-gnus3.el --- tm-gnus module for GNUS 3.*

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1993 .. 1996 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1993/11/20
;; Version: $Revision: 7.11 $
;; Keywords: news, MIME, multimedia, multilingual, encoded-word

;; This file is not part of tm (Tools for MIME).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'tl-list)
(require 'tl-str)
(require 'tl-misc)
(require 'gnus)
(require 'tm-gd3)

(autoload 'mime/decode-message-header
  "tm-ew-d" "Decode MIME encoded-words in message header." t)
(autoload 'mime-eword/decode-string
  "tm-ew-d" "Decode MIME encoded-words in string." t)

(fset 'gnus-summary-select-article 'gnus-Subject-select-article)


;;; @ version
;;;

(defconst tm-gnus/RCS-ID
  "$Id: tm-gnus3.el,v 7.11 1996/12/25 17:24:21 morioka Exp $")

(defconst tm-gnus/version
  (concat (get-version-string tm-gnus/RCS-ID) " for GNUS 3"))


;;; @ variable
;;;

(defvar tm-gnus/decoding-mode t "*Decode MIME header if non-nil.")


;;; @ mode-line
;;;

(defun tm-gnus/add-decoding-mode-to-mode-line ()
  (or (assq 'tm-gnus/decoding-mode minor-mode-alist)
      (setq minor-mode-alist
	    (cons (list 'tm-gnus/decoding-mode " MIME")
		  minor-mode-alist))
      ))

(if (not (string-match "^GNUS 3\.14\.4" gnus-version))
    (progn
      (add-hook 'gnus-Article-mode-hook
		(function
		 (lambda ()
		   (make-local-variable 'minor-mode-alist)
		   (tm-gnus/add-decoding-mode-to-mode-line)
		   )))
      )
  (progn
    (add-hook 'gnus-Article-mode-hook
	      (function tm-gnus/add-decoding-mode-to-mode-line))
    ))

;;; @@ to decode subjects in mode-line
;;;
;; This function imported from gnus.el.
;;
;; New implementation in gnus 3.14.3
;;

(defun tm-gnus/article-set-mode-line ()
  "Set Article mode line string.
If you don't like it, define your own gnus-article-set-mode-line."
  (let ((maxlen 15)			;Maximum subject length
	(subject
	 (if gnus-current-headers
	     (mime-eword/decode-string
	      (nntp-header-subject gnus-current-headers))
	   "")
	 ))
    ;; The value must be a string to escape %-constructs because of subject.
    (setq mode-line-buffer-identification
	  (format "GNUS: %s%s %s%s%s"
		  gnus-newsgroup-name
		  (if gnus-current-article
		      (format "/%d" gnus-current-article) "")
		  (truncate-string subject (min (string-width subject)
						maxlen))
		  (if (> (string-width subject) maxlen) "..." "")
		  (make-string (max 0 (- 17 (string-width subject))) ? )
		  )))
  (set-buffer-modified-p t))

(fset 'gnus-Article-set-mode-line 'tm-gnus/article-set-mode-line)


;;; @ change MIME encoded-word decoding mode, decoding or non decoding.
;;;

(defun tm-gnus/set-decoding-mode (arg)
  "Set MIME encoded-word processing.
With arg, turn MIME encoded-word processing on iff arg is positive."
  (setq tm-gnus/decoding-mode arg)
  (setq gnus-have-all-headers (not gnus-have-all-headers))
  (gnus-summary-select-article (not gnus-have-all-headers) t)
  )

(defun tm-gnus/toggle-decoding-mode ()
  "Toggle MIME encoded-word processing.
With arg, turn MIME encoded-word processing on iff arg is positive."
  (interactive)
  (tm-gnus/set-decoding-mode (not tm-gnus/decoding-mode))
  )


;;; @ for tm-view
;;;

(autoload 'mime/viewer-mode "tm-view" "View MIME message." t)

(defun tm-gnus/view-message (arg)
  "MIME decode and play this message."
  (interactive "P")
  (let ((gnus-break-pages nil))
    (gnus-Subject-select-article t t)
    )
  (pop-to-buffer gnus-Article-buffer t)
  (mime/viewer-mode)
  )

(call-after-loaded
 'tm-view
 (function
  (lambda ()
    (set-alist 'mime-viewer/quitting-method-alist
	       'gnus-Article-mode
	       (if (string-match (regexp-quote "3.14.4") gnus-version)
		   (function
		    (lambda ()
		      (mime-viewer/kill-buffer)
		      (delete-other-windows)
		      (gnus-Article-show-summary)
		      ))
		 (function
		  (lambda ()
		    (mime-viewer/kill-buffer)
		    (delete-other-windows)
		    (gnus-Article-show-subjects)
		    ))
		 ))
    )))


;;; @ for tm-edit
;;;

;; suggested by OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp>
;;	1995/11/08 (c.f. [tm ML:1067])
(defun tm-gnus/insert-article (&optional message)
  (interactive)
  (let (;; for Emacs 18
	(mail-yank-ignored-headers mime-editor/yank-ignored-field-regexp)
	(news-make-reply-yank-header (function
				      (lambda (message-id from) "")
				      ))
	(news-yank-original-quoting-indicator "")
	
	;; select raw article buffer
	(mail-reply-buffer
	 (save-excursion
	   (set-buffer gnus-article-buffer)
	   (if (eq major-mode 'mime/viewer-mode)
	       mime::preview/article-buffer
	     gnus-article-buffer)))
	)
    (news-reply-yank-original 0)
    ))

(call-after-loaded
 'tm-edit
 (function
  (lambda ()
    (set-alist
     'mime-editor/message-inserter-alist
     'news-reply-mode (function tm-gnus/insert-article))
    
    (autoload 'tm-mail/insert-message "tm-mail")
    (set-alist 'mime-editor/message-inserter-alist
	       'mail-mode (function tm-mail/insert-message))

    (set-alist 'mime-editor/split-message-sender-alist
	       'news-reply-mode
	       'gnus-inews-news)
    )))


;;; @ for tm-partial
;;;

(call-after-loaded
 'tm-partial
 (function
  (lambda ()
    (set-atype 'mime/content-decoding-condition
	       '((type . "message/partial")
		 (method . mime-article/grab-message/partials)
		 (major-mode . gnus-Article-mode)
		 (summary-buffer-exp . gnus-Subject-buffer)
		 ))
    
    (set-alist 'tm-partial/preview-article-method-alist
	       'gnus-Article-mode
	       (function
		(lambda ()
		  (tm-gnus/view-message (gnus-Subject-article-number))
		  )))
    )))


;;; @ Summary decoding
;;;

(add-hook 'gnus-Select-group-hook (function tm-gnus/decode-summary-subjects))


;;; @ set up
;;;

(define-key gnus-Subject-mode-map "\et" 'tm-gnus/toggle-decoding-mode)
(define-key gnus-Subject-mode-map "v" 'tm-gnus/view-message)

(defun tm-gnus/decode-encoded-word-if-you-need ()
  (if (and tm-gnus/decoding-mode
	   (cond ((boundp 'all-headers) (not all-headers))
		 (t                     t))
	   )
      (mime/decode-message-header)
    )
  (run-hooks 'tm-gnus/article-prepare-hook)
  )

(add-hook 'gnus-Article-prepare-hook
	  (function tm-gnus/decode-encoded-word-if-you-need) t)


;;; @ for BBDB
;;;

(call-after-loaded
 'bbdb
 (function
  (lambda ()
    (require 'tm-bbdb)
    )))

(autoload 'tm-bbdb/update-record "tm-bbdb")

(defun tm-gnus/bbdb-setup ()
  (if (memq 'bbdb/gnus-update-record gnus-Article-prepare-hook)
      (progn
	(remove-hook 'gnus-Article-prepare-hook 'bbdb/gnus-update-record)
	(add-hook 'gnus-Article-prepare-hook 'tm-bbdb/update-record)
	)))

(add-hook 'gnus-startup-hook 'tm-gnus/bbdb-setup t)

(tm-gnus/bbdb-setup)


;;; @ end
;;;

(provide 'tm-gnus3)

;;; tm-gnus3.el ends here
