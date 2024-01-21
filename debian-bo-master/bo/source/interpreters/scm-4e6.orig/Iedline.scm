;; Copyright (C) 1994, 1995 Free Software Foundation, Inc.
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; As a special exception, the Free Software Foundation gives permission
;; for additional uses of the text contained in its release of GUILE.
;;
;; The exception is that, if you link the GUILE library with other files
;; to produce an executable, this does not by itself cause the
;; resulting executable to be covered by the GNU General Public License.
;; Your use of that executable is in no way restricted on account of
;; linking the GUILE library code into it.
;;
;; This exception does not however invalidate any other reasons why
;; the executable file might be covered by the GNU General Public License.
;;
;; This exception applies only to the code released by the
;; Free Software Foundation under the name GUILE.  If you copy
;; code from other Free Software Foundation releases into a copy of
;; GUILE, as the General Public License permits, the exception does
;; not apply to the code that you add in this way.  To avoid misleading
;; anyone as to the status of such modified files, you must delete
;; this exception notice from them.
;;
;; If you write modifications of your own for GUILE, it is your choice
;; whether to permit this exception to apply to your modifications.
;; If you do not wish that, delete this exception notice.  

;;  "Iedline.scm" Scheme interface to readline library
;; Author: Radey Shouman

;; Change both current-input-port and current-output-port to
;; allow line editing of input.
;; All output goes through a soft port in order to detect prompt 
;; lines, i.e. lines unterminated by a newline.

(define (make-edited-line-port)
  (let ((prompt "")
	(outp (default-output-port))
	(inp (default-input-port))
	(strp (call-with-input-string "" identity)))
    (make-soft-port
     (vector (lambda (c)
	       (write-char c outp))
	     (lambda (s)
	       (display s outp)
	       (or (zero? (string-length s))
		   (eq? #\newline (string-ref s (- (string-length s) 1)))
		   (begin
		     (set! prompt (string-append "\r" s))
		     (force-output outp))))
	     (lambda ()
	       (force-output outp))
	     (lambda ()
	       (let tail ((c (read-char strp)))
		 (if (char? c) c
		     (let ((str (read-edited-line prompt)))
		       (if (string? str)
			   (let ((n (string-length str)))
			     (add-history str)
			     (vector-set-length! str (+ 1 n))
			     (string-set! str n #\newline)
			     (set! strp (call-with-input-string
					 str identity))
			     (tail (read-char strp)))
			   str)))))
	     #f)
     OPEN_BOTH)))

(define line-editing
  (let ((edit-port #f)
	(oiport #f)
	(ooport #f))
    (lambda arg
      (define past edit-port)
      (cond ((null? arg))
	    ((and (car arg) (not edit-port))
	     (set! edit-port (make-edited-line-port))
	     (set! oiport (set-current-input-port edit-port))
	     (set! ooport (set-current-output-port edit-port)))
	    (edit-port
	     (set-current-input-port oiport)
	     (set-current-output-port ooport)
	     (set! edit-port #f)))
      past)))

(and 
 (if (provided? 'unix) (isatty? (current-input-port)) #t)
 (eq? (current-input-port) (default-input-port))
 (not (getenv "EMACS"))
 (line-editing #t))
