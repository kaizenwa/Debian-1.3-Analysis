;; Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.
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

;;;; "split.scm", split input, output, and error streams into windows.
;;; Author: Aubrey Jaffer.

(require 'curses)
(define *stdscr* (initscr))
(nocbreak)
(echo)
(nl)
(define subwindow-height (max 2 (quotient (output-port-height) 5)))
(define *output-window*
  (newwin (- (output-port-height) (* 2 subwindow-height) 2)
	  (output-port-width)
	  0
	  0))
(define *input-window*
  (newwin subwindow-height
	  (output-port-width)
	  (- (output-port-height) (* 2 subwindow-height) 1)
	  0))
(define *error-window*
  (newwin subwindow-height
	  (output-port-width)
	  (- (output-port-height) subwindow-height)
	  0))
(wmove *stdscr* (- (output-port-height) subwindow-height 1) 0)
(wstandout *stdscr*)
(display (make-string (output-port-width) #\-) *stdscr*)
(wmove *stdscr* (- (output-port-height) (* 2 subwindow-height) 2) 0)
(display (make-string (output-port-width) #\-) *stdscr*)
(wstandend *stdscr*)
(touchwin *stdscr*)
(force-output *stdscr*)
(scrollok *output-window* #t)
(scrollok *input-window* #t)
(scrollok *error-window* #t)
(define *default-output-port* (set-current-output-port *output-window*))
(define *default-input-port* (set-current-input-port *input-window*))
(define *default-error-port* (set-current-error-port *error-window*))
(leaveok *output-window* #t)
(leaveok *input-window* #f)
(leaveok *error-window* #t)

(define (unsplit)
  (cond ((endwin)
	 (set-current-output-port *default-output-port*)
	 (set-current-input-port *default-input-port*)
	 (set-current-error-port *default-error-port*))))
