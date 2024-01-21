;;;-*- Package: USER; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-
#|
 Copyright (c) 1991-1995 Xerox Corporation.  All Rights Reserved.  
 
 Unlimited use, reproduction, and distribution of this software is
 permitted.  Any copy of this software must include both the above
 copyright notice of Xerox Corporation and this paragraph.  Any
 distribution of this software must comply with all applicable United
 States export control laws.  This software is made available AS IS,
 and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
 INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
 AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
 PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
 THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
 CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
 XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

 $Id: client.lisp,v 1.3 1995/05/15 22:30:27 janssen Exp $
|#

(cl:in-package :user)

(defun test-server ()
  (let ((server (ilu:lookup 'test1:the-o1 "Test1_Initial_Object@Test1-Server")))
    (if (not server)
	(error "Couldn't bind server.~%")
      (progn
	(let* ((u1 '(5 . t))
	       (css '("just" "some" "strings"))
	       (u2 (test1:u-css-to-u server u1 css)))
	  (format t "(u-css-to-u ~s ~s) => ~s~%" u1 css u2))
	(let* ((css '("just" "some" "strings"))
	       (r (test1:f-css-to-ro server css)))
	  (format t "(f-css-to-r0 ~s) => ~s~%" css r))
	(let* ((r (test1:make-r :i 238 :css '("more" "strings") :a #("test1" "test2" "bletch")))
	       (s "just a string")
	       (f (test1:r-sc-s-to-f server r s)))
	  (format t "(r-sc-s-to-f ~s ~s) => ~s~%" r s f))
	(let* ((ro nil))
	  (test1:a-ro server ro)
	  (format t "(a-ro ~s)~%" ro))
	(let* ((o2 (test1:get-o2 server)))
	  (format t "(get-o2) => ~s~%" o2)
	  (if (not o2)
	      (error "Can't get instance of O2~%")
	    (progn
	      (let* ((o server)
		     (a (make-array 8 :element-type '(unsigned-byte 8)))
		     (css (test1:oo-a0-to-css o2 o a)))
		(format t "(test1:oo-a0-to-css ~s ~s) => ~s~%" o a css))
	      (let ((r (test1:make-r :i 13 :css '("another" "list" "of" "strings") :a #("and" "an" "array")))
		    (i 3289)
		    (a1 (make-array 3 :element-type 'simple-string)))
		(setf (aref a1 0) "string 1")
		(setf (aref a1 1) "string 2")
		(setf (aref a1 2) "string 3")
		(multiple-value-bind (i2 a0)
		    (test1:r-i-a1-to-i-a0 o2 r i a1)
		  (format t "(r-i-a1-to-i-a0 ~s ~s ~s) => ~s ~s~%" r i a1 i2 a0)))
	      )))
	(let* ((o3 (test1:get-o3 server nil)))
	  (format t "(get-o3 nil) => ~s~%" o3)
	  (if (null o3)
	      (error "Couldn't construct o3 from ~s~%" server)
	    (progn
	      (let ((r (list (test1:make-r :i 2349 :css '("more") :a #("one" "two" "three")))))
		(multiple-value-bind (r2 is)
		    (test1:rs-r-to-r-is o3 r)
		  (format t "(rs-r-to-r-is ~s) => ~s ~s~%" r r2 is)))
	      (let* ((o server)
		     (u '(5 . t))
		     (u2 (test1:o1-u-to-u o3 o u)))
		(format t "(o1-u-to-u ~s ~s) => ~s~%" o u u2))
	      )))
	))))
