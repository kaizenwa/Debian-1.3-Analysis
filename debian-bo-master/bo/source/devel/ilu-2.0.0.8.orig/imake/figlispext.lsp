;;; $Id: figlispext.lsp,v 1.2 1995/06/17 05:21:16 janssen Exp $
;;;
;;; Copyright (c) 1991-1995 Xerox Corporation.  All Rights Reserved.  
;;; 
;;; Unlimited use, reproduction, and distribution of this software is
;;; permitted.  Any copy of this software must include both the above
;;; copyright notice of Xerox Corporation and this paragraph.  Any
;;; distribution of this software must comply with all applicable United
;;; States export control laws.  This software is made available AS IS,
;;; and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
;;; INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
;;; AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
;;; PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
;;; THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
;;; CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
;;; XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
;;;

(in-package :user)

(load "../runtime/lisp/pdefsys.lisp.dist")
(with-open-file (out "./conftestlispext" :direction :output
		 :if-exists :supersede)
  (format out "~a~%" (cadr pdefsys::lisp-file-types)))
(with-open-file (out "./conftestlispcext" :direction :output
		 :if-exists :supersede)
  (format out "~a~%" #+svr4 "so" #-svr4 "o"))
(exit)
