;{{{ module declaration

(module files
	(import
	 (doc-basename typeset "typeset.scm")
	 (string-tr-all strings "strings.scm"))
	(export
	 (path-find-file path file)
	 (file-cat name port)
	 (make-tmp-file-name)
	 make-subfile-name
	 (path-to-file-name path)
	 (file-system-set-meta-characters meta protect)

	 output-port
	 (write-out obj)
	 (write-out-str str)
	 capture-out )

	;BEGIN BIGLOO1.8
	(eval
	 (export output-port)
	 )
	;END BIGLOO1.8
	(foreign
	 (string mktemp (string) "mktemp")
	 ;BEGIN BIGLOO1.7
	 ; (export int write-out-str (string) "write_out_str" )
	 ;END BIGLOO1.7
	 ;BEGIN BIGLOO1.8
	 (export write-out-str "write_out_str")
	 ;END BIGLOO1.8
	 )
	)

;}}}
;{{{ file/dir service

(define (path-find-file lib fn)
  (let* ((ld (if (pair? lib) (car lib) #f))
	 (ffn (if (and ld (string? ld))
		  (if (eqv? (string-ref ld (- (string-length ld) 1)) #\/)
		      (string-append ld fn)
		      (string-append ld "/" fn))
		  #f)))
    (if ffn
	(if (file-exists? ffn)
	    ffn
	    (path-find-file (cdr lib) fn))
	#f)))

(define make-subfile-name
  (let ((no 0))
    (lambda ()
      (set! no (+ no 1))
      (string-append doc-basename "-" (number->string no)))))

;;; `file-system-meta-charactes' are characters with a special meaning
;;; to the file system. Usually the path seperators. When translating
;;; from a path name into a flat file name by `path-to-file-name'
;;; those characters are replaced with the corresponding characters in
;;; file-system-protect-characters.
;;;
(define file-system-meta-characters "/")
(define file-system-protect-characters "_")

(define (file-system-set-meta-characters meta protect)
  (set! file-system-meta-characters meta)
  (set! file-system-protect-characters protect))

(define (path-to-file-name path)
  (string-tr-all path
		 file-system-meta-characters
		 file-system-protect-characters))

(define (file-cat string port)
  (let ((inport (open-input-file string)))
    (do ((line (read-line inport) (read-line inport)))
	((eof-object? line) (close-input-port inport))
    (display line port) (newline port))))

(define (make-tmp-file-name)
  (let ((str (string-copy "tsXXXXXX")))
    (mktemp str)
    str))

;}}}
;{{{ old output scheme

; the current used output port

(define output-port (open-output-file "/dev/null"))
(define (write-out obj) (display obj output-port)) ; instead of display

(define (write-out-str string)
  (display string output-port)
  1)

; capture-out with #t pushes a buffer for output-port
;             with #f get the captured output back
(define capture-out
  (let ((stack '()))
    (lambda (u/d)
      (if u/d
	  (begin
	    (set! stack (cons output-port stack))
	    (set! output-port (open-output-string))
	    output-port)
	  (if (pair? stack)
	      (let ((str (close-output-port output-port)))
		(set! output-port (car stack))
		(set! stack (cdr stack))
		str)
	      output-port)))))
;}}}
