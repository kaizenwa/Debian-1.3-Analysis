(module chrproc
	(export
	 process-char			; to become obsolete
	 (process-escape-char c)
	 plain-process-char		; obsolete
	 man-process-char		; to become obsolete
	 process-text			; to become obsolete
	 process-cdata
	 (html-tr-string line)
	 (info-tr-string line)
	 (latex-tr-string line)
	 (latex-math-tr-string line)
	 (lout-tr-string line)
	 (man-tr-string line)
	 (plain-tr-string line)
	 html-process-text		; to become obsolete
	 man-process-text		; to become obsolete
	 lout-process-text		; to become obsolete
	 latex-process-text		; to become obsolete
	 plain-process-text		; to become obsolete
	 info-process-text		; to become obsolete
	 generic-process-text		; obsolete
	 )
	(import (write-out files "files.scm")
		(write-out-str files "files.scm"))
	(foreign 
	 (int proc-esc-char (char) "proc_esc_char" )
	 (string c-html-tr-string  (string) "html_tr_string" )
	 (string c-info-tr-string  (string) "info_tr_string" )
	 (string c-latex-tr-string (string) "latex_tr_string" )
	 (string c-latex-math-tr-string (string) "latex_math_tr_string" )
	 (string c-lout-tr-string  (string) "lout_tr_string" )
	 (string c-man-tr-string   (string) "man_tr_string" )
	 (string c-plain-tr-string (string) "plain_tr_string" )
	 )
	)

; Character parsing
; These definitions are obsolete. Don't use them!!! They are to be
; deleted.

(define process-char-orig #f)

(define (process-escape-char c)
  (let ((ci (proc-esc-char c)))
    (cond 
     ((eqv? ci -1) #f)
     ((eqv? ci -2) 
      (set! process-char process-char-orig)
      #f)
     ((eqv? ci -3)
      (set! process-char-orig process-char)
      (set! process-char process-escape-char)
      #f)
     (else(integer->char ci)))))

(define plain-process-char process-escape-char)

(define man-process-char
      (let ((cc 0))
	(lambda (c)
	  (let ((cv (process-escape-char c)))
	    (set! cc (+ cc 1))
	    (cond
	     ((eqv? cv #\newline) (set! cc 0) cv)
	     ((eqv? cv #\- ) "\\-" )
	     ((eqv? cv #\\ ) "\\e" )
	     ((eqv? cv #\.) (if (eqv? cc 1) "\\&." cv ))
	     ((eqv? cv #\') (if (eqv? cc 1) "\\&'" cv ))
	     (else cv))))))

(define process-char plain-process-char)

(define (generic-process-text line)
  (let ((len (string-length line)))
    (do ((i 0 (+ i 1)))
	((>= i len) #t)
      (let ((c (process-char (string-ref line i))))
	(if c
	    (write-out c)
	    #t)))))

;; Here the code which is due to go sometime

(define (html-process-text line)
  (write-out (c-html-tr-string line)))

(define (info-process-text line)
  (write-out (c-info-tr-string line)))

(define (latex-process-text line)
  (write-out (c-latex-tr-string line)))

(define (lout-process-text line)
  (write-out (c-lout-tr-string line)))

(define (man-process-text line)
  (write-out (c-man-tr-string line)))

(define (plain-process-text line)
  (write-out (c-plain-tr-string line)))

(define process-text generic-process-text)

;; And that for the new way:

(define (html-tr-string  s) (c-html-tr-string  s))
(define (info-tr-string  s) (c-info-tr-string  s))
(define (latex-tr-string s) (c-latex-tr-string s))
(define (latex-math-tr-string s) (c-latex-math-tr-string s))
(define (lout-tr-string  s) (c-lout-tr-string  s))
(define (man-tr-string   s) (c-man-tr-string   s))
(define (plain-tr-string s) (c-plain-tr-string s))

(define process-cdata plain-tr-string)
