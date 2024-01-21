(v-load *typeset-lib* "include" "lists" )

; list/enum handling


(define do-list-item
'(

"
{\\field{\\*\\fldinst symbol 183 \\\\f \"Symbol\" \\\\s 10 \\\\h}
{\\fldrslt }}\\tab \\fi-357 "

"
{\\field{\\*\\fldinst symbol 45 \\\\f \"Symbol\" \\\\s 10 \\\\h}
{\\fldrslt }}\\tab \\fi-357 "

"{\\field{\\*\\fldinst symbol 42 \\\\f \"Symbol\" \\\\s 10 \\\\h}
{\\fldrslt }}\\tab \\fi-357 "

))

(define (o-item args)
;  (set! indent-first-line #f)
  (list-item-handler args))

(define (h-enum args)
  (inc-indent 1)
  (list-compile-enum o-enum-item)
  (dec-indent))

(define (o-enum-item args)
  (list-enum-next)
  (set! para-start "")
  (for-each write-out (list (cdar (stack->list para-start-stack))
			    (list-enum-id) "\\tab \\fi-375 ")))

(define (h-list args)
  (inc-indent 1)
  (list-compile-list o-list-item)
  (dec-indent))

(define (o-list-item args)
  (write-out (cdar (stack->list para-start-stack)))
  (set! para-start "")
  (write-out (list-list-id do-list-item) ))

(define (o-desc args)
  (push para-start-stack 'dummy))

(define (c-desc args)
  (dec-indent))

(define (o-dt args)
  (dec-indent)
  (for-each write-out
	    (list (cdar (stack->list para-start-stack)) "{\\plain\\b1 ")))

(define (c-dt args)
  (inc-indent 1)
  (for-each write-out 
	    (list #"\\b0\\par}\n" (cdar (stack->list para-start-stack)) "\\sb0 "))
  (set! para-start ""))
		  

(define (o-verb args) 
  (write-out "{\\plain \\f4\\fs20 ")
  (set! process-char rtf-process-char-verb))

(define (c-verb args)
  (write-out "\\par}")
  (set! process-char rtf-process-char))

(define (o-quote args) 
  (inc-indent 2))

(define (c-quote args)
  (dec-indent))

(define (c-caption args)
  (capture-out #f)) ; through the caption away

(define rtf-fmts-alist
`(
  (O        . #( ,o-item "" ))
  (ITEM     . #( ,o-item "" ))

  (DESC     . #( ,o-desc ,c-desc ))
  (DT       . #( ,o-dt ,c-dt ))
  (DT      . #( #"{\\plain \\s255\\li708\\sl240 " #"\\par}"))

  (ENUM     . ,h-enum )
  (LIST     . ,h-list )

  (VERB     . #( ,o-verb ,c-verb ))
  (RVERB    . #( ,o-verb ,c-verb ))
  (QUOTE    . #( ,o-quote ,c-quote ))

  (DEF      . #( #"{\\plain \\s255\\li708\\sl240 " #"\\par}"))
  (THTAG    . #( "{\\b1 " "}"))
;  (FIGURE   . #( ,o-figure "</A>"))
   (CAPTION  . #( ,capture-on ,c-caption ))
;  (GRAPHIC  . #( ,o-graphic ""))
))