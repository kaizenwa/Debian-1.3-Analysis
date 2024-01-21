;;; -*-scheme-*-

;;; token-in? depends on the desired target
;;; better import it (inline)
;{{{ (token-in? x set-token)

(define-inline (token-in? x set-token)
  (let ((max (vector-length set-token)))
    (or
     (eqv? max 0)
     (and
      (if (pair? (token-type set-token))
	  (memq (token-type x) (token-type set-token))
	  (eq? (token-type x) (token-type set-token)))
      (or (< max 2)
	  (and (memq (token-type x) '(DATA OUTPUT PI))
	       (if (pair? (data-token-data set-token))
		   (member (data-token-data x) (data-token-data set-token))
		   (equal? (data-token-data x) (data-token-data set-token))))
	  (if (list? (token-gi set-token))
	      (memq (token-gi x) (token-gi set-token))
	      (eq? (token-gi x) (token-gi set-token))))
      (or (< max 3)
	  (equal? (token-args x) (token-args set-token))))
     )))

;}}}

;{{{ --- Some macros to be used outside ---
;  (how to export those???

;{{{ macro: (rdp-call f)

(define-macro (rdp-call f) `(lambda (c h s) (,f c h s)))

;}}}
;{{{

(define-macro (rdp-reduce c h s) `((car ,c) (cdr ,c) ,h ,s))
(define-macro (rdp-push f stack) `(cons ,f ,stack))

;}}}
