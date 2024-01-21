(define-macro (start-tag? t) `(eq? (token-type ,t) 'STARTTAG))
(define-macro (end-tag? t) `(eq? (token-type ,t) 'ENDTAG))
(define-macro (data-token? t) `(eq? (token-type ,t) 'DATA))
