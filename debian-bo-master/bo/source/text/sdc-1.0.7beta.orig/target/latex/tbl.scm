(define (latex-write-tbl t)
  (letrec
      ((longest-pattern
	(lambda (pattern-list pattern)
	  (if (null? pattern-list)
	      (reverse (cdr pattern))
	      (if (< (length pattern) (length (car pattern-list)))
		  (longest-pattern (cdr pattern-list) (car pattern-list))
		  (longest-pattern (cdr pattern-list) pattern)))))

       (write-tabular-pattern 
	(lambda (pattern-list)
	  (if (null? pattern-list)
	      '()
	      (cons
	       (let*
		   ((pattern (car pattern-list))
		    (token (vector-ref pattern 0))
		    (option (vector-ref pattern 2)))
		 (cond
		  ((equal? token `SEP)                 ;Is it a seperator?
		   (cond
		    ((equal? option "YES") "|")        ;Single seperator?
		    ((equal? option "DOUBLE") "||")    ;Double seperator?
		    (else "")))
		  ;It is a cell. Is it a normal cell?
		  ((equal? (vector-ref pattern 3) 'AUTO) 
		   (cond
		    ((equal? option 'RIGHT) "r")       ;oriantation right?
		    ((equal? option 'CENTER) "c")      ;centered?
		    (else "l")))                       ;left.
		  (else
		   (string-append '#"p{"
				  (string-append 
				   (number->string 
				    (/ (vector-ref pattern 3) 10))
				   '#"mm}")))))      
	       (write-tabular-pattern (cdr pattern-list))))))

       (write-table-tail '#"\n\\end{table}")
       
       (write-table-head
	(lambda (longest-pattern)
	  (cons 
	   #"\n\\begin{tabular}{"
	   (append
	    (write-tabular-pattern longest-pattern)
	    '(#"}\n")
	    ))))

       (write-rows
	(lambda (row-list longest-pattern)
	  (if (null? row-list)
	      '()
	      (letrec 
		  ((current-cells (vector-ref (car row-list) 1))
		   (current-pattern  (vector-ref (car row-list) 0)))
		(append
		 (if (equal? current-pattern 'SEP)
		     (cond
		      ((equal? current-cells "YES") '(#"\\hline\n"))
		      ((equal? current-cells "DOUBLE") '(#"\\hline \\hline\n"))
		      (else '(#"")))
		     (if (equal? (reverse (cdr current-pattern)) longest-pattern)
			 (write-simple-cells (reverse current-cells))
			 (write-multi-column (reverse current-cells) 
					    (reverse (cdr current-pattern))
					    longest-pattern)))
		 (write-rows (cdr row-list) longest-pattern))))))

       (write-simple-cells
	(lambda (cell-list)
	    (if (null? (cdr cell-list))
		(cons  (car cell-list) '(#"\\\\\n"))
		(cons (car cell-list)
		      (cons '#" & "
			    (write-simple-cells (cdr cell-list)))))))

       (write-multi-column-cell 
	(lambda (cell pattern seperator)
	  (cons 
	   '#"\\multicolumn{"
	   (cons
	    (number->string (vector-ref pattern 1))
	    (cons
	     '#"}{"
	     (cons
	      (cond
	       ((equal? (vector-ref pattern 3) 'AUTO) 
		(cond
		 ;oriantation right?
		 ((equal? (vector-ref pattern 2) 'RIGHT) "r") 
		 ;centered?
		 ((equal? (vector-ref pattern 2)'CENTER) "c")      
		 (else "l")))                       ;left.
	       (else
		(string-append '#"p{"
			       (string-append 
				(number->string 
				 (/ (vector-ref pattern 3) 10))
				'#"mm}"))))      
	      (cons
	       (cond
		;Single Seperator?
		((equal? (vector-ref seperator 2) "YES") "|")        
		;Double Seperator?
		((equal? (vector-ref seperator 2) "DOUBLE") "||")    
		(else ""))
	       (cons
		'#"}{"
		(cons
		 cell
		 '(#"} "))))))))))
	   

       (write-multi-column 
	(lambda (cell-list pattern longest-pattern)
	  (if (null? (cdr cell-list))
	      (append
	       (write-multi-column-cell (car cell-list) (car pattern)
				       (if (null? (cdr pattern))
					   '#(SEP 0 "NO" 0)
					   (cadr pattern)))
	       '(#" \\\\\n"))
	      (append 
	       (write-multi-column-cell (car cell-list) (car pattern)
				       (cadr pattern))
	       (cons
		'#" & "
		(write-multi-column (cdr cell-list) (cddr pattern) 
				   (cdr longest-pattern)))))))
       );letrec
       
    
    (let* 
	((rt (reverse (vector-ref t 0)))
	 (lp (longest-pattern rt (car rt))))
      (apply string-append
	     (append
	      (write-table-head lp)
	      (append
	       (append
		(write-rows (reverse(vector-ref t 1)) lp))
	       '(#"\n\\end {tabular}\n")))))))










