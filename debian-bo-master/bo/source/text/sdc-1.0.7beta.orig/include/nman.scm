(define (p-manpage->xxx xxx id)
  (let* ((author "an unknown author")	; to be overridden.
	 (end '#(ENDTAG DIVISION))
	 (exc (lambda (t)
		(set! author (xatv t 'AUTHOR)) ; extract for later use
		`#(STARTTAG
		   ,xxx 
		   (,@(if id (list id) '())
		    #(LANG TOKEN ("EN")) . ,(token-args t)))))
	 (end-doc `#(ENDTAG ,xxx))
	 (ren (lambda (t)
		`#(STARTTAG
		   DIVISION
		   (#(ID IMPLIED #f)
		    #(LANG TOKEN ("EN")) . ,(token-args t)))))
	 (s-division '#(STARTTAG
			DIVISION
			(#(ID IMPLIED #f) #(LANG TOKEN ("EN")))))
	 (s-hd '#(STARTTAG HEADING ()))
	 (e-hd '#(ENDTAG HEADING))
	 (sect (lambda (name what . stuff)
		 `(#(STARTTAG ,name)
		   ,(rdp-repll
		     `(,ren ,end)
		     (apply rdp-insert `(,s-hd ,@stuff ,e-hd))
		   what))))
	 )
    (let
	((gram
	  (rdp-cond
	   `((#(STARTTAG MANPAGE)
	      ,(rdp-repll
		`(,exc ,end-doc)
		(letrec
		    ((call (rdp-call stuff))
		     (stuff (rdp-cond*
			    `((#(STARTTAG SECT1)
			       ,(rdp-repll `(,ren ,end) call))
			      (#(STARTTAG) ,(rdpp-keep call))
			      (#(ENDTAG) ,rdp-leave)
			      (#() ,(lambda (c h s)
				      (cons-stream
				       (head s)
				       (rdp-reduce c h (tail s))))))))
		     (gram
		      (rdp-cond*
		       `(
			 ; if we compile into a manpage doctype,
			 ; don't touch the heading
			 (#(STARTTAG TITLE)
			  ,(if (eq? xxx 'MANPAGE)
			       (rdpp-keep call)
			       (rdp-repll '(#(STARTTAG HEADING ())) call)))
			 (#(STARTTAG SHORT)
			  ,(if (eq? xxx 'MANPAGE)
			       (rdpp-keep call)
			       (rdp-repll `(#(DATA " -- ")
					    #(ENDTAG HEADING)) call)))
			 ,(sect 'SYNOPSIS call '#(DATA "SYNOPSIS"))
			 ,(sect 'CONFIG   call '#(DATA "CONFIGURATION"))
			 ,(sect 'DESCRIPT call '#(DATA "DESCRIPTION"))
			 ,(sect 'OPTIONS  call '#(DATA "OPTIONS"))
			 ,(sect 'RETURN   call '#(DATA "RETURN CODE"))
			 ,(sect 'ERRORS   call '#(DATA "ERRORS"))
			 ,(sect 'EXAMPLES call '#(DATA "EXAMPLES"))
			 ,(sect 'ENV      call '#(DATA "ENVIRONMENT"))
			 ,(sect 'FILES    call '#(DATA "FILES"))
			 ,(sect 'CONFORM  call '#(DATA "CONFORMING TO"))
			 ,(sect 'NOTES    call '#(DATA "NOTES"))
			 ,(sect 'DIAG     call '#(DATA "DIAGNOSTICS"))
			 ,(sect 'RESTRICT call '#(DATA "RESTRICTIONS"))
			 ,(sect 'HISTORY  call '#(DATA "HISTORY"))
			 (#(STARTTAG SEE)
			  ,(rdp-call	; delay to eval "author"
			    (rdp-insert s-division
					'#(STARTTAG HEADING ())
					'#(DATA "AUTHOR")
					'#(ENDTAG HEADING)
					'#(DATA "This was written by ")
					`#(DATA ,author)
					'#(DATA ".")
					'#(ENDTAG DIVISION)))
			  ,(rdp-repll
			    `(,ren ,end)
			    (rdp-insert s-hd '#(DATA "SEE ALSO") e-hd)
			    call))
			 (#(STARTTAG SECT)
			  ,(rdp-repll `(,ren ,end) call))
			 (#(ENDTAG MANPAGE))
			 (#() ,(lambda (c h s)
				 (message 0 "Don't know what " (head s)
					  " should be.")))
			 ))))
		  gram)))))))
      gram)))

