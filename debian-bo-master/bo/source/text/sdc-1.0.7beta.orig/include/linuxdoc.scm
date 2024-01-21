(define (data->string s)
  (apply string-append (map data-token-data (filter is-data-token? s))))

(define skip-element
  (rdp-cond*
   `((#(ENDTAG)
      (#(STARTTAG) ,(rdp-repll '() (rdp-call skip-element)))
      (#() ,(rdp-skip 1))))))

(define linuxdoc-descrip
  (let ((text (rdp-call linuxdoc-text)))
    (rdp-repll
     '(#(STARTTAG DESC ()) #(ENDTAG DESC))
     (rdp-cond*
      `((#(STARTTAG TAG) ,(rdp-repll '(#(STARTTAG DT ()) #(ENDTAG DT)) text)
			 ,(rdp-wrap '#(STARTTAG DD ()) '#(ENDTAG DD) text)
			 )
	(#(STARTTAG P) ,(rdp-skip 1))
	(#(ENDTAG P) ,(rdp-skip 1))
	(#(DATA) ,(rdp-skip 1))
	)))))

(define linuxdoc-text
  (rdp-cond*
   `((#((DATA PI)) ,pass-token-action)
     (#(ENDTAG))
     (#(STARTTAG (TSCREEN LQ))
      ,(rdp-repll '(#(STARTTAG QUOTE (#(STYLE TOKEN ("DEFAULT"))))
		    #(ENDTAG QUOTE))
		  (rdp-call linuxdoc-text)))
     (#(STARTTAG P)
      ,(rdp-repll '((#(STARTTAG P ()) #(ENDTAG P)))
		  (rdp-call linuxdoc-text)))

     (#(STARTTAG ITEMIZE) ,(rdp-repll '(#(STARTTAG LIST ()) #(ENDTAG LIST))
				      (rdp-call linuxdoc-text)))

     (#(STARTTAG DESCRIP) ,linuxdoc-descrip)

     (#(STARTTAG SF) ,(rdp-repll '()
				 (rdp-call linuxdoc-text)))
     (#(STARTTAG CPARAM) ,(rdp-repll '(#(STARTTAG TT ()) #(ENDTAG TT))
				     (rdp-call linuxdoc-text)))
     (#(STARTTAG SL) ,(rdp-repll '(#(STARTTAG IT ()) #(ENDTAG IT))
				 (rdp-call linuxdoc-text)))

     (#(STARTTAG REF)
      ,(rdp-repll (if (equal? doc-output "html")
		      (lambda (t)
			`((#(STARTTAG REF (#(T TOKEN ("X")) . ,(token-args t)))
			   #(DATA ,(xatv t 'NAME)))
			  #(ENDTAG REF)))
		      (lambda (t)
			`((#(DATA ,(xatv t 'NAME))
			   #(DATA " (")
			   #(STARTTAG REF (#(T TOKEN ("X")) . ,(token-args t))))
			  (#(ENDTAG REF) #(DATA ")")))))
		  rdp-leave))

     (#(STARTTAG PAGEREF)
      ,(rdp-repll (lambda (t) `(" Page of " ,(xatv t 'ID))) rdp-leave))

     (#(STARTTAG URL)
      ,(rdp-repll (if (equal? doc-output "html")
		      (lambda (t)
			`((#(STARTTAG REF (#(T TOKEN ("U"))
					   #(ID CDATA ,(xatv t 'URL))))
			   #(DATA ,(xatv t 'NAME)))
			  #(ENDTAG REF)))
		      (lambda (t)
			`((#(DATA ,(xatv t 'NAME))
			   #(DATA " (")
			   #(STARTTAG REF (#(T TOKEN ("U"))
					   #(ID CDATA ,(xatv t 'URL))))
			   #(ENDTAG REF)
			   #(DATA ") "))
			  #f)))
		  rdp-leave))

     (#(STARTTAG HTMLURL)		; nur id, wenn html
      ,(rdp-repll (if (equal? doc-output "html")
		      (lambda (t)
			`((#(STARTTAG REF (#(T TOKEN ("U"))
					   #(ID CDATA ,(xatv t 'URL))))
			   #(DATA ,(xatv t 'NAME)))
			  #(ENDTAG REF)))
		      (lambda (t)
			`(#(DATA ,(xatv t 'NAME)))))
		  rdp-leave))
     (#(STARTTAG (IDX CDX))
      ,(rdp-map1
	(lambda (id)
	  (stream `#(STARTTAG INDEX (#(ID CDATA ,(data->string id))
				     #(SUB IMPLIED #f)))
		  '#(ENDTAG INDEX)))
	(rdp-repll '() (rdp-call linuxdoc-text))))

     (#(STARTTAG COMMENT)
      ,(rdp-repll '(#(STARTTAG NOTE ()) #(ENDTAG NOTE))
		  (rdp-call linuxdoc-text)))

     (#(STARTTAG (PH EPS HLINE TABULAR F DM EQ))
      ,(rdp-repll (lambda (t)
		    (message 0 "Skipping element " t)
		    '())
		  (rdp-call linuxdoc-text)))

     ; BE CAREFULL ABOUT THE FOLLOWING LINES
     (#(STARTTAG (TAG)))
     (#(STARTTAG (SECT SECT1 SECT2 CHAPT)))
     (#(STARTTAG) ,(rdpp-keep (rdp-call linuxdoc-text))))))

(define linuxdoc-heading
  (rdpp-keep linuxdoc-text))

(define linuxdoc-header
  (rdp-cond
   `((#(STARTTAG HEADER) ,(rdp-repll '() skip-element)))))

(define linuxdoc-garbage
  (rdp-cond*
   `((#(STARTTAG (TOC LOF LOT)) ,(rdp-skip 2)))))

(define linuxdoc-division
  (rdp-begin
   (rdp-map
    (lambda (div heading)
      (let* ((heading (stream->list heading))
	     (labels (filter (start-gi? '(LABEL)) heading))
	     (id (if (eq? labels empty-stream)
		     '#(ID IMPLIED #f)
		     `#(ID CDATA ,(xatv (car labels) 'ID)))))
	(apply
	 stream
	 `#(STARTTAG DIVISION (,id #(LANG TOKEN ("EN"))))
	 (filter (lambda (t) (not (or ((start-gi? '(LABEL)) t)
				      ((end-gi? '(LABEL)) t))))
		 heading))))
    pass-token-action			; the division token
    linuxdoc-heading)
   linuxdoc-header
   linuxdoc-text
   (rdp-cond*
    `((#(STARTTAG (SECT SECT1 SECT2)) ,(rdp-call linuxdoc-division))))
   (rdp-skip 1)				; the original end tag (check?)
   (rdp-insert '#(ENDTAG DIVISION))))

(define linuxdoc->report
  (rdp-repll
   '(#f #f)
   (rdp-cond
    `((#(STARTTAG ARTICLE)
       ,(rdp-repll
	 '(#f #(ENDTAG REPORT))
	 (rdp-cond
	  `((#(STARTTAG TITLEPAG)
	     ,(rdp-repll
	       '()
	       (rdp-map
		(lambda (title author date abstract)
		  (stream-append
		   (stream
		    `#(STARTTAG
		       REPORT
		       (#(AUTHOR CDATA ,(data->string (stream->list author)))
			#(DATE CDATA  ,(data->string (stream->list date)))
			#(LANG TOKEN ("EN"))
			#(INST CDATA "")
			#(FACE TOKEN ("1C"))))
		    '#(STARTTAG HEADING ()))
		   title
		   (stream '#(ENDTAG HEADING))
		   abstract))
		(rdp-repll '() linuxdoc-text)
		(rdp-repll '() linuxdoc-text)
		(rdp-repll '() linuxdoc-text)
		(rdpp-keep linuxdoc-text)))
	     )))
	 linuxdoc-header
	 linuxdoc-garbage
	 linuxdoc-text
	 (rdp-cond*
	  `((#(STARTTAG SECT) ,linuxdoc-division)
	    (#(STARTTAG APPENDIX)
	     ,(rdp-skip 2)
	     ,(rdp-wrap '#(STARTTAG APPENDIX ()) '#(ENDTAG APPENDIX)
			(rdp-cond*
			 `((#(STARTTAG SECT) ,linuxdoc-division)
			   (#(ENDTAG))
			   (#() ,(rdp-watch #"\n linuxdoc: Took wron turn ")
				,(rdp-skip 1))))))
	    (#(ENDTAG))
	    (#() ,(rdp-watch #"\nlinuxdoc: Took wrong turn ")
		 ,(rdp-skip 1))
	    ))))))))
