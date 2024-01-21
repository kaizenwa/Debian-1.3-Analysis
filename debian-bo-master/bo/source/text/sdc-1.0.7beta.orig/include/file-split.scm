;;; file-split-info-c/as
;;; add split-info for chapters and appendices

(define (file-split-info-c/as files)
  (lambda (s)
    (let ((ids (gen-ns "Subfile ID's"))
	  (idx (gen-ns "Subfile Index ID's"))
	  (get-val (lambda (ids from)
		     (let*
			 ((v1 (if
			       (pair? from)
			       (cdr from)
			       ; (error "file info, get" "can't resolve" from)
			       "<nowhere>"))
			  (v2 (ids 'lookup v1)))
		       (if (pair? v2)
			   (cons (cdr v2) v1)
			   (error "file info, get" "no file bound to" v1)
			   ;`("<nofile>" . ,v1)
			   ))))
	  (xcadr (lambda n (cadr n)))
	  (xcadr-tail (lambda n (tail (cadr n))))
	  (gt-no (lambda (t old) (xatv t 'NO))))
      (letrec
	  ((g (rdp-cond*
	       `((#(ENDTAG (XREF INDEX FIGURE
			    SECT SECT1 SECT2 SECTN
			    APPDX APPDX1 APPDX2 APPDXN PART CHAPT
			    APPDXS
			    BOOK DOCUMENT REPORT)))
		 (#(STARTTAG CHAPT)
		  ,(rdp-let-fetch
		    `((FILES ,xcadr ,xcadr-tail))
		    (rdp-repll
		     `((,(lambda (t f)
			   (let* ((ida (xatv t 'NO))
				  (cf (head f)))
			     (ids 'bind cf ida)
			     `#(STARTTAG CHAPT
				(#(FILE TOKEN ,cf) . ,(token-args t)))))
			FILES)
		       ,identity)
		     (rdp-call g))))
		 (#(STARTTAG APPDXS)
		  ,(rdp-repll
		    `((,(lambda (t f)
			  `#(STARTTAG
			     ,(token-gi t)
			     (#(FILE TOKEN ,(head f)) . ,(token-args t))))
		       FILES)
		      ,identity)
		    (rdp-call g)))
		 (#(STARTTAG XREF)
		  ,(rdp-repll
		    `(,(lambda (t)
			 (let* ((ma (xat t 'MARK))
				(mav (arg-val ma)))
			   `#(STARTTAG XREF
			      (,(if
				 (eq? (arg-type ma) 'PROMISE)
				 `#(MARK PROMISE
				    ,(delay (get-val ids (force mav))))
				 `#(MARK TOKEN ,(get-val ids mav)))
			       . ,(token-args t)))))
		      ,identity)
		    (rdp-call g)))
		 (#(STARTTAG INDEX)
		  ,(rdp-repll
		    `((,(lambda (t f division)
			  (let* ((mark (xatv t 'MARK))
				 (cf (head f)))
			    (idx 'bind `(,cf . ,division) mark)
			    ;`#(STARTTAG INDEX
			    ;    (#(FILE TOKEN ,cf)
			    ;     #(DIVISION TOKEN ,division)
			    ;     . ,(token-args t)))
			    t
			    ))
		       FILES DIVISION)
		      ,identity)
		    (rdp-call g)))
		 (#(STARTTAG (SECT SECT1 SECT2 SECTN
				   APPDX APPDX1 APPDX2 APPDXN PART))
		  ,(rdp-let-fetch
		    `((DIVISION ,gt-no ,xcadr))
		    (rdp-repll
		     `((,(lambda (t f)
			   (let ((ma (xatv t 'NO))
				 (cf (head f)))
			     (ids 'bind cf ma)
			     `#(STARTTAG
				,(token-gi t)
				(#(FILE TOKEN ,cf) . ,(token-args t)))))
			FILES)
		       ,identity)
		     (rdp-call g))))
		 (#(STARTTAG FIGURE)
		  ,(rdp-repll
		    `((,(lambda (t f)
			  (let ((ma (xatv t 'NO))
				(cf (head f)))
			    (ids 'bind cf ma)
			    `#(STARTTAG
			       ,(token-gi t)
			       (#(FILE TOKEN ,cf) . ,(token-args t)))))
		       FILES)
		      ,identity)
		    (rdp-call g)))
		 (#(PLACE INDEX)
		  ,(rdp-map
		    (lambda (i)
		      (stream `#(PLACE INDEX (,(token-args (head i))
					      ,idx))))
		    pass-token-action))
		 (#(INDEXDB)
		  ,(rdp-map
		    (lambda (i)
		      (stream `#(INDEXDB (,(data-token-data (head i))
					  ,idx))))
		    pass-token-action))
		 (#() ,pass-token-action))))
	   (no-filenames (lambda () (cons-stream "" (no-filenames)))))
	(rdp-parse
	 (rdp-cond
	  `((#(STARTTAG BOOK)
	     ,(rdp-let `((FILES ,(lambda i files) ,identity))
		       (rdpp-keep g)))
	    (#(STARTTAG)
	     ,(rdpp-keep g))))
	 s
	 `(FILES ,(no-filenames))
	 `(DIVISION "")
	 )))))
