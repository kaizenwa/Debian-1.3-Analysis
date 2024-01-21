(message 1 "Loading Slide")
(set! sgml-opts (cons "-i Lout " (cons "-i Slide " sgml-opts)))
(set! *load-path* (append *typeset-lib* *load-path*))
(load "target/lout/preparse.scm")

(define-macro (divert-on) ''#(DIVERT POP))
(define-macro (divert-off) ''#(DIVERT "/dev/null"))

(define (slide-heads contents)
  (let* ((heading '())
	 (get-heading
	  (rdp-repll '()
		     (rdp-map
		      (lambda (h)
			(set! heading (accumulate cons '() h))
			empty-stream)
		      contents))))
    `((#(STARTTAG (H0 H1 H2 H3 H4 HP AH0 AH1 AH2 AH3 AH4))
       ,get-heading)
      (#(STARTTAG SLIDE)
       ,(rdp-repll
	 (lambda (t)
	   `((,(divert-on) #"\n@Overhead @Title{" ,@heading #"}\n@Begin\n")
	     (#"\n@End @Overhead\n" ,(divert-off))))
	 contents))
      (#(STARTTAG NEWPAGE)
       ,(rdp-repll
	 (lambda (t)
	   `((#"\n@NP @CenteredDisplay @Heading{" ,@heading #"}\n")))
	 contents))
      )))

(define slide-divs
  `(((SECT APPDX)
     (,(divert-on) #"\n@Lecture\n@Begin\n@BeginOverheads\n" ,(divert-off))
     (,(divert-on) #"\n@EndOverheads\n@End @Lecture\n" ,(divert-off)))
    ((DTTL RTTL BTTL)  "    @Title{ " (" }
//
" ,(divert-off)))

    ((SECT1 SECT2 SECTN APPDX1 APPDX2 APPDXN
      SECTS SECT1S SECT2S SECTNS APPDXS APPDX1S APPDX2S APPDXNS
      CPAPT CHAPTS
      INTRO PREFACE ABSTRACT))
    ))

(define slide-repls
  (letrec
      ((contents
	(rdp-cond*
	 (lout-desc
	  (old-lout-tbl
	   `(,lout-p-data
	     ,@(slide-heads (rdp-call contents))
	     (#(ENDTAG) ,rdp-leave)
	     . ,(def-repl
		  (rdp-call contents)
		  `(,slide-divs ,lout-repls1 ,lout-body)
		  `(,@(lout-figure (rdp-call contents))
		    (#(PLACE) ,(lambda (c h s) (rdp-reduce c h  (tail s))))
		    (#((PI OUTPUT)) ,pass-token-action)
		    (#() ,(lambda (c h s)
			    (message 0 #"\nUnhandled: " (head s))
			    (rdp-reduce c h (tail s))))))))))))
    contents))

(define (slide-all basic)
  ((rdp-cond
    `((#(STARTTAG (BOOK DOCUMENT REPORT)) ; all the possible docs
					; replaced by:
       ,(rdp-repll
	 (lambda (t)
	   (let ((author (lout-tr-string (xatv t 'author)))
		 (inst (lout-tr-string (xatv t 'inst))))
	     `(( ,lout-general-header
		 "
  @SysInclude { slidesf }			  # OverheadLayout extension
  @SysInclude { graph }
  @Include { mydefs }

@Use { @DocumentLayout
    @InitialFont	{ Times Base 20p	} # initial font
    @InitialBreak	{ ragged 1.2fx nohyphen	} # initial break
    @PageType		{ Other			} # page type (width, height)
    @PageWidth		{ 595p			} # page width if type Other
#    @PageHeight	{ 595p			} # page height if type Other
    @PageHeight		{ 842p			} # page height if type Other
#    @PageOrientation	{  Landscape	} # Portrait, Landscape, etc.
    @PageOrientation	{  Portrait	} # Portrait, Landscape, etc.
    @PageBoxType	{ CurveBox	} # None Box CurveBox ShadowBox
    @PageBoxMargin	{ 0.4c			} # page box margin
    @FootMargin		{ 5.00c		} # bottom margin of all pages
#    @OddLeftMargin	{  5.0c	} # left margin of odd pages
#    @EvenLeftMargin	{  5.0c	} # left margin of even pages
    @ListGap{ 1v }
    @PageHeaders	{ Titles	       } # None Simple Titles NoTitles
    @OddTop		{ @Right 8p @Font @PageNum	}
    @EvenTop		{ @Right 8p @Font @PageNum	}
    @StartOddTop	{ @Right 8p @Font @PageNum	}
    @StartEvenTop	{ @Right 8p @Font @PageNum	}
    @RunningOddTop  { 8p @Font {@MajorTitle @MinorNum (ctd.) @Right @PageNum}}
    @RunningOddFoot { 8p @Font { @Right " ,AUTHOR "}	      	     }
    @RunningEvenTop { 8p @Font {@MajorTitle @MinorNum (ctd.) @Right @PageNum}}
    @RunningEvenFoot{ 8p @Font { @Right " ,AUTHOR "}	      	     }
    @RunningStartOddTop	 { 8p @Font { @MajorTitle @MinorNum @Right @PageNum }}
    @RunningStartOddFoot { 8p @Font { @Right " ,AUTHOR "}		     }
    @RunningStartEvenTop { 8p @Font { @MajorTitle @MinorNum @Right @PageNum }}
    @RunningStartEvenFoot{ 8p @Font { @Right " ,AUTHOR "}		     }
}

@Use { @OverheadLayout }
@Use { @TypesetLayout }

@OverheadTransparencies
   @Author{ "      ,AUTHOR " }
   @Institution{ " ,INST " }
"))))
	 slide-repls))
       ))				; close the repl list
					; and the rdp-cond

     `(,(lambda (c h s) empty-stream))	; terminator
     (lambda nanu nanu)			; dummy
     basic				; the input
  ))

(set! compile-function
      (lambda (o d e)
	(with-output-to-file
	    (string-append d e)
	  (lambda ()
	    (doc-preprocess-hook 'run)
	    ((stream-display-diverted)
	     (stream-through
	      (token-stream o)
	      slide-all
	      all-doctypes))))))

(set! compile-function (lout-run-over compile-function))
