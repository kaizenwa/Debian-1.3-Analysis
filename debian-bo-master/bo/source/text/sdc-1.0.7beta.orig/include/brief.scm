(define p-brief-ren
  (let ((self (rdp-call p-brief-ren)))
    (rdp-cond*
     `((#(ENDTAG))
       (#(STARTTAG DISP) ,(rdpp-rename 'QUOTE self))
       (#(STARTTAG LISTE) ,(rdpp-rename 'LIST self))
       (#(STARTTAG PKT) ,(rdpp-rename 'O self))
       (#(STARTTAG) ,(rdpp-keep self))
       ,(pass-otherwise)
       ))))

(define p-adr
  (let ((tatv (lambda (t sym)
		(lout-tr-string (xatv t sym)))))
    (rdp-repll
     (lambda (t)
       `(("
@ADDR
titel{" ,(tatv t 'TITEL) "}
vorname{" ,(tatv t 'VORNAME) "}
name{" ,(tatv t 'NAME) "}
firma{" ,(tatv t 'FIRMA) "}
strasse{" ,(tatv t 'STRASSE) "}
ort{" ,(tatv t 'ORT) "}
plz{ " ,(xatv t 'PLZ) "}
telefon{" ,(tatv t 'TELEFON) "}
telefax{" ,(tatv t 'TELEFAX) "}
")
#f))
     rdp-leave)))

(define p-adresse (rdp-repll '(#f #f) p-adr))

(define p-brief
  (rdp-repll
   `(
(#(STARTTAG BRIEF)
#"@SysInclude{\"brief.lout\"}
@Text @Begin
@Marke fenster{" ,(lambda (t) (xatv t 'FENSTER)) "}
")
(#"
@End @Text
" #(ENDTAG BRIEF)))
   (rdp-process p-brief-ren)
   (rdp-cond
    `((#(STARTTAG VON) ,(rdp-repll `(#\newline #"{Absender}\n") p-adresse))))
   (rdp-map
    (lambda (logo an inserted)
     (stream-append an inserted logo))
    (rdp-cond
     `((#(STARTTAG LOGO) ,(rdp-repll `(#\newline #"{Absender}\n") p-text))))
    (rdp-cond
     `((#(STARTTAG AN) ,(rdp-repll `(#\newline #"{Adresse}\n") p-adresse))))
    (rdp-insert #"\n@JFWBrief @Abs @Adr\n"))
   (rdp-cond
    `((#(STARTTAG DATUM) ,(rdp-repll `(#"\n@Datum{" #"}") p-text))))
   (rdp-cond
    `((#(STARTTAG BETR) ,(rdp-repll `(#"\n@Betreff{" #"}") p-text))))
   (rdp-cond
    `((#(STARTTAG ANREDE) ,(rdp-repll `(#"\n&0c " #"\n@LP\n") p-text))))
   (rdp-cond
    `((#(STARTTAG TEXT) ,(rdp-repll `(#f #f) p-body*))))
   (rdp-cond
    `((#(STARTTAG GRUSS) ,(rdp-repll `(#"\n// @Gruss{" "}") p-text))))
   (rdp-cond
    `((#(STARTTAG ANLAGE) ,(rdp-repll `(#"\n@Anlage{"   #"}\n" ) p-body*))))
   ))
