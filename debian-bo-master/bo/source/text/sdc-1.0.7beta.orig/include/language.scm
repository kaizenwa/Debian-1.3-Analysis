
(define words-en 
'(
  ( intro . "Introduction")
  ( abstract . "Abstract")
  ( author . "Author")
  ( here . "[here]" )
  ( content . "Contents" )
  ( bibl . "Bibliography" )
  ( figure . "Figure")
  ( table . "Table" )
  ( footnote . "Footnote" )
  ( footnotes . "Footnotes" )
  ( preface . "Preface" )
  ( chapt . "Chapter" )
  ( appendix . "Appendix" )
  ( index . "Index" )
  ( page . "page" )
))

(define words-de
'(( intro . "Einleitung")
  ( abstract . "Zusammenfassung")
  ( author . "Autor")
  ( here . "[hier]" )
  ( content . "Inhalt" )
  ( bibl . "Literatur")
  ( figure . "Abbildung" )
  ( table . "Tabelle" )
  ( footnote . "Fußnote" )
  ( footnotes . "Fußnoten" )
  ( preface . "Vorwort" )
  ( chapt . "Kapitel" )
  ( appendix . "Anhang" )
  ( index . "Index" )
  ( page . "Seite" )
))

; translate one word to the current language
; should be improved somtime

(define language-word-list '())
(define doc-lang "DE" )

(define (set-lang! args)
  (let* ((pp (assv 'LANG args))
	 (pl (if pp (cdr pp) #f)))
    (if pl
	(begin 
	  (set! doc-lang  pl)
	  (set! language-word-list 
		(cond
		 ((equal? pl "EN") words-en)
		 ((equal? pl "DE") words-de)))))))

(define (lw w)
  (let ((p (assq w language-word-list)))
    (if p
	(cdr p)
	(begin (warn 1 `(#"\nWarning: Word for " ,w " unknown."))
	""))))

(define (phrase lang)
  (let ((words
	 (cond
	  ((equal? lang "DE") words-de)
	  ((equal? lang "EN") words-en)
	  (else (message 0 "Language \"" lang "\" not defined in language.scm")
		words-en))))
    (lambda (phrase)
      (let ((p (assq phrase words)))
	(if p
	    (cdr p)
	    (symbol->string phrase))))))
