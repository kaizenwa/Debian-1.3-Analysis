; ------ general ------------------------------------------------------------

(define english-quotation-style '("EN"))

; ------ HTML ---------------------------------------------------------------
(define html-tbl-writer-function 'html3-write-tbl)

; ------ man ----------------------------------------------------------------

(define tbl-prog "tbl")

(define nroff-prog "nroff")

; ------ Lout ---------------------------------------------------------------
(define lout-initial-break "adjust 1.20fx hyphen" )
(define lout-initial-font "Times Base 10p" )

; Ensure that inner-margin + outer-margin = both-margin

(define lout-inner-margin "3.5c" )
(define lout-outer-margin "1.5c" )
(define lout-both-margin "2.5c" )

; Pagetype is one of:
; Letter, Tabloid Ledger, Legal, Statment, Executive, 
; A3, A4, A5, B4, B5, Folio, Quarto, 10x14

(define lout-pagetype "A4" )

(define (lout-language-adjust-formatting lang)
  (set-cdr! (assq 'SQ final-replacement-alist)
	    (cond
	     ((equal? lang "EN") '#("``" "''"))
	     ((equal? lang "DE") '#(",," "''"))
	     (else '#(",," "''")))))

;----------------------------------------------------------------------------
; LaTeX
;----------------------------------------------------------------------------

; latex-latex-type is either LATEX209 or LATEX2E

(define latex-latex-type 'LATEX2E)

(define latex-styleoptions (if (eq? latex-latex-type 'LATEX209)
			       '("epsfig")
			       '()))

; a list of packages and their options for the \usepackage
; command. Don't forget the braces!

(define latex-packages '("[dvips]{graphics}"))

; the following is from deutsch.sty

(define latex-german-preamble "
%----------------------------------------------%
% no dot begind numbers in heading (pagestyle): (report, book)
%----------------------------------------------%
\\def\\@headdot{}
 
%
% Paragraphen in deutschen Texten: kein Einzug der ersten Zeile,
%                                  Abstand zwischen zwei Paragraphen
%
\\parindent 0pt
\\parskip 5pt plus 1pt minus 2pt
 
%
% quotation-environment soll gleiche Werte f\"ur Paragraphen-behandlung
%    erhalten wie au\"serhalb auch.
%                                                         MW 08.08.1988
\\def\\quotation{\\list{}{\\listparindent\\parindent
 \\itemindent\\listparindent
 \\rightmargin\\leftmargin \\parsep\\parskip}\\item[]}

\\frenchspacing
\\umlautlow
")

(define latex-a4-preamble "
% DIN_A4.TEX/STY: set pagesize to DIN A4
%
\\ifx\\textheight\\undefined% then we are in NOT latex: assume plain etc.
\\message{DIN A4 for Plain and AmS TeX}%
\\voffset 0.3333333in
\\hsize 15.9424103cm
\\vsize 24.6501778cm
\\else
\\message{DIN A4 for LaTeX}%
\\oddsidemargin 0pt \\evensidemargin 0pt \\topmargin 0pt
\\textwidth 15.9424103cm
\\textheight 24.6501778cm
\\advance\\textheight -\\topmargin
\\advance\\textheight -\\headheight \\advance\\textheight -\\headsep
\\fi
% -eof-
")

(define latex-preamble (list latex-a4-preamble))
