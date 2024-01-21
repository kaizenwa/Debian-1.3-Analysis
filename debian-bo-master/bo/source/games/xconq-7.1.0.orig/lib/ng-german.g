;;; German-like place name generator.

;;; Conventional combos most common, random syllables rare.
;;; Needs more conventional words to combine?

(namer german-place-names (grammar root 100
  (root (or 95 (name)
             5 ("Bad " name)
	     ))
  (name (or 40 (prefix suffix)
            20 (both suffix)
            20 (prefix both)
	     5 (prefix both suffix)
	    10 (syll suffix)
	    10 (prefix syll suffix)
	))
  (prefix (or
	schwarz blau grun gelb rot roth braun weiss
	wolf
	neu alt alten
	salz
	hoch uber nieder gross klein
	west ost nord sud
	;; from real names
	frank dussel chem stras mut
	))
  (suffix (or
	dorf torf heim holz hof burg stedt haus hausen
	bruck brueck bach tal thal furt
	;; these aren't so great
	ach ingen nitz
	))
  (both (or
	feld stadt stein see schwein schloss wasser eisen berg
	))
  (syll (or 40 (startsyll vowel endsyll) 5 (vowel endsyll)))
  (startsyll (or 30 startcons 10 startdiph))
  (startcons (or b k d f g l m n r 5 s 3 t))
  (startdiph (or bl kl fl gl 5 sl 3 sch 2 schl
                 br dr kr fr gr 2 schr 3 tr 2 th 2 thr))
  (vowel (or 6 a ae 2 au 5 e 2 ei 2 ie 6 i 3 o oe 2 u ue))
  (endsyll (or 4 b 5 l 3 n 4 r 4 t
               bs ls ns rs ts
               3 ch 3 ck
               lb lck lch lk lz ln lt lth ltz
               rb rck rch rn rt rth rtz
               ss sz
               2 th tz
  ))
))

; cons vowel cons [vowel] (usually en em es)

; cons vowel (or ss rr ll) vowel [cons]

;;; Should include a personal name generator that will do "Jacob von
;;; Dittersdorfnachspieleruntermunter... von Ulm" :-)

