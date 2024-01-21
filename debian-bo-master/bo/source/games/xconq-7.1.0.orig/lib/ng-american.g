;;; American names tend to be a hodgepodge of Indian, Spanish, English, etc,
;;; sometimes even mixed together, and usually distorted to boot.

(namer american-place-names (grammar root 9
  (root (or amerindian-name
	    english-name
	    spanish-name
	))
  (amerindian-name ("?"))
  (english-name ("?"))
  (spanish-name (or saint-name spanish-word ("los " spanish-word)))
  (saint-name ("San " (or jose francisco lucas joao juan luis miguel)))
))

