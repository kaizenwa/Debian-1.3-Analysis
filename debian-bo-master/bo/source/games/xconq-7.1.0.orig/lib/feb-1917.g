(game-module "feb-1917"
  (title "February Revolution")
  (blurb "Russian revolution of february 1917")
  (base-module "russian-rev")
  (variants
    (see-all false)
  )
)

(set terrain-seen true)

(include "u-rus-1910")

(set synthesis-methods '(make-initial-materials))

(set calendar ("usual" "day"))
(set initial-date "12 Mar 1917") ; but february, old style

(side 1 (name "Russian Empire") (noun "Czarist")
        (color "blue") (emblem-name "arms-russia"))
(side 2 (name "The Revolution") (adjective "revolutionary")
        (color "red") (emblem-name "hammer-and-sickle"))
(side 3 (name "Germany") (noun "German") 
        (color "gray,black,white") (emblem-name "german-cross"))

(unit "Tashkent" (s 1))
(unit "Petrograd" (s 2))
(unit "Warszawa" (s 3))

(game-module (design-notes (
  "This scenario is under developement;"
  "Massimo Campostrini (campo@sunthpi3.difi.unipi.it)."
  ""
  "Really set up the forces in february 1917."
)))
(game-module (notes (
  "Under contruction."
)))