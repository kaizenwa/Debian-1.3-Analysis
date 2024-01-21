(game-module "hill"
  (title "King of the Hill")
  (blurb "Be 10 years old again!")
)

(set see-all true)

(unit-type kid (image-name "person") (help "a participant"))

(terrain-type grass (char "+") (color "green"))
(terrain-type hill (char "^") (color "sienna"))

(add kid acp-per-turn 2)

(table mp-to-enter-terrain (kid hill 2))

(add kid hp-max 99)

;; The parameters here simulate kids shoving each other.

;false counterattack

;false kid can-counter

(table withdraw-chance-per-attack
  (kid kid 50)
  )

(table hit-chance (kid kid 90))

(table damage (kid kid 1))

;; Harder to get at a kid that's on the hill.

(table defend-terrain-effect
  (kid hill 70)
  )

;80 kid retreat

;; Get any hp losses back always.

(add kid hp-recovery 99.00)

(set action-messages '(
  (attack done "%actor shoves %u1!")
  ))

;(set action-special-effects '(
;  (attack done (defender "oof"))
;  )

(game-module (notes 
  "This period `implements' 10-year-old kids.  They can hit each other,"
  "but damage is very minor, and soon recovered from.  The most likely result"
  "is for one or both to run away."
))

(area 30 21 (terrain
  "30a"
  "30a"
  "30a"
  "30a"
  "30a"
  "30a"
  "30a"
  "30a"
  "30a"
  "30a"
  "15ab14a"
  "30a"
  "30a"
  "30a"
  "30a"
  "30a"
  "30a"
  "30a"
  "30a"
  "30a"
  "30a"
))

(side 1 (name "Garbage Pail Kids"))

(side 2 (name "Deadend Kids"))

(kid (n "Tommy") (@ 7 10) (s 1))
(kid (n "Jenny") (@ 7 9) (s 1))
(kid (n "Stanley") (@ 7 11) (s 1))
(kid (n "Barry") (@ 6 10) (s 1))
(kid (n "Charlie") (@ 8 10) (s 1))
(kid (n "Mikey") (@ 6 11) (s 1))
(kid (n "Marty") (@ 8 9) (s 1))
(kid (n "Cathy") (@ 9 10) (s 1))
(kid (n "Joey") (@ 23 10) (s 2))
(kid (n "Leslie") (@ 24 9) (s 2))
(kid (n "Timmy") (@ 22 11) (s 2))
(kid (n "Harry") (@ 24 10) (s 2))
(kid (n "Bobby") (@ 22 10) (s 2))
(kid (n "Julie") (@ 23 11) (s 2))
(kid (n "Jimmy") (@ 23 9) (s 2))
(kid (n "Patti") (@ 21 10) (s 2))

;;; The scorekeeper (should) gives the win to the occupier of the hill after
;;; some number of turns has passed.
