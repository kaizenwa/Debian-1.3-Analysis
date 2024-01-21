(game-module "insects"
  (blurb "Bugs, Mr Rico! Zillions of 'em!")
  (variants (see-all false) (world-seen true))
  )

(unit-type worker (char "w")
  (help ""))
(unit-type spider (char "s")
  (help ""))
(unit-type skeeter (char "k")
  (help "waterbug"))
(unit-type beetle (char "B")
  (help ""))
(unit-type queen (image-name "crown") (char "Q")
  (help ""))
(unit-type fly (char "f")
  (help ""))
(unit-type bee (char "b")
  (help ""))
(unit-type web (image-name "web2") (char "%")
  (help ""))
(unit-type mound (char "*")
  (help ""))
(unit-type nest (char "@")
  (help ""))

(material-type mobility (help ""))

(terrain-type puddle (char "-") (color "sky blue"))
(terrain-type field (char "+") (color "green"))

(define w worker)
(define s spider)
(define k skeeter)
(define B beetle)
(define Q queen)
(define f fly)
(define b bee)
(define % web)
(define * mound)
(define @ nest)

(define walkers ( worker spider skeeter beetle ))
(define movers ( worker spider skeeter beetle queen fly bee ))
(define flyers ( queen fly bee ))
(define land ( field ))

;;; Static relationships.

(table unit-capacity-x
  (spider web 4)
  )

(add queen capacity 2)
(add ( nest mound ) capacity 8)
(add web capacity 1)

(table unit-size-as-occupant
  (u* u* 99)
  (movers ( Q * @ ) 1)
  (walkers web 1)
  )

(table unit-storage-x add (web mobility 1))

;;; Actions.

;                     w s k B Q f b % * @
(add u* acp-per-turn (2 4 2 1 7 7 7 1 1 1))

;;; Movement.

(table mp-to-enter-terrain
  (u* t* 99)
  (walkers land 1)
  (skeeter puddle 1)
  (skeeter land 2)
  (flyers t* 1)
  (web t* 1)
  )

;; Web gets one move, for placement, then must stay there.

(table material-to-move (web mobility 1))

(table consumption-per-move (web mobility 1))

;;; Construction.

(add movers cp ( 2 4 10 30 25 2 10 ))
(add web cp 5)

(table acp-to-create
  (nest movers 1)
  (spider web 1)
  )

(table cp-on-creation
  (nest movers 1)
  (spider web 1)
  )

(table acp-to-build
  (nest movers 1)
  (spider web 1)
  )

(table cp-per-build
  (nest movers 1)
  (spider web 1)
  )

(table supply-on-creation (web mobility 1))

;FIXME 1 nest mound make

;(table material-to-build (nest queen 1))

;;; Combat.

;                 w   s   k   B   Q   f   b  %   *   @
(add u* hp-max ( 20  50  20 100  20  10  20  2  10  10 ))

;FIXME in addition you can set hp-to-repair and other things to cripple a unit
;(add movers hp-at-min-speed ( 1 3 1 1 1 1 1 ))

(table hit-chance
  ;               w   s   k   B   Q  f   b  %   *   @
  (worker u*  (  70  50  70  30  90  80 75 30 100 100 ))
  (spider u*  (  70  50  70  40  90  90 75 70 100 100 ))
  (skeeter u* (  70  50  70  30  90  80 75 50 100 100 ))
  (beetle u*  (  90  70  80  50  95  50 50 70 100 100 ))
  (queen u*   (  10  10  10  10  50  50 40 10 100 100 ))
  (fly u*     (  10  10  10  10  20  50 25 10  50  50 ))
  (bee u*     (  90  70  90  50  90  80 75 30 100 100 ))
  (web u*     (   0   0   0   0   0   0  0  0   0   0 ))
  (mound u*   (   0   0   0   0   0   0  0  0   0   0 ))
  (nest u*    (   0   0   0   0   0   0  0  0   0   0 ))
  )

(table damage
  (u* u* 1)
  (movers movers 10)
  )

;(add bee acp-to-detonate 1)

(table protection
  (nest worker 50)
  (mound nest 30)
  )

(table capture-chance
  (% movers ( 50 30 50 20 70 70 50 ))
  (movers mound 100)
  )

;FIXME "eats" movers destroy-message

;;; Random events.

;; Insects generally have short lifespans.

(table attrition (movers t* 50.00))

;;; Scoring.

(add nest point-value 25)

(scorekeeper (do last-side-wins))

;;; Random setup.

(add t* alt-percentile-min (   0    0 ))
(add t* alt-percentile-max (  10  100 ))
(add t* wet-percentile-min (  50    0 ))
(add t* wet-percentile-max ( 100  100 ))

(set edge-terrain puddle)

(add nest start-with 1)

(table independent-density (* t* 100))

(table favored-terrain (( * @ ) field 100))

(add u* already-seen 100)

(add nest initial-seen-radius 5)

(set side-library '(
  ((noun "Buzzer"))
  ((noun "Flitter"))
  ((noun "Flyer"))
  ((noun "Hummer"))
  ))

;FIXME 100 nest queen guard

;FIXME "expires" u* starve-message
;FIXME "dissolves" web starve-message

;;; Documentation.

(game-module (instructions (
  "When you defeat your enemies, you get to eat them!"
  ""
  "Capture the other sides' nests."
  )))

(game-module (notes (
  "The nest is the center of your world; produce workers to "
  "expand with, spiders to build webs for defense, and skeeters "
  "to cross water."
  )))

(game-module (design-notes (
  "This was originally designed by Chris Christensen,
   inspired by a board game called `Chiten I'(?)."
  )))
