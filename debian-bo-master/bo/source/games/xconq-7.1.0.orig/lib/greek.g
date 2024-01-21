(game-module "greek"
  (title "Ancient Greece")
  (blurb "Classical Greece, 500-350 BC")
  (variants
   (world-seen true)
   (see-all false)
   (world-size)
   ("Keep Score" keep-score
     (true (scorekeeper (do last-side-wins))))
   )
  )

(unit-type peltast
  (help "light fast infantry"))
(unit-type hoplite
  (help "heavy destructive infantry"))
(unit-type archer
  (help "archers and slingers"))
(unit-type cavalry
  (help "more like light cavalry"))
(unit-type trireme (char "T")
  (help "three-decked ship - mainstay of the navies"))
(unit-type |siege engine| (image-name "catapult") (char "S")
  (help "bashes cities (slowly)"))
(unit-type fortifications
  (help "augments a polis' defense"))
(unit-type polis (image-name "walltown") (char "*")
  (help "typical city-state"))
(unit-type metropolis (image-name "parthenon") (char "@")
  (help "a large and powerful city"))

(define p peltast)
(define h hoplite)
(define a archer)
(define c cavalry)
(define T trireme)
(define S |siege engine|)
(define / fortifications)
(define * polis)
(define @ metropolis)

(add / image-name "camp")

(material-type food
  (help "everybody needs food to survive"))
(material-type talents
  (help "the unit of big money"))

(terrain-type sea (color "sky blue") (char "."))
(terrain-type plains (color "green") (char "+"))
(terrain-type forest (color "forest green") (char "%"))
(terrain-type desert (color "yellow") (char "~"))
(terrain-type mountains (color "sienna") (char "^"))
(terrain-type river (color "blue") (char "-")
  (subtype border))

(define cities (* @))
(define places (/ * @))
(define ship-u* (trireme))
(define land-u* (p h a c S))
(define movers (p h a c T S))

(define water (sea river))
(define land (plains forest desert mountains))

;;; Static relationships.

(table vanishes-on
  (land-u* water true)
  (places water true)
  (ship-u* land true)
  )

(add t* capacity 4)

(table unit-size-in-terrain
  (u* t* 1) ; isn't this the default?
  )

;; Cities and ships have relatively limited capacity.

(add u* capacity 0)

(add cities capacity 8)

(add trireme capacity 4)

(table unit-size-as-occupant
  (u* u* 99)
  ((p h a c) trireme (2 2 1 4))
  (movers cities 1)
  )

;(table unit-capacity-x
;  (cities / 4)
;  )

;;; Unit-material capacities.

(table unit-storage-x
  (u* food (2 2 2 2 100 0 100 100 100))
  (u* talents (0 0 0 0 120 0 1000 3000 9000))
  )

;;; Vision.

;; A month's time is sufficient for news about cities
;; to get around everywhere.

(add cities see-always true)

;;; Actions.

;; A turn is a whole month, so lots can happen.

(add u* acp-per-turn (16 12 16 32 24 4 0 1 1))

;;; Movement.

(add places speed 0)

(table mp-to-enter-terrain
  (land-u* water 99)
  (land-u* mountains 2)
  (ship-u* land 99)
  )

;;; Construction.

(add u* cp (20 20 3 12 6 12 1 1 1))

(table acp-to-create
  (* movers 1)
  (@ movers 1)
  )

(table cp-on-creation
  (* movers 1)
  (@ movers 1)
  )

(table acp-to-build
  (* movers 1)
  (@ movers 1)
  )

(table cp-per-build
  (* movers 1)
  (@ movers 1)
  )

;; Production.

(table base-production
  ((p h a c) food 1)
  (cities food 10)
  )

(table productivity
  (u* (desert mountains) 0)
  )

(table base-consumption
  ((p h a c) food 1)
  (cities food 10)
  )

(table hp-per-starve
  ((p h) food 10)
  ((a c) food 1)
  (cities food 4)
  )

;;; Combat.

;;               p  h  a  c  T  S  /  *  @
(add u* hp-max (20 20  1  1 10  2 10 20 40))

(table acp-to-attack
  (u* u* 8)
  (S  u* 4)
  )

(table hit-chance
;;        p  h  a  c  T  S  /  *  @
  (p u* (40 20 50 40 10 30 10  0  0))
  (h u* (60 40 50 50 70 50 70 20 10))
  (a u* (50 20 30 30 60 30  0  0  0))
  (c u* (60 30 50 50 50 60 50  0  0))
  (T u* ( 0  0  0  0  0 30  0  0  0))
  (S u* ( 0  0  0  0  0  5 99 99 99))
  (/ u* ( 0  0  0  0 20  0  0  0  0))
  (* u* ( 0 40  0  0  0 10  0  0  0))
  (@ u* ( 0 80  0  0  0 20  0  0  0))
  )

(table damage
  (u* u* 1)
  (h u* 2)
  (S cities 4)
  )

(table capture-chance
  ;; Only hoplites can capture anything.
  (h S 50)
  (h * 10)
  (h @ 5)
  )

;;; Backdrop.

;; 1000 c attrition

;; The mere presence of a hoplite army might cause the city
;; to change sides.

(table surrender-chance
  (cities h (10 5))
  )

(table surrender-range
  (cities h 1)
  )

;;; Random generation.

(set alt-blob-density 10000)
(set alt-blob-size 40)
(set alt-smoothing 1)
(set wet-blob-density 2000)
(set wet-blob-size 100)

(add t* alt-percentile-min (  0  70  70  70  90 0))
(add t* alt-percentile-max ( 70  90  90  90 100 0))
(add t* wet-percentile-min (  0  20  80   0   0 0))
(add t* wet-percentile-max (100  80 100  20 100 0))

(set edge-terrain sea) ; the river Oceanus

(add @ start-with 1)
(add h start-with 1)
(add T start-with 1)
(add * independent-near-start 3)
;; Try to get countries on the coast.
(add (sea plains) country-terrain-min (1 1))

(table independent-density
  (* (plains desert) (100 50)))

(table favored-terrain
  (u* t* 100)
  (u* sea 0)
  (ship-u* sea 100)
  )

;;; (should set up a BC calendar)

(world 2500 (year-length 12))  ; big world, can't circumnavigate.

;;; An assortment of city-states.  The famous ones are weighted more heavily.

(set side-library '(
  (10 (name "Athens") (adjective "Athenian"))
  (10 (name "Sparta") (adjective "Spartan"))
  (5 (name "Corinth") (adjective "Corinthian"))
  (5 (name "Thebes") (adjective "Theban"))
  (3 (name "Argos") (adjective "Argive"))
  (2 (name "Megara") (adjective "Megaran"))
  (2 (name "Miletus") (adjective "Miletan"))
  (2 (name "Messene") (adjective "Messenian"))
  (2 (name "Syracuse") (adjective "Syracusan"))
  (2 (name "Ephesus") (adjective "Ephesian"))
  (2 (name "Delos") (adjective "Delian"))
  ((name "Lemnos") (adjective "Lemnian"))
  ((name "Ambracia") (adjective "Ambraciot"))
  ((name "Phokia") (adjective "Phokian"))
  ((name "Chios") (adjective "Chian"))
  ((name "Gelos") (adjective "Geloan"))
  ((name "Caria") (adjective "Carian"))
  ((name "Lokria") (adjective "Lokrian"))
  ((name "Melos") (adjective "Melian"))
  ((name "Phlias") (adjective "Phliasian"))
  ((name "Samos") (adjective "Samian"))
  ((name "Thuria") (adjective "Thurian"))
  ((name "Tegea") (adjective "Tegean"))
  ((name "Dolope") (adjective "Dolopian"))
  ((name "Olynthia") (adjective "Olynthian"))
  ((name "Elis") (adjective "Elean"))
  ((name "Lucania") (adjective "Lucanian"))
  ((name "Kythera") (adjective "Kytheran"))
  ((name "Skios") (adjective "Skionian"))
))

(game-module (notes (
  "This time strictly covers about 500 to 350 B.C.  Land warfare"
  "was most significant, with some notable sea-fights.  Cities were nearly"
  "untakeable, so the action centered around sieges and field battles."
  ""
  "The numbers border on the plausible, but again this one has not been"
  "played enough to find the imbalances even, let alone decide on good"
  "strategies."
)))

(add peltast notes
  "Useful in skirmishes."
  )
(add hoplite notes
  "Hoplite (heavy infantry).  For the capture of cities by assault."
  )
(add archer notes
  "???"
  )
(add cavalry notes
  "Fast, but ineffective against cities."
  )
(add trireme notes
  "Use this for everything naval."
  )

;;; Should add variable loyalties.

(game-module (design-notes (
  "Time scale is one month."
  )))
