(game-module "intro"
  (title "Introductory Game")
  (blurb "An introduction to Xconq; your goal is to explore and take over the world.")
  ;; no variants, this is just a basic game.
)

(unit-type infantry (image-name "soldiers"))
(unit-type armor (image-name "tank"))
(unit-type transport (image-name "ap"))
(unit-type battleship (image-name "bb"))
(unit-type base (image-name "airbase") (char "/"))
(unit-type city (image-name "city20") (char "@"))

(terrain-type sea (color "sky blue") (char "."))
(terrain-type land (image-name "plains") (color "green") (char "+"))
(terrain-type mountains (color "sienna") (char "^"))

;;; Static relationships.

(table vanishes-on
  ((infantry armor base city) sea true)
  ((transport battleship) (land mountains) true)
)

(add (transport base city) capacity (6 6 24))

(table unit-size-as-occupant
  (u* u* 99)
  ((infantry armor) transport 1)
  ((infantry armor) (base city) 1)
  ((transport battleship) city (3 6))
  )

;;; Actions.

(add u* acp-per-turn (1 2 2 4 0 1))

;;; Movement.

(add (base city) speed 0)

(table mp-to-enter-terrain
  ((infantry armor) sea 99)  ; cheap accidental drowning prevention
  ((transport battleship) (land mountains) 99)
  )

(table mp-to-leave-terrain
  (armor mountains 1)
  )

;;; Construction.

(add u* cp (4 6 12 30 6 1))

(table acp-to-create
  (city (infantry armor transport battleship) 1)
  (infantry base 1)
  )

(table cp-on-creation
  (city (infantry armor transport battleship) 1)
  (infantry base 1)
  )

(table acp-to-build
  (city (infantry armor transport battleship) 1)
  (infantry base 1)
  )

(table cp-per-build
  (city (infantry armor transport battleship) 1)
  (infantry base 1)
  )

;;; Combat.

(add u* hp-max (2 2 1 4 10 40))

(table hit-chance
  (infantry u* (50 30 20 10 30 50))
  (armor u* (70 70 40 20 50 100))
  (battleship u* 100)
  )

(table damage
  (u* u* 1)
  (armor armor 1)
  (armor city 1)
  (battleship armor 1)
  )

(table capture-chance
  (infantry (base city) (50 50))
  (armor (base city) (90 70))
  ;; As a special case, infantry can sometimes capture armor.
  (infantry armor 10)
  )

;;; Initialization parameters.

(add t* alt-percentile-min (  0  60  90))
(add t* alt-percentile-max ( 60  90 100))
(add t* wet-percentile-min (  0   0   0))
(add t* wet-percentile-max (100 100 100))

(add (sea land) country-terrain-min (1 7))
(add city start-with 1)
(add city independent-near-start 3)
(set country-radius-min 3)
;; Countries are close together so beginners can find the enemy easily.
(set country-separation-min 6)
(set country-separation-max 8)

(table independent-density
  (city (land mountains) (200 20))
  )

(table favored-terrain
  (u* t* 0)
  ((infantry armor) land 100)
  (city (land mountains) (100 10))
  )

(include "ng-weird")
(add city namer "short-generic-names")

(world 120) ; cylinder maps are screwed up, avoid them for now

;; Fixed area, easier to play on.

(area 40 20)

;; Wire to exactly two players, no options.

(side 1 (noun "human"))

(side 2 (noun "robot"))

(set sides-min 2)
(set sides-max 2)

;; This is just a simple "take over the world", easier to explain.

(scorekeeper (do last-side-wins))

;; Only cities count.

(add u* point-value 0)
(add u* point-value 1)

(game-module (instructions (
  "This game is an introduction to Xconq."
  ""
  "You start with one city and try to expand throughout the world."
  "Eventually you will encounter the evil robots, and must do battle"
  "with them."
  ""
  "To get things going, use your first infantry to explore around."
  ""
  "You should try to capture independent cities and set them to"
  "building also, either infantry or armor.  Armor moves faster,"
  "but also takes longer to build."
  ""
  "When you find the bad guys, try to capture their cities.  There may"
  "be a lot of these, so search carefully.  They may also have armies secretly"
  "in reserve - watch out for surprise invasions!"
)))

