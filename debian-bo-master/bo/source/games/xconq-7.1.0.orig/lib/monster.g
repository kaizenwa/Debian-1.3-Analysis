(game-module "monster"
  (title "Monster")
  (blurb "Type defns for Tokyo 1962")
  )

;;; Reliving those old movies...

(set see-all true)

(unit-type monster (image-name "monster") (char "M")
  (point-value 100)
  (help "breathes fire and stomps on buildings"))
(unit-type mob (name "panic-stricken mob") (image-name "horde") (char "m")
  (help "helpless civilians"))
(unit-type |fire department| (image-name "fireman") (char "f")
  (help "puts out fires"))
(unit-type |national guard| (image-name "soldiers") (char "g")
  (help "does battle with the monster"))
(unit-type building (image-name "city20") (char "B")
  (point-value 1)
  (help "good for hiding, but crushed by monster"))
(unit-type burning-building (image-name "city20-burning") (char "B")
  (point-value 1)
  (help ""))
(unit-type wrecked-building (image-name "city20-wrecked") (char "W")
  (point-value 0)
  (help ""))
(unit-type rubble-pile (image-name "city20-rubble") (char "r")
  (point-value 0)
  (help ""))

(define firedept |fire department|)
(define guard |national guard|)
(define B building)
(define BB burning-building)
(define WB wrecked-building)
(define R rubble-pile)

(terrain-type sea (color "sky blue") (char "."))
(terrain-type beach (color "yellow") (image-name "desert") (char ","))
(terrain-type street (color "light gray") (image-name "road") (char "+"))
(terrain-type block (color "sienna") (char "-"))
(terrain-type fields (color "green") (image-name "plains") (char "="))
(terrain-type trees (color "forest green") (image-name "forest") (char "%"))

(define movers (monster mob firedept guard))
(define water (sea))
(define land (beach street block fields trees))

(add (monster) possible-sides "monster")
(add (mob firedept guard B) possible-sides "human")

(table vanishes-on
  (u* water true)
  ;; Godzilla can go in the water.
  (monster water false)
  ;; Fire trucks and mobs can only go along the streets.
  ((mob firedept) t* true)
  ((mob firedept) street false)
  )

(add movers acp-per-turn (2 1 2 2))

(table mp-to-enter-terrain
  (u* water 99)
  (monster water 0)
  ((mob firedept) t* 99)
  ((mob firedept) street 0)
  )

(table unit-capacity-x
  ((B BB) (mob firedept guard) 1)
  (WB guard 1)
)

(add u* hp-max (100 1 2 5 6 6 6 1))

(table hit-chance
  (monster  u* ( 50  50  50  50 100 100 100   0))
  (mob      u* (  0   0   0   0   0   0   0   0))
  (firedept u* (  0  90  90   0   0   0   0   0))
  (guard    u* ( 80   0   0   0   0   0   0   0))
  (B        u* ( 10   0   0   0   0   0   0   0))
)

(table damage
  (u* u* 1)
  (guard monster 4)
  (monster B 3)
  (monster BB 3)
  (monster WB 3)
  (u* rubble-pile 0)
)

(add monster acp-to-fire 1)

(add monster range 2)

(add building wrecked-type burning-building)

(add burning-building wrecked-type wrecked-building)

(add wrecked-building wrecked-type rubble-pile)

(add BB hp-per-detonation 4)

(table detonation-unit-range
  (BB u* 2)
  )

(table detonation-damage-at
  (BB u* 6)
  )

(table detonation-damage-adjacent
  (BB u* 6)
  )

(table detonation-accident-chance
  (BB t* 10.00)
  )

;(add u* destroy-message
;  ("kills" "extinguishes" "extinguishes" "massacres" "wipes out" "wipes out" "flattens"))

(add monster namer "monster-names")

(namer monster-names (random
  "Godzilla" "Rodan" "Mothra" "Megalon" "Gajira" "Aspidra"
  "Reptilicus" "Gamera"
  ))

;;; Random setup, for testing.

(add t* alt-percentile-min (  0  20  25  35  80  90 ))
(add t* alt-percentile-max ( 20  25  35  80  90 100 ))
(add t* wet-percentile-min 0)
(add t* wet-percentile-max 100)

(set country-radius-min 5)

(add u* start-with (1 10 3 3 10 0 0 0))

(table favored-terrain
  (u* sea 0)
  )

(game-module (notes (
  "Typically, one would set up a scenario with one or more monsters on"
  "one side, and mobs, fire departments, and national guards on the"
  "other.  Note"
  "that the monster can easily defeat national guards one after another,"
  "and that the most successful strategy for the human side is to"
  "attack the monster with several units at once.  The monster can use"
  "fires as a barricade to keep the national guards from getting close"
  "enough to attack.  Destroying buildings is fun but not very useful."
  ""
  "Sandra Loosemore (sandra@cs.utah.edu) is the person to blame for this"
  "piece of silliness (well, Stan aided and abetted)."
)))

