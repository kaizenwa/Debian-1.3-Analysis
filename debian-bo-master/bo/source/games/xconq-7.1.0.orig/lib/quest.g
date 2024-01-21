(game-module "quest"
  (title "Quest for XP")
  (blurb "Wilderness exploration in a fantasical world")
  (variants (world-seen false) (see-all false))
  )

(unit-type human (image-name "person") (char "@")
  (hp-max 10)
  )
;; should add other levels of human

(unit-type goblin
  (hp-max 4)
  )
(unit-type skeleton
  (hp-max 5)
  )
(unit-type orc
  (hp-max 10)
  )
(unit-type elf
  (hp-max 15)
  )
(unit-type dwarf
  (hp-max 9)
  )
(unit-type bugbear
  (hp-max 10)
  )
(unit-type troll
  (hp-max 20)
  )
(unit-type centaur
  (hp-max 20)
  )
(unit-type yeti
  (hp-max 20)
  )
(unit-type dragon
  (hp-max 40)
  )

(unit-type ant
  (hp-max 5)
  )
(unit-type beetle
  (hp-max 20)
  )
(unit-type spider
  (hp-max 20)
  )
(unit-type scorpion
  (hp-max 20)
  )

(unit-type vampire
  (hp-max 40)
  )

(unit-type sorceror
  (hp-max 40)
  )

(unit-type purple-worm
  (hp-max 40)
  )

(unit-type beholder
  (hp-max 40)
  )

(define humanoid (goblin orc elf dwarf bugbear yeti sorceror))

(define reptile (dragon))

(define bug (ant beetle spider scorpion))

(define undead (skeleton vampire))

(define other (troll centaur purple-worm beholder))

(define monster (append humanoid reptile bug undead other))

(define animate (append human monster))

(unit-type diamond-ring (image-name "ring")
  )
(unit-type treasure-chest
  )

(define objects (diamond-ring treasure-chest))

(unit-type lair
  (hp-max 20)
  )
(unit-type village
  (hp-max 20)
  )
(unit-type town (image-name "town20") (char "*")
  (hp-max 50)
  )
(unit-type castle (char "K")
  (hp-max 50)
  )
(unit-type city (image-name "city18") (char "!")
  (hp-max 100)
  )

(define places (lair village town castle city))

(material-type food
  )
(material-type water
  )
(material-type gold
  )

(include "stdterr")

(define water-t* (sea shallows))
(define land (swamp plains forest desert mountains))

(include "ng-weird")

(add human namer "generic-names")

(add places namer "generic-names")
(add lair namer "")

(add human possible-sides "human")

(add monster possible-sides "monster")

;;; Static relationships.

(table vanishes-on
  (animate water-t* true)
  (dragon water-t* false)
  (yeti t* true)
  (yeti (mountains ice) false)
  (places water-t* true)
  (places ice true)
  )

;;; Unit-unit capacities.

(add places capacity (20 20 40 40 80))

(table unit-size-as-occupant
  (u* u* 100)
  (animate places 1)
  )

;;; Unit-terrain capacities.

(add t* capacity 16)

(table unit-size-in-terrain
  (u* t* 1)
  (village t* 9)
  (town t* 12)
  (castle t* 9)
  (city t* 16)
  )

(table unit-storage-x
  (human m* (10 5 100))
  (dragon gold 1000)
  )

;;; Vision.

(add places already-seen 100)
(add lair already-seen 0)
(add village already-seen 50)
(add castle already-seen 50)

;;; Actions.

(add u* acp-per-turn 4)
(add places acp-per-turn 0)

;;; Movement.

(add places speed 0)

(table mp-to-enter-terrain
  ;; Accident prevention.
  (animate water-t* 99)
  (animate mountains 2)
  (animate ice 3)
  (animate river 0)
  (humanoid river 1)
  (animate road 0)
  (dwarf mountains 1)
  ;; Insects aren't intelligent enough to figure out how to cross water.
  (bug river 99)
  (yeti t* 99)
  (yeti (mountains ice) 0)
  ;; Dragons can go anywhere.
  (dragon t* 0)
  )

(table mp-to-leave-terrain
  ;; Forests are hard to get out of.
  (humanoid forest 1)
  ;; ...but not for elves.
  (elf forest 0)
  ;; Swamps are hard to get out of too.
  (humanoid swamp 1)
  (elf swamp 0)
  ;; Note that we make mountains harder to enter,
  ;; but exact no penalty for departure (walking
  ;; downhill is easy).
  )

(table mp-to-traverse
  (animate road 1)
  )

(table can-enter-independent
  ((human elf dwarf) places true)
  )

;;; Construction.

;;; (should have lairs make monsters)

;;; Combat.

(table acp-to-attack
  (animate places 0)
  (dragon places 2)
  )

(table hit-chance
  (u* u* 50)
  (animate places 0)
  (dragon places 100)
  (beholder u* 80)
  (purple-worm u* 80)
  (places u* 0)
  )

(table damage
  (u* u* 1)
  (animate places 0)
  (beholder u* 1d4)
  (dragon animate 1d5)
  (dragon places 1d4)
  (purple-worm animate 1d10)
  (places u* 0)
  )

;;; Backdrop.

(add u* hp-recovery 100)

(table base-production
  (animate food 1)
  (animate water 4)
  (places food 10)
  (places water 100)
  )

(table productivity
  (animate desert 0)
  (village land (0 100 50 20 20))
  (town land (0 100 50 20 20))
  (city land (0 100 50 20 20))
  )

(table base-consumption
  (animate food 1)
  (animate water 1)
  )

(table hp-per-starve
  (animate food 1.00)
  (dragon food 0.10)
  (animate water 1.00)
  (dragon water 0.10)
  )

;;; Random setup.

(add cell-t* alt-percentile-min (  0  40  44  45  45  45  90  97))
(add cell-t* alt-percentile-max ( 40  44  45  90  90  90  97 100))
(add cell-t* wet-percentile-min (  0   0  50   0  20  80   0   0))
(add cell-t* wet-percentile-max (100 100 100  20  80 100 100 100))

;;; One adventurer on a side.

(add human start-with 1)

;; A sampling of monsters.
(add monster start-with 1)
(add orc start-with 5)
(add lair start-with 3)

(set country-radius-min 8)
(set country-separation-min 15)
(set country-separation-max 20)

(table favored-terrain
  (u* t* 0)
  (u* (sea shallows) 0) 
  (u* plains 100)
  (elf forest 100)
  (dwarf plains 20)
  (dwarf mountains 100)
  (centaur forest 100)
  (scorpion desert 100)
  (yeti t* 0)
  (yeti mountains 50)
  (yeti ice 100)
  (dragon mountains 100)
  (lair plains 10)
  (lair (desert forest mountains) (20 100 100))
  )

(table independent-density
  (village (plains forest) (300 100))
  (town (plains forest) (100 50))
  (castle (plains forest mountains) (100 50 50))
  (city plains 10)
  )

(table road-chance
  (town (town city) ( 2   5))
  (castle (town city) 20)
  (city (town city) (80 100))
  )

(add (town city) road-to-edge-chance 100)

(set edge-road-density 100)

;; Nearly all towns and villages should be connected by road to
;; somewhere else.

(add village spur-chance 50)
(add village spur-range 2)

(add town spur-chance 90)
(add town spur-range 2)

(add castle spur-chance 90)
(add castle spur-range 2)

;(set synthesis-methods
;  '(make-maze-terrain make-countries make-independent-units))

(set sides-min 1)

(side 1 (name "You") (class "human"))

(side 2 (noun "Monster") (class "monster") (emblem-name "none"))

(table unit-initial-supply
  (u* food 10000)
  (u* water 10000)
  (u* gold 0)
  )

(scorekeeper (do last-side-wins))

(game-module (notes (
  "This is a sort of outdoors adventure for individual adventurers."
  )))

;;; Various pieces of equipment should be available.

;;; Have nobrains machine-run sides for countries, towns, castles, etc.
;;; (or quiescent unless player becomes outlaw?)

;;; Monsters belong to sides defined by alignment.  Sides of matching
;;; alignment friendly, etc.  No brains to side, but player could run
;;; a monster side, just for fun.
;;; Monsters can breed periodically, but limit total # somehow.

;;; Win by collecting the most treasure after <n> turns.
;;; Treasure should be hidden inside towns, carried by monsters.

