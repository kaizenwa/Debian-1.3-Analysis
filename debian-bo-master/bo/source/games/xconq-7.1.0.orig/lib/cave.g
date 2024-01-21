(game-module "cave"
  (title "Cave")
  (blurb "Explore the Cave of Wandering Death")
)

(terrain-type floor (color "light gray") (image-name "floor") (char "."))
(terrain-type passage (color "gray") (image-name "flagstone") (char "."))
(terrain-type rock (color "dim gray") (image-name "rock") (char "#"))

(add rock thickness 10)

(unit-type human (image-name "person") (char "@")
  (hp-max 10) (acp-per-turn 4))

(unit-type orc
  (hp-max 10) (acp-per-turn 4))
(unit-type elf
  (hp-max 10) (acp-per-turn 4))
(unit-type dwarf
  (hp-max 10) (acp-per-turn 4))

(unit-type giant-ant (image-name "ant")
  (hp-max 15) (acp-per-turn 4))
(unit-type giant-beetle (image-name "beetle")
  (hp-max 20) (acp-per-turn 4))
(unit-type giant-spider (image-name "spider")
  (hp-max 25) (acp-per-turn 4))

(unit-type white-dragon (image-name "dragon")
  (hp-max 40) (acp-per-turn 4))
(unit-type red-dragon (image-name "red-dragon")
  (hp-max 50) (acp-per-turn 4))

(unit-type imp (image-name "person")
  (hp-max 5) (acp-per-turn 4))
(unit-type pit-demon
  (hp-max 50) (acp-per-turn 4))

(unit-type sapphire (name "huge sapphire") (image-name "gem") (char "*"))
(unit-type ruby (name "huge ruby") (image-name "gem") (char "*"))
(unit-type diamond (name "huge diamond") (image-name "gem") (char "*"))

(define humanoids (orc elf dwarf))

(define insects (giant-ant giant-beetle giant-spider))

(define dragons (white-dragon red-dragon))

(define reptiles (white-dragon red-dragon))

(define demons (imp pit-demon))

(define monsters (append humanoids insects reptiles demons))

(define animate (append human monsters))

(define items (sapphire ruby diamond))

(material-type food)
(material-type water)
(material-type gold)

;;; Static relationships.

(include "ng-weird")

(add human namer "generic-names")

(add human possible-sides "human")

(add monsters possible-sides "monster")

(table unit-storage-x
  (human m* (200 50 0))
  )

(add human capacity 100)
(add humanoids capacity 100)
(add dragons capacity 200)

(table unit-size-as-occupant
  (u* u* 999)
  (items u* 1)
  )

(add t* capacity (16 4 0))

(table unit-size-in-terrain
  (animate t* 4)
  (items t* 1)
  )

;;; Vision.

;; (should make longer only around light sources or some such)

(add u* vision-range 5)
;; Humans are not cave dwellers by nature, don't see as well.
(add human vision-range 4)

(add u* vision-bend 0)

(table eye-height
  (u* t* 5)
  )

;;; Action parameters.

(add items acp-per-turn 0)

;;; Movement parameters.

(table mp-to-enter-terrain
  (u* rock 99)
  )

;;; Combat parameters.

(table hit-chance
  (u* u* 50)
  )

(table damage
  (u* u* 1d6)
  )

(table acp-to-capture
  (u* items 1)
  )

(table independent-capture-chance
  (u* items 100)
  )

;; Dwarves can dig.

(table acp-to-add-terrain
  (dwarf (floor passage) 1)
  )

(table acp-to-remove-terrain
  (dwarf rock 1)
  )

;;; Backdrop activities.

(add u* hp-recovery 100)

;;; Random setup.

(add floor maze-room-occurrence 1)

(add passage maze-passage-occurrence 1)

(add t* occurrence 0)

(add rock occurrence 1)

(set edge-terrain rock)

;;; One adventurer on a side.

(add human start-with 1)

(add monsters start-with 1)
;(add insects start-with 2)
;(add humanoids start-with 3)

(set country-radius-min 4)
(set country-separation-min 15)
(set country-separation-max 20)

(table independent-density
  (items floor 1000)
  )

(table favored-terrain
  (u* floor 100)
  (u* rock 0)
  )

(set synthesis-methods
  '(make-maze-terrain make-countries make-independent-units))

(set sides-min 2)

(side 1 (name "You") (class "human") (emblem-name "none")
  (self-unit 1))

;(unit 0 human (s 1))

(side 2 (noun "Monster") (class "monster") (emblem-name "none"))

(scorekeeper (do last-side-wins))

(game-module (notes (
  "Cave exploration, with monsters."
  )))

(game-module (design-notes (
  "This is a pretty basic game.  It could be much elaborated."
  )))
