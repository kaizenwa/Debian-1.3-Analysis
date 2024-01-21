(game-module "beirut"
  (title "Beirut 1982")
  (blurb "The heroic fighters of Beirut")
  (variants
   (see-all true)
   )
  )

; buildings should be able to be indep after revolt
; death squads should be as fast as leaders and shouldn't retreat
; and should be able to take over buildings.
; no capturing if leader if they are win/lose condition.

(unit-type militia (char "m") (image-name "soldiers")
  (help "hide and fight from building to building"))
(unit-type |death squad| (char "d") (image-name "45")
  (help "hit people and not buildings"))
(unit-type leader (char "L") (image-name "man")
  (help "an individual, one for each side"))
(unit-type |car bomb| (char "C") (image-name "auto")
  (help "destroys buildings and all else in the vicinity"))
(unit-type tank (char "T") (image-name "tank")
  (help "also destroys buildings and other things"))
(unit-type building (char "B") (image-name "city20")
  (help "good for hiding out"))

; hostages material type?

(terrain-type sea (char ".") (color "sky blue"))
(terrain-type beach (char ",") (color "yellow"))
(terrain-type street (char "+") (color "light gray"))
(terrain-type junkheap (char "^") (color "sienna"))
(terrain-type fields (char "=") (color "green"))
(terrain-type trees (char "%") (color "forest green"))

(define m militia)
(define d |death squad|)
(define l leader)
(define c |car bomb|)
(define t tank)
(define B building)

(define movers ( m d l c t ))
(define vehicles ( c t ))
(define water ( sea ))
(define land ( beach street junkheap fields trees ))

;;; Static relationships.

(add B capacity 2)

(table unit-size-as-occupant
  (u* u* 99)
  ((m d l) B 1)
  )

;;; Vision.

;FIXME visibility is U T -> N, U's % visibility in T
(table visibility add (d t* 0))

(add B see-always 1)

(set terrain-seen true)

;;; Actions.

(add movers acp-per-turn ( 1 1 2 3 3 ))

;;; Movement.

(add B speed 0)

(table mp-to-enter-terrain
  (u* t* 99)
  (( m d l ) land 1)
  (vehicles ( beach street ) 1)
  )

;;; Combat.

(add u* hp-max ( 1 1 1 1 2 10 ))

(table hit-chance
  (m u* ( 50 50 50 50 50 50 ))
  (d u* (  5 50 70 20 20  0 ))
  (l u* (  0 20 50  0 20  0 ))
;  (c u* ( 90 90 90 90 90 90 ))
  (t u* ( 90 90 90 90 90 90 ))
  (B u* ( 10 10 10 10  0  0 ))
  )

(table damage
  (u* u* 1)
;  (c ( t B ) ( 2 10 ))
  )

(table capture-chance
  (m B 100)
  (m c 100)
  (m l 50)
  (m t 50)
  )

(table withdraw-chance-per-attack
  (u* m 20)
  (u* d 80)
  (u* l 95)
  )

(table protection
  (B ( m d ) 50)
  (m B 10)
  )

(add c acp-to-detonate 1)

(add c hp-per-detonation 1)

(table detonation-unit-range (c u* 2))

(table detonation-damage-at (c u* 10))

(table detonation-damage-adjacent (c u* 8))

(table detonate-on-hit (c u* (50 50 50 100 100 100)))

(table detonate-on-capture (c u* 30))

(table detonation-accident-chance (c t* 10))

;;; Random events.

;(set random-events (units-revolt))

(add B revolt-chance 100)

;;; Scoring.

(scorekeeper (do last-side-wins))

(add l point-value 10)
(add B point-value 1)

;;; Setup.

(add u* start-with ( 5 1 1 5 2 1 ))

(table independent-density (B junkheap 9000))

(table favored-terrain
  (u* t* 0)
  (movers street 100)
  (B junkheap 100)
  )

(set country-radius-min 6)
(set country-separation-min 4)
(set country-separation-max 10)

(add street country-terrain-min 15)
(add junkheap country-terrain-min 1)

;; Don't let this go on forever.

(set last-turn 200)

(side 1 (name "Maronite")
  )
(side 2 (name "Amal")
  )
(side 3 (name "Hezbollah")
  )
(side 4 (name "Druze")
  )
(side 5 (name "Syrian")
  )
(side 6 (name "Israeli")
  )
(side 7 (name "PLO")
  )

(set sides-min 7)
(set sides-max 7)

(world 100000)

(area 72 42)

(area (terrain
  (by-name
    (sea 0) (beach 1) (street 2) (junkheap 3) (fields 4)
    (trees 5))
  "72a"
  "72a"
  "72a"
  "15ab3d6b4a2bc2dc2dbd4bc2dc2dc4bc17a"
  "9abc5bc4d2c2dc2d4c2dc2dc2dc2dc2dc2dc2dc2dc16a"
  "8a2b3c3dc2d3cd6c3d6c3d16c15a"
  "7ab2dc2dc2d4cdc3dc2dc3dedc2dcd3cdc2dc2dc2dc2dcdc14a"
  "7ab2dc2d2cdc2dc2dc2dc2dc3dedc2dcdc3dc2dc2dc2dcdfc2dc13a"
  "7ab3c2dcd2c2d14ce10c2d11cd2c12a"
  "7ab2dc2dc2dc2dc2dcdc3dcf2c2df2dc2dc2dc2dc2dc2dc2dcd2cde11a"
  "7ab3d10cdc2dc2dc2dc2de2dc2dc2dc2dc2dc3dcdc2dcdfe10a"
  "8ab3c3dc3dcd5c2d7cf7cd8cdc2d4cf2e9a"
  "8ab3dc2dc3dc2dc2dc2dc2dc2dcfdcdc3dc2dcdc3d2c4dcdcd2e8a"
  "9a4dcd5c2dc2d4c2dc2dcfdc2dc2dc2dc2dc2dc2dc2dcfd4e7a"
  "9ab2cd4c2d7c2d8cd2c2dcd6cd11cefefc6a"
  "10abd2c2dc2dc2dcd2c2dc2dc2dc2dedcdcdc5dc5dc2dcfdcde2ce5a"
  "10abdcd3c2dc2dc2dc2dc2dc2dc2de2d2cdc2dc2dc2df2dc2dc2dcefcfde4a"
  "11abcdcd14c2d6cedcd11cd3cd4c2ecf3e3a"
  "11abd2cdc2dc2dcdc3dcd4c3dcdec2dc2dc2dc2dcdc2d2c2dcdecd4e2a"
  "12abdc2dcdc2dc2dc2dc5dc2dcdfc2dc2dc3dcdc2dc2dc2dc2dcf2ef2ea"
  "13ab16c4d6cf8c2d14c3efefe"
  "13adcdc2dc2dc2dcdc2dc2d2cdc3df2dc2dc2dc2dc2dc2dcdcdfdcdf2ef2e"
  "14adcdcdc2dcd2c2dc2dcdc2dc3dc2dc3dcdc2dcd2c2dc2dc2dc7e"
  "15abcd4c2dcd16ce3cdc2d3cdc2d8cd2fef2e"
  "16adcdc2dcdc3dc2dcdc3dc2dc2de2dc2dc2dc2dc2dcdc3dcdf5e"
  "17ab2c2dc2dc3dcdc2dc2dc2dc3dedc2d2cfc2dcd2c2dc2dcd3ef2e"
  "18ab13c2d4c2d10cd7cdcdcd4c5e"
  "19a3dc2dc5dc2dcdc3dc2dcdec2dc5dc2dc2dc2dc2dcfd2e"
  "20a2dc3d2c3dcd2c2dc2dc2dcdfc2dc2dc2dcd2c2dc2dc2d3cfe"
  "22ad2b2cd2cd3c3d8cf10cdcdcd7cfece"
  "25a2b3d2c2d2cdcdc3dcd2c2dc2dc2dcdc2d2c2dc2dcd2ec"
  "27a2b2dc2dcd2c2dc3dcdcedc2dc2dcdc3dc2dc2dc2def"
  "30a3cdc2d8cd2cf4c2dc2d2cd10ce"
  "31abd2c2dc2dc5dc2df2dc3dc3d2c2dc2dc2dce"
  "32abdc3dcdc5dc2dc2dc2d3c3dc2dc2dc3dc"
  "34a7c2d2cd4cf5c2d14c"
  "35a2dc2dc2dcd2cd2cdfc2dc2dc5dc2dc3dc"
  "36adc2dc2dcdcdcdc2de2dc2dc5dc2dc3dc"
  "37ab8c2d4ce14cd4c"
  "37ab2dc2dc2dc2dc2dcedc2dc2dc2dc2dc3dc"
  "38a2dc2dc2dc2dc2dc2dc2dc2dc2dc2dc3dc"
  "38a34c"
))

;;; Documentation.

(game-module (instructions (
  )))

(game-module (design-notes (
"Actually, this includes only a subset of the actual participants.
They all fight each other here; there should actually be some alliances."
  )))

(game-module (notes (
  "Relive the heroic struggles of the heroic factions fighting for"
  "the just and righteous cause of control of Beirut."
  ""
  "Try not to destroy too much of the city in the process."
  )))
