(game-module "fred"
  ;; This is not a standalone game, has only a test setup defined.
  (title "Frederick")
  (blurb "Type definitions for games set during the Seven Years War")
  (variants (world-size (45 30 1000)))
)

(set see-all true) ; for debugging

(unit-type army (image-name "soldiers"))
(unit-type general (image-name "flag"))
(unit-type marshal (image-name "flag"))
(unit-type prince (image-name "flag"))
(unit-type king (image-name "crown"))
(unit-type depot (image-name "walltown"))
(unit-type town (image-name "town20"))
(unit-type fortress (image-name "fortress"))

(material-type supplies (help "generic supplies"))

(terrain-type sea (color "sky blue") (char "."))
(terrain-type countryside (color "green") (char "+") (image-name "countryside"))
(terrain-type swamp (color "yellow green") (char "="))
(terrain-type forest (color "forest green") (char "%")) ; no longer used?
(terrain-type hills (color "khaki") (char ">")) ; no longer used
(terrain-type mountains (color "sienna") (char "^"))
(terrain-type river (color "blue") (subtype border))
(terrain-type barrier (color "sienna") (image-name "mountains") (subtype border))
(terrain-type pass (color "gray") (image-name "road") (subtype connection))

(define land (countryside swamp forest hills mountains))
(define cell-t* (sea countryside swamp forest hills mountains))
(define leader (general marshal prince king))
(define bases (depot town fortress))

;;; Static relationships.

(table vanishes-on
  ;; No navy in this game.
  (u* sea true)
  )

;;; Armies carry leaders around and are propelled by them.

(add army capacity 1)
(add bases capacity 10)

(table unit-size-as-occupant
  (u* u* 100)  ; disables capacity by default
  (army bases 1)
  (leader army 1)  ; army can only have one commander
  (leader bases 0)
  )

;;; Leaders can carry each other around, higher ranks "carrying" lower.

(table unit-capacity-x
  (king (general marshal prince) 4)
  (prince (general marshal) 3)
  (marshal (general) 2)
  )

(table unit-size-in-terrain
  (u* t* 0)
  ;; (need to limit armies?)
  )

(table unit-storage-x
  (army supplies 100)
  (bases supplies 1000)
  )

;;; Actions.

(add army acp-per-turn 1)
(add leader acp-per-turn 6)
(add bases acp-per-turn 0)

(table acp-occupant-effect
  ;; Leaders "get the lead out", accelerate the army.
  (leader army 600)
  )

;;; Movement.

(table mp-to-enter-terrain
  (u* sea 99)
  ;; Crossing rivers is hard for armies.
  (army river 1)
  (army barrier 99)
  (leader barrier 99)
)

(table mp-to-traverse
  (army pass 1 #|2|#)
  )

(add army free-mp 1)

;; 5 s supply-line

;;; Construction.

(add depot cp 6)

(table acp-to-create (army depot 1))

(table acp-to-build (army depot 1))

;;; Combat.

(add army hp-max 80)

;(table acp-to-attack
;  )

(table hit-chance
  (army army 50)
  (army leader 50)
  )

(table damage
  (army army 1d4)
  (army leader 1)
  )

;;; Leaders should be able to retreat from hits.

(table acp-to-capture
  (army army 1)
  (army leader 1)
  (army bases 1)
  )

(table capture-chance
  ;; implausible for small units capturing large
  (army army 25)
  (army leader 100)
  (army bases 100)
  )

;;; Garrisons prevent immediate capture.

(table protection
  (army fortress 10)
  )

;;; Need a surrender chance for depots vs fortresses.

;;; Backdrop.

;;; Scoring.

(add u* point-value 0)
(add fortress point-value 1)

;;; Seasons.

(world (year-length 24))

(set calendar '(usual "month" 1 2))

(set season-names
  ((0 6 "winter") (7 11 "spring") (12 17 "summer") (18 23 "autumn")))

;;; Random generation; should be for testing only.

(add cell-t* alt-percentile-min (  0  70  30  20  90  95))
(add cell-t* alt-percentile-max ( 20  90  31  90  95 100))
(add cell-t* wet-percentile-min (  0  50  50   0   0   0))
(add cell-t* wet-percentile-max (100 100 100 100 100 100))

(set edge-terrain mountains)

(add u* start-with 1)  ; one of everything

(table favored-terrain
  (u* t* 0)
  (u* countryside 100)
  )

;;; Always max out at a 9-month campaign season.

(set last-turn 18)

(game-module (notes (
  "This is a game about the Seven Years War in central Europe.  The number of unit types"
  "is small, and there are only a few special characteristics.  The game"
  "is basically one of maneuvering for position, since combat was mostly"
  "ineffective."
  ""
  "Map scale is 15 miles/cell, time is 2 weeks/turn."
  ""
  )))

(game-module (design-notes (
  "A number of items remain to be developed:"
  ""
  "Disable free moves and make combat cost high, to simulate the mutual"
  "consent to combat."
  ""
  "Supply paths (about 4-5 cells)."
  ""
  "River effect on combat."
  )))
