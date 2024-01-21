(game-module "ww2-div-pac"
  (title "WWII, Division-level, Pacific")
  (blurb "Definitions for divisional/theater-level WWII in the Pacific")
  )

(unit-type a-inf (image-name "inf-div") (possible-sides "allied")
  (help "Allied infantry division"))
(unit-type a-armor (image-name "armor-div") (possible-sides "allied"))

(unit-type j-inf (image-name "inf-div") (possible-sides "japanese"))
(unit-type j-armor (image-name "armor-div") (possible-sides "japanese"))

(unit-type para (image-name "para-div") (possible-sides "allied"))
(unit-type hq (image-name "flag"))

(define ground-types (a-inf a-armor j-inf j-armor para hq))

(unit-type aa (image-name "ap"))
(unit-type dd (image-name "dd"))
(unit-type ss (image-name "sub"))
(unit-type cl (image-name "ca"))
(unit-type ca (image-name "ca"))
(unit-type cve (image-name "cv"))
(unit-type cvl (image-name "cv"))
(unit-type cvs (image-name "cv"))
(unit-type cv (image-name "cv"))
(unit-type bc (image-name "bb"))
(unit-type bb (image-name "bb"))

(define small-ship-types (aa dd ss))

(define large-ship-types (cl ca cve))

(define capital-ship-types (cvl cvs cv bc bb))

(define carrier-types (cve cvl cvs cv))

(define ship-types (append small-ship-types large-ship-types capital-ship-types))

(unit-type airfield (image-name "airbase"))
(unit-type airbase)
(unit-type port)
(unit-type town (image-name "town20"))
(unit-type city (image-name "city20"))

(define place-types (airfield airbase port town city))

(material-type org ; organization
  (help "important to do things, acquired from HQs"))
(material-type air ; aircraft
  (help "attached aircraft used for support"))

(terrain-type sea)
(terrain-type atoll)
(terrain-type clear)
(terrain-type jungle (image-name "forest"))
(terrain-type hills)
(terrain-type mountains)
(terrain-type road (subtype connection))
(terrain-type river (subtype border))
(terrain-type reef (subtype border) (color "gray"))

(define water (sea atoll))
(define land (clear jungle mountains))

(define cell-t* (sea atoll clear jungle hills mountains))

;;; Static relationships.

(table vanishes-on
  (ground-types sea true)
  (ship-types land true)
  (place-types sea true)
  ((town city) atoll true)
  )

(add sea capacity 100)

(add land capacity 4)

(add aa capacity 1)

(table unit-size-as-occupant
  (u* aa 99)
  (ground-types aa 1)
  )

(table unit-storage-x
  (u* org 1)
  ;; Aircraft complements for various types of carriers.
  (carrier-types air (24 36 12 72))
  (place-types air (72 144 0 288 576))
  )

;;; Vision.

(set terrain-seen true)

(add u* already-seen 20)
(add place-types already-seen 100)

;; Range should depend on availability of scout aircraft...

(add carrier-types vision-range 4)

(add place-types vision-range 4)

;;; Action parameters.

(add ground-types acp-per-turn 1)
(add ship-types acp-per-turn 24)

;;; Movement.

(table mp-to-enter-terrain
  (ground-types sea 99)
  (ship-types land 99)
  (ship-types atoll 12)
  )

(table mp-to-leave-terrain
  (ship-types atoll 12)
  )

;;; Combat.

(add ground-types hp-max    12)
(add ground-types parts-max 12)

;aa dd ss cl ca cve cvl cvs cv bc bb

(add ship-types hp-max    (6 6 6 4 6 4 4 4 5 5 6))
(add ship-types parts-max (6 6 6 2 2 2 1 1 1 1 1))

(add place-types hp-max (10 20 20 40 80))

(table acp-to-attack
  (ship-types u* 6)
  )

(table acp-to-defend
  (u* ship-types 6)
  )

(add carrier-types acp-to-fire 12)
(add place-types acp-to-fire 12)

(table acp-to-be-fired-on
  (carrier-types carrier-types 6)
  )

(add carrier-types range 8)
(add place-types range 8)

(table hit-chance
  (ship-types ship-types 50)
  )

(table damage
  (ship-types ship-types 1)
  )

(table consumption-per-attack
  ;; Expect to lose one plane in an attack.
  (carrier-types air 1)
  (place-types air 1)
  )

(table hit-by
  ;; Planes can attack just about anything.
  (u* air 1)
  )

(table material-to-fight
  (carrier-types air 6)
  )

(table capture-chance
  (ground-types airfield 100)
  (ground-types (airbase port) 90)
  (ground-types (town city) 50)
  )

;;; Defaults for testing.

;;; Some defns for the fractal percentile generator.

(set alt-blob-density 10000)
(set alt-blob-size 20)
(set alt-smoothing 6)
(set wet-blob-density 2000)
(set wet-blob-size 100)

(add cell-t* alt-percentile-min (  0  90  90  90  97  98))
(add cell-t* alt-percentile-max ( 90  91  97  97  98 100))
(add cell-t* wet-percentile-min (  0   0   0  50   0   0))
(add cell-t* wet-percentile-max (100 100  50 100 100 100))

;;; River generation.

(add (clear jungle hills mountains) river-chance (10.00 25.00 25.00 25.00))

(set river-sink-terrain sea)

;(table terrain-interaction (river (sea shallows) non-terrain))

(set edge-terrain sea)

(add u* start-with 1)  ; one of everything

(set country-radius-min 4)
(set country-separation-min 20)
(set country-separation-max 60)

(add (sea clear) country-terrain-min (10 5))

(table favored-terrain
  (u* t* 0)
  (ground-types land 100)
  (ship-types sea 100)
  (place-types land 100)
  )

(table unit-initial-supply
  (u* m* 9999)
  (place-types air 100)
  )

(set side-library '(
  ((name "Allies") (adjective "Allied") (emblem-name "flag-usa")
   (class "allied") (names-locked true)
   (unit-namers
    (cl "us-cl-names")
    (ca "us-ca-names")
    (cve "us-cve-names")
    (cv "us-cv-names")
    (bb "us-bb-names"))
   )
  ((name "Japan") (adjective "Japanese") (emblem-name "flag-japan")
   (class "japanese") (names-locked true)
   (unit-namers
    (ca "japanese-ca-names")
    (cve "japanese-cve-names")
    (cv "japanese-cv-names")
    (bb "japanese-bb-names"))
   )
  ;; Russia and China useful as distinct sides?
  ))

(include "ng-ships")

(set calendar '(usual "day"))

(world 800 (year-length 365))

(game-module (design-notes (
  "The map-scale is 30 miles/hex, game time is 1 day/turn."
  "One unit of aircraft material is one plane."
  ""
  "To Do:"
  ""
  "Adapt old pacific map."
  ""
  "Could have one port and one airfield share a hex, but no more."
  ""
  "Units with aircraft should have a long vision range."
  ""
  "Add fuel as material, scenarios could predefine certain places to have lots."
  )))
