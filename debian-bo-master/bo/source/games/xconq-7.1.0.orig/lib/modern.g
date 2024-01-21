(game-module "modern"
  (title "Modern Times")
  (blurb "Economics and politics of today's world")
  (variants (see-all true))
  )

;; This could be a variant, but is implausible to have an unseen world.
(set terrain-seen true)

(unit-type engrs (image-name "engineers"))

(unit-type fishing-boat (image-name "ap"))
(unit-type cargo-ship (image-name "ap"))

(unit-type farm (image-name "farm"))
(unit-type oil-field (image-name "oil-derrick"))
(unit-type oil-platform (image-name "oil-derrick"))

(unit-type shipyard (image-name "facility"))

(unit-type base (image-name "airbase"))
(unit-type port (image-name "port"))
(unit-type town (image-name "town20"))
(unit-type city (image-name "city20"))

(define ship-types (fishing-boat cargo-ship))

(define cities (town city))

(define places (farm oil-field oil-platform shipyard base port town city))

(material-type food)
(material-type oil)
(material-type iron)
(material-type people)

(include "stdterr")

(define water (sea shallows river))

;;; Static relationships.

(table unit-capacity-x
  (cities engrs 16)
  (city oil-field 1)
  )

(table unit-storage-x
  (farm food 100)
  (cities food 900)
  (u* oil 10)
  (cities oil 400)
  )

(table vanishes-on
  (places water true)
  (oil-platform shallows false)
  )

(table unit-size-in-terrain
  (u* t* 1)
  (farm t* 4)
  (oil-field t* 4)
  (town t* 4)
  (city t* 16)
  )

(add t* capacity 16)

(table terrain-storage-x
  (t* oil 100)
  )

;;; Actions.

(add engrs acp-per-turn 1)

(add cities acp-per-turn 1)

;;; Movement.

(add places speed 0)

;;; Construction.

(add engrs cp 2)

(add farm cp 48)

(add oil-field cp 48)

(add oil-platform cp 144)

(table acp-to-create
  (engrs (farm oil-field oil-platform shipyard) 1)
  (shipyard ship-types 1)
  (cities engrs 1)
  (city oil-field 1)
  )

(table cp-on-creation
  (engrs (farm oil-field oil-platform shipyard) 3)
  (shipyard ship-types 1)
  (cities engrs 1)
  (city oil-field 1)
  )

(table create-range
  (engrs oil-platform 1)
  (city oil-field 2)
  )

(table acp-to-build
  (engrs (farm oil-field oil-platform shipyard) 1)
  (shipyard ship-types 1)
  (cities engrs 1)
  (city oil-field 1)
  )

(table cp-per-build
  (engrs (farm oil-field oil-platform shipyard) 3)
  (shipyard ship-types 1)
  (cities engrs 1)
  (city oil-field 1)
  )

(table build-range
  (city oil-field 2)
  )

;;; Automatic production.

(table base-production
  ((oil-field oil-platform city) oil (20 20 10))
  )

(table base-consumption
  (u* food 1)
  ((town city) food (2 10))
  (u* oil 1)
  ((town city) oil (2 10))
  )

(table hp-per-starve
  (u* food 1.00)
  (u* oil 1.00)
  )

(table out-length
  ((oil-field oil-platform) oil 5)
  )

(table in-length
  (farm oil 5)
  (cities oil 5)
  )

;;; Terrain alteration.

(table acp-to-add-terrain
  (engrs road 1)
  )

(table acp-to-remove-terrain
  (engrs road 1)
  )

;;; Initial random setup.

(add city start-with 1)
(add town start-with 5)
(add farm start-with 10)
(set country-radius-min 3)
(set country-separation-min 16)
(set country-separation-max 48)
; Try to get countries on the coast.
(add (sea plains) country-terrain-min (1 4))

(table favored-terrain
  (u* t* 0)
  (farm plains 100)
  (cities plains 100)
  (cities land-t* 20)
  (cities plains 40)
  )

(table independent-density (cities plains 100))

(add land-t* country-people-chance 90)
(add plains country-people-chance 100)

(add land-t* independent-people-chance 50)

(table unit-initial-supply
  (u* m* 9999)
  (cities oil 400)
  )

(table terrain-initial-supply
  (t* oil 10d100)
  )

(game-module (design-notes (
  "A detailed game of modern problems."
  "Scale is 1 week/turn, 50km/cell."
  ""
  "Although this is a lot like the net empire games, it does not
   need to emulate them."
  "(should have a many-nukes-available option)"
  )))
