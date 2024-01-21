(game-module "ww2-adv"
  (title "WWII Advanced")
  (blurb "Types for global-scale simulation of WWII, at national level")
  )

;;; Types.

(unit-type infantry (image-name "infantry")
  (help "a primarily foot-powered army"))
(unit-type armor (image-name "armor")
  (help "a primarily mechanized army"))

(define ground-types (infantry armor))

(unit-type air-force (image-name "4e")
  (help "ground support capability"))
(unit-type bomber
  (help "strategic bombing capability"))
(unit-type interceptor (image-name "1e")
  (help "anti-bomber capability"))

(define air-types (air-force bomber interceptor))

(unit-type convoy (image-name "ap")
  (help "transportation capability"))
(unit-type fleet (image-name "bb")
  (help "naval combat capability"))
(unit-type cv-fleet (image-name "cv")
  (help "seaborne aircraft base"))
(unit-type sub-fleet (image-name "sub")
  (help "submarine capability"))
(unit-type asw-fleet (image-name "dd")
  (help "anti-submarine capability"))

(define ship-types (convoy fleet cv-fleet sub-fleet asw-fleet))

(unit-type base (image-name "airbase")
  (help ""))
(unit-type town (image-name "town20")
  (help ""))
(unit-type city (image-name "city20")
  (help "major cities only"))
(unit-type port-city (image-name "port")
  (help "major port cities only"))
(unit-type capital (image-name "city20")
  (help "capital of a country or region"))

(define place-types (base town city port-city capital))

(define builders         (town city port-city capital))

(define inf infantry)

(material-type oil
  (help "motive power for units"))
(material-type planes
  (help "combat power for air units"))

(terrain-type sea (color "sky blue") (image-name "sea") (char ".")
  (help "deep water"))
(terrain-type shallows (color "cyan") (image-name "shallows") (char ",")
  (help "shallow coastal water and lakes"))
(terrain-type swamp (color "yellow green") (image-name "swamp") (char "="))
(terrain-type desert (color "yellow") (image-name "desert") (char "~")
  (help "dry open terrain"))
(terrain-type land (color "green") (image-name "plains") (char "+")
  (help "open flat or rolling country"))
(terrain-type forest (color "forest green") (image-name "forest") (char "%"))
(terrain-type mountains (color "sienna") (image-name "mountains") (char "^"))
(terrain-type ice (color "white") (image-name "ice") (char "_"))
(terrain-type neutral (color "gray") #|(image-name "ice")|# (char "-"))
(terrain-type river (color "blue") (char "<")
  (subtype border) (subtype-x river-x))
(terrain-type cliffs (color "black") (char "|")
  (subtype border))
(terrain-type salt-marsh (color "blue") (char "=")
  (subtype border))
(terrain-type road (color "gray") (char ">")
  (subtype connection) (subtype-x road-x))

; need a coating for snow

;(table terrain-interaction (river (sea shallows) non-terrain))

(add (sea shallows) liquid true)

(define sea-t* (sea shallows))

(define land-t* (desert land forest mountains))

;;; Static relationships.

(table vanishes-on
  (ground-types sea-t* true)
  (ground-types ice true)
  (ship-types land-t* true)
  (ship-types ice true)
  (place-types sea-t* true)
  (place-types ice true)
  (u* neutral true)
  )

;;; Unit-unit capacities.

;(table unit-capacity-x
;  (cv fighter 4)
;  )

(table unit-size-as-occupant
  ;; Disable occupancy by default.
  (u* u* 99)
  (ground-types (convoy fleet) 1)
  (ground-types place-types 20)
  (air-types (convoy fleet cv-fleet) 1)
  (air-types place-types 1)
  (ship-types place-types 1)
  )

(add (convoy fleet cv-fleet) capacity (2 1 2))

(add place-types capacity 40)

;;; Unit-terrain capacities.

(table unit-size-in-terrain
  (u* t* 1)
  (ground-types t* 50)
  (place-types t* 100)
  )

(add t* capacity 100)

;;; Unit-material capacities.

(table unit-storage-x
  (u* oil 1)
  ;; Ships typically carry enough for six months at sea.
  (ship-types oil 6)
  (place-types oil (100 500 1000 1000 1000))
  (air-types planes (200 100 200))
  )

;;; Vision.

(set terrain-seen true)

;;; Reporters and civilians are always reporting on activities
;;; at fixed places.

(add place-types see-always true)

;;; Actions.

(add u* acp-per-turn 1)

(add ground-types acp-per-turn (4 5))

(add air-types acp-per-turn 4)

(add ship-types acp-per-turn 40)

(add base acp-per-turn 0)

;;; Movement.

(add place-types speed 0)

(table mp-to-enter-terrain
  (ground-types sea-t* 99)
  (ship-types land-t* 99)
  ((fleet cv-fleet) shallows 2)
  (u* ice 99)
  (air-types ice 1)
  (u* neutral 99)
)

;;; Marshes and cliffs should be basically uncrossable when landing.

(table material-to-move
  (u* oil 1)
  )

(table consumption-per-move
  (armor oil 1)
  )

;;; Construction.

(table acp-to-toolup
  (builders u* 1)
  )

(table tp-to-build
  (builders infantry 6)
  (builders armor 24)
  (builders air-types 24)
  (builders ship-types 36)
  )

(table tp-max
  (builders infantry 6)
  (builders armor 24)
  (builders air-types 24)
  (builders ship-types 36)
  )

(add ground-types cp (6 9))
(add air-types cp 12)
(add ship-types cp 6)

(table acp-to-create
  (builders ground-types 1)
  (builders air-types 1)
  (builders ship-types 1)
  )

(table cp-on-creation
  (builders ground-types 1)
  (builders air-types 1)
  (builders ship-types 1)
  )

(table acp-to-build
  (builders ground-types 1)
  (builders air-types 1)
  (builders ship-types 1)
  )

(table cp-per-build
  (builders ground-types 1)
  (builders air-types 1)
  (builders ship-types 1)
  )

(table supply-on-creation
  (u* oil 1)
  (air-types planes (20 10 20))
  )

;;; Repair.

(add ground-types hp-recovery (1.00 0.50))

(table auto-repair
  (place-types ground-types 1.00)
  )

(table auto-repair-range
  ;; Replacements can easily get to nearby units.
  (u* ground-types 1)
  )

;;; Production.

(table base-production
  (place-types oil 1)
  (base oil 0)
  ((city capital) planes 10)
  )

(table base-consumption
  ((infantry armor) oil (1 5))
  (ship-types oil 1)
  (air-types oil (3 8 3))
  )

(table hp-per-starve
  (ship-types oil 1.00)
  (air-types oil 1.00)
  )

(table consumption-as-occupant
  (air-types oil 1)
  )

;;; Combat.

(add ground-types hp-max (9 6))

(add air-types hp-max 3)

(add ship-types hp-max 9)

(add place-types hp-max (3 36 48 48 48))

(table acp-to-attack
  (u* u* 1)
  ;; Ships can always foil attacks by staying out to sea.
  (ground-types ship-types 0)
  ;; Convoys are not combat units.
  (convoy u* 0)
  ;; Subs are only useful against surface ships.
  (sub-fleet u* 0)
  (sub-fleet ship-types 1)
  (sub-fleet sub-fleet 0)
  ;; Places can't attack anything by themselves.
  (place-types u* 0)
  )

;; Air units attack by "firing" (ie raids).

(add air-types acp-to-fire 1)

(add air-types range (3 8 3))

(table withdraw-chance-per-attack
  (infantry ground-types (20 10))
  (armor    ground-types (30 20))
  )

(table acp-for-retreat
  (ground-types ground-types 1)
  )

(table hit-chance
  (u* u* 50)
  ;; Ships are hard to shoot at from shore, but a counterattack
  ;; might connect.
  (ground-types ship-types 10)
  (fleet convoy 95)
  ;; Convoys are not combat units, but do have a couple escorts.
  (convoy u* 5)
  ;; Subs are only useful against surface ships.
  (sub-fleet u* 0)
  (sub-fleet ship-types 50)
  (sub-fleet sub-fleet 0)
  ;; Subs can generally sink *something* in a convoy.
  (sub-fleet convoy 75)
  ;; ASW is effective once it finds the subs.
  (asw-fleet sub-fleet 80)
  (place-types infantry 20)
  (place-types armor 10)
  )

(table damage
  (u* u* 1)
  ;; A fleet of surface ships can do a lot of damage.
  (fleet u* 2d3+1)
  ;; Subs are only useful against surface ships.
  (sub-fleet u* 0)
  (sub-fleet ship-types 1)
  (sub-fleet sub-fleet 0)
  ;; Subs can devastate a convoy.
  (sub-fleet convoy 2d4+1)
  ;; ASW can take a chunk out of a sub fleet.
  (asw-fleet sub-fleet 2d4+3)
  )

(table capture-chance
  ;; Armor can be captured and made use of.
  (ground-types armor 20)
  ;; (Ground units are not going to catch planes in the air
  ;; or ships at sea, so can only capture them if in cities.)
  ;; Cities offer basically zippo resistance to armies.
  (ground-types place-types 90)
  ;; Battle fleets can capture cargo ships and such.
  ((fleet asw-fleet) convoy 90)
  )

(table protection
  ;; Ground units protect cities.
  (ground-types place-types 50)
  ((air-force interceptor) place-types 80)
  (place-types ground-types 50)
  )

(table material-to-fight
  (air-types planes (100 50 100))
  )

(table consumption-per-attack
  (air-types planes 10)
  )

(table hit-by
  ;; This reflect plane losses due to AA.
  (ground-types planes 1)
  (ship-types planes (1 3 4 0 2))
  (place-types planes (1 2 5 5 10))
  )

;;; Backdrop.

(table out-length
  ;; Net consumers of supply should never give any up automatically.
  (ground-types m* -1)
  ;; Cities and towns can share things around.
  (place-types m* 3)
  )

(table in-length
  ;; Supply to ground units can go several hexes away.
  (ground-types m* 3)
  ;; Cities and bases can get their supplies from some distance away.
  (place-types m* 10)
  (base m* 5)
  )

(table people-surrender-chance
  (ground-types t* 100)
  (place-types t* 100)
  )

;;; The world.

(world 360 (year-length 12))

;;; Sides.

(set side-library '(
  ((name "USA") (adjective "American") (emblem-name "flag-usa"))
  ((name "UK") (adjective "British") (emblem-name "flag-uk"))
  ((name "USSR") (adjective "Soviet") (emblem-name "soviet-star"))
  ((name "France") (adjective "French") (emblem-name "flag-france"))
  ((name "Germany") (adjective "German") (emblem-name "flag-swastika"))
  ((name "Italy") (adjective "Italian") (emblem-name "flag-italy"))
  ((name "Japan") (adjective "Japanese") (emblem-name "flag-japan"))
  ((name "China") (adjective "Chinese") (emblem-name "flag-china"))
  ((name "Spain") (noun "Spaniard") (adjective "Spanish") (emblem-name "flag-spain"))
  ((name "Turkey") (adjective "Turkish") (emblem-name "flag-turkey"))
  ))

(set calendar ("usual" "month"))

(set season-names
  ((0 2 "winter") (3 5 "spring") (6 8 "summer") (9 11 "autumn")))

;;; Random setup parameters (for testing).

(add t* alt-percentile-min   0)
(add t* alt-percentile-max   0)
(add (sea land) alt-percentile-min ( 0  70))
(add (sea land) alt-percentile-max (70 100))
(add t* wet-percentile-min   0)
(add t* wet-percentile-max 100)

;;; River generation.

(add (land forest mountains) river-chance (10.00 25.00 25.00))

(set river-sink-terrain shallows)

;;; Road generation.

(table road-into-chance
  (t* land 100)
  (land (desert forest mountains) (50 40 20))
  )

(set edge-terrain ice)

(set country-radius-min 4)

(add ground-types start-with (4 1))
(add place-types start-with 3)
(add port-city start-with 1)
(add capital start-with 1)

;(add (sea land) country-terrain-min (4 4))

(table favored-terrain
  (u* t* 0)
  (ground-types land 100)
  (air-types land 100)
  (ship-types sea 100)
  (place-types land 100)
  )

(add land country-people-chance 100)

(table unit-initial-supply
  (u* oil 9999)
  (air-types planes 9999)
  )

(game-module (design-notes
  "The basic idea of this game is to model the grand strategy of the WWII era."
  "Scale is 1 deg or 90-100km/hex, 1 month/turn."
  ""
  "At this scale the details of maneuver become less important than managing"
  "production and logistics."
  ""
  "Land units are infantry and armor armies, basically the same except that armor"
  "is more highly mechanized.  Units are about corps-sized."
  ""
  "Air forces represent strategic bombing, etc abilities."
  ""
  "Convoys are primarily transport ships with some protection."
  ""
  "Fleets are battleship/cruiser fleets, while carrier fleets have a long-range"
  "strike capability due to their aircraft."
  "Submarine fleets represent a large number of subs operating over a wide area."
  ""
  "City production is very hard to change."
  ))

