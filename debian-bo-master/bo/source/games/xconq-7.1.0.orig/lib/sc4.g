(game-module "sc4"
  ;; (should add a game size variant)
  ;; (should add land/water ratio variants)
  )

(unit-type tank (image-name "tank")
  (help ""))
(unit-type artillery (image-name "arty-1")
  (help ""))
(unit-type fighter (image-name "jets")
  (help ""))
(unit-type helicopter (image-name "helicopter")
  (help ""))
(unit-type transport (image-name "ap")
  (help ""))
(unit-type destroyer (image-name "dd")
  (help ""))
(unit-type submarine (image-name "sub")
  (help ""))
(unit-type carrier (image-name "cv")
  (help ""))
(unit-type battleship (image-name "bb")
  (help ""))
(unit-type bomber (image-name "4e")
  (help ""))
(unit-type city (image-name "city20")
  (help ""))

(material-type fuel)

(terrain-type water (color "sky blue") (char ".")
  )
(terrain-type land (color "green") (char "+")
  )

(define ground-types (tank artillery))
(define air-types (fighter helicopter bomber))
(define ship-types (transport destroyer submarine carrier battleship))

;;; Static relationships.

(table vanishes-on
  (ground-types water true)
  (ship-types land true)
  (city water true)
  )

;;; Unit-unit capacities.

(add transport capacity 6)
(add carrier capacity 100)
(add city capacity 100)

(table unit-size-as-occupant
  (u* u* 200)
  (ground-types transport 1)
  (air-types carrier 1)
  (u* city 1)
  ;; Cities can't occupy each other.
  (city city 200)
  )

;;; Unit-terrain capacity.

(add t* capacity 100)

(table unit-size-in-terrain
  (u* t* 1)
  ;; One ship per cell.
  (ship-types water 100)
  ;; Allow aircraft to pass over ships and towns.
  (air-types t* 0)
  ;; One city per cell.
  (city t* 100)
  )

;;; Unit-material capacities.

(table unit-storage-x
  (air-types fuel (20 20 30))
  ;; Carriers and cities have effectively infinite storage.
  ((city carrier) fuel 9999)
  )

;;; Vision.

(table see-chance-adjacent
  ;; Submarines are always hard to see for ships to see.
  (u* submarine 0)
  ;; ...except destroyers.
  (destroyer submarine 100) 
  )

;;; Actions.

(add ground-types acp-per-turn (2 1))
(add air-types acp-per-turn (20 10 10))
(add ship-types acp-per-turn 3)
(add destroyer acp-per-turn 4)

(add city acp-per-turn 1)

;;; Movement.

(add city speed 0)

(table mp-to-enter-terrain
  (ground-types water 99)
  (ship-types land 99)
  )

(table mp-to-enter-unit
  (u* u* 1)
  ; Aircraft can't sortie again until next turn.
  (air-types carrier (20 10 10))
  (air-types city (20 10 10))
  )

(add air-types free-mp (20 10 10))

(table consumption-per-move
  (air-types fuel 1)
  )

(table zoc-range
  ;; Units only ever control their own cells.
  (u* u* 0)
  ;; Cities don't control the air overhead.
  (city air-types -1)
  )

;;; Construction.

(add ground-types cp 4)
(add air-types cp (6 8 20))
(add ship-types cp (8 8 8 10 20))

(table acp-to-create
  (city u* 1)
  (city city 0)
  )

(table acp-to-build
  (city u* 1)
  (city city 0)
  )

;;; Combat.

(add u* hp-max (2 1 1 2 3 4 3 12 18 1 1))

(table hit-chance
  ;;                t   a   f   h   t   d   s   c   b   b   c
  (tank       u* ( 50  50  50  50  50  50  50  50  50  50  50))
  (artillery  u* ( 50  50  30  60  50  50  50  50  50  50  50))
  (fighter    u* ( 20  20  60  70  20  20  20  20  20  70  20))
  (helicopter u* ( 50  50  50  50  50  50  50  50  50  60  50))
  (transport  u* ( 10  10  10  10  10  10  10  10  10  10  10))
  (destroyer  u* ( 50  50  50  50  50  50  70  50  50  20  50))
  (submarine  u* ( 10  10   0   0  50  50  50  50  50   0  50))
  (carrier    u* ( 10  10  20  20  20  20  20  20  20  20  10))
  (battleship u* ( 50  50  50  50  50  50  50  50  50  50  50))
  (bomber     u* ( 50  50  50  50  50  50  50  50  50  50  50))
  (city       u* ( 50  50  50  50  50  50  50  50  50  50  50))
  )

(table damage
  (u* u* 1)
  ;; Armies don't damage cities.
  (ground-types city 0)
  ;; Submarines can be deadly.
  (submarine ship-types 2d4+1)
  )

(add artillery acp-to-fire 1)
(add battleship acp-to-fire 1)

(add artillery range 3)
(add battleship range 3)

(table capture-chance
  (ground-types city  50)
  )

(table hp-to-garrison
  ;; Use up the capturing unit.
  (ground-types city 1)
  )

;; (should add detonation for bombers)

;;; Backdrop.

(table base-consumption
  (air-types fuel (20 10 10))
  )

(table base-production
  ;; Our fuel sources can never run out.
  ((carrier city) fuel 9999)
  )

(table hp-per-starve
  (air-types fuel 1.00)
  )

(table auto-repair
  ;; Cities repair 1 hp of damage each turn for each ship occupant.
  (city ship-types 1.00)
  )

;;; Scoring.

(add u* point-value 0)
(add city point-value 1)

(scorekeeper (do last-side-wins))

;;; Random setup.

(add t* alt-percentile-min (0 70))
(add t* alt-percentile-max (70 100))
(add t* wet-percentile-min 0)
(add t* wet-percentile-max 100)

(set alt-smoothing 4)

(set country-radius-min 3)

(set country-separation-min 15)

(add t* country-terrain-min (30 6))

(add city start-with 1)
(add city independent-near-start 2)

(table favored-terrain
  (city (water land) (0 100))
  )

(table independent-density
  (city land 200)
  )

(table unit-initial-supply
  (city fuel 9999)
  )

(game-module (notes (
  "This is not a perfect emulation of StratCon."
)))
