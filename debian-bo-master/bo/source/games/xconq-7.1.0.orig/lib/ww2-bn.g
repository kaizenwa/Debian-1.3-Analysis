(game-module "ww2-bn"
  (title "WWII, Battalion-level, Europe")
  (blurb "Definitions for battalion-level WWII, European theater")
  (variants (see-all true))
  )

;;; Non-motorized infantry.

(unit-type inf (long-name "infantry"))
(unit-type inf-bicycle (long-name "bicycle infantry"))
(unit-type inf-para (long-name "parachute infantry") (possible-sides "allied"))
(unit-type inf-glider (long-name "glider infantry") (possible-sides "allied"))
(unit-type inf-sec (long-name "security company") (possible-sides "allied"))
(unit-type inf-ranger (long-name "rangers") (possible-sides "allied"))
(unit-type inf-cmdo (long-name "commando") (possible-sides "allied"))
(unit-type inf-fusil (long-name "fusilier") (possible-sides "german"))
(unit-type inf-ost (long-name "Ost infantry") (possible-sides "german"))
(unit-type inf-pion (long-name "pioniere") (possible-sides "german"))

(define non-mot-inf-bn-types
  (inf inf-bicycle inf-para inf-glider inf-ranger inf-cmdo inf-fusil inf-ost inf-pion))

(define non-mot-inf-co-types (inf-sec))

(define non-mot-inf-types (append non-mot-inf-bn-types non-mot-inf-co-types))

(add non-mot-inf-bn-types image-name "inf-bn")
(add inf-bicycle image-name "inf-bike-bn")
(add non-mot-inf-co-types image-name "inf-co")

(add non-mot-inf-types acp-per-turn 3)
(add inf-sec acp-per-turn 2)
(add (inf-bicycle inf-ranger inf-cmdo inf-pion) acp-per-turn 4)

(define allied-inf-types (inf-para inf-glider inf-sec inf-ranger inf-cmdo))

;;; Motorized infantry.

(unit-type inf-mot (long-name "motorized infantry"))
(unit-type inf-mg-co (long-name "machinegun company"))
(unit-type inf-mot-sec (long-name "motorized security"))
(unit-type inf-recon (long-name "recon") (possible-sides "allied"))
(unit-type inf-fsj (long-name "Fallschirmjaeger") (possible-sides "german"))
(unit-type inf-mot-pion (long-name "Pioniere") (possible-sides "german"))

(define mot-inf-bn-types (inf-mot inf-fsj inf-recon))

(define mot-inf-co-types
  (inf-mg-co inf-mot-sec inf-recon inf-mot-pion))

(define mot-inf-types (append mot-inf-bn-types mot-inf-co-types))

(add mot-inf-bn-types image-name "inf-mot-bn")
(add mot-inf-co-types image-name "inf-mot-co")

(add mot-inf-types acp-per-turn 8)

;;; Tracked infantry.

(unit-type inf-armored (long-name "armored infantry"))
(unit-type inf-pz-pion (long-name "Panzer pioniere") (possible-sides "german"))
(unit-type inf-armored-recon (long-name "armored recon") (possible-sides "german"))
(unit-type inf-lw-recon (long-name "Luftwaffe recon") (possible-sides "german"))

(define tracked-inf-types (inf-armored inf-pz-pion inf-armored-recon inf-lw-recon))

(add tracked-inf-types image-name "inf-tracked-bn")

(add (inf-armored inf-pz-pion) acp-per-turn 5)
(add (inf-armored-recon inf-lw-recon) acp-per-turn 6)

(define inf-bn-types (append non-mot-inf-bn-types mot-inf-bn-types inf-armored inf-pz-pion inf-armored-recon))
(define inf-co-types (append non-mot-inf-co-types mot-inf-co-types inf-lw-recon))

;;; All infantry types together.

(define inf-types (append non-mot-inf-types mot-inf-types tracked-inf-types))

(add inf-types hp-max 12)
(add inf-co-types hp-max 3)

;;; Cavalry types.

(unit-type cav-mech (long-name "mechanized cavalry") (image-name "cav-sqdn"))
(unit-type cav-mech-trp (long-name "mechanized cavalry troop") (image-name "cav-trp"))

(define cav-bn-types (cav-mech))
(define cav-co-types (cav-mech-trp))

(define cav-types (cav-mech cav-mech-trp))

(add cav-types acp-per-turn 12)

;;; Artillery types.

(unit-type arty-cd-cased (long-name "casemated coastal artillery"))
(unit-type arty-cd-open (long-name "open coastal artillery"))
(unit-type arty-horse (long-name "horse-drawn artillery battalion"))
(unit-type arty-horse-bty (long-name "horse-drawn artillery battery"))
(unit-type arty-para (long-name "parachute artillery"))
(unit-type arty-glider (long-name "glider artillery"))
(unit-type arty-rr (long-name "railroad artillery"))

(define german-fixed-arty-types
  (arty-cd-cased arty-cd-open))

(add german-fixed-arty-types possible-sides "german")

(add german-fixed-arty-types acp-per-turn 2)

(add german-fixed-arty-types speed 0)

(add german-fixed-arty-types acp-to-fire 1)

(add german-fixed-arty-types range (12 10))

(define allied-non-mot-arty-types
  (arty-para arty-glider))

(add allied-non-mot-arty-types possible-sides "allied")

(add allied-non-mot-arty-types acp-per-turn 6)

(add allied-non-mot-arty-types acp-to-fire 3)

(add allied-non-mot-arty-types range (8 6))

(define horse-arty-types
  (arty-horse arty-horse-bty))
 
(add horse-arty-types acp-per-turn 3)

(add horse-arty-types acp-to-fire 1)

(add horse-arty-types range (8 12))

(add arty-rr acp-per-turn 6)

(add arty-rr acp-to-fire 3)

(add arty-rr range 20)

(add arty-rr range-min 5)

(define non-mot-arty-types
  (append german-fixed-arty-types allied-non-mot-arty-types
          horse-arty-types arty-rr))

(add non-mot-arty-types image-name "arty-bn")
(add arty-cd-cased image-name "arty-cd-cased")
(add arty-horse-bty image-name "arty-co")

(unit-type arty-group (long-name "artillery group") (possible-sides "allied"))
(unit-type arty-hq (long-name "HQ artillery") (possible-sides "allied"))
(unit-type arty-towed (long-name "towed artillery"))
(unit-type arty-flak (possible-sides "german"))
(unit-type arty-flak-bty (long-name "Flak battery") (possible-sides "german"))
(unit-type arty-at (long-name "anti-tank battalion"))
(unit-type arty-nebel (long-name "Nebelwerfer battalion") (possible-sides "german"))
(unit-type arty-mortar (long-name "mortar battalion"))
(unit-type arty-mortar4.2 (long-name "mortar battalion") (possible-sides "allied"))
(unit-type arty-fortress-bn (long-name "fortress battalion"))

(define mot-arty-types
  (arty-group arty-hq arty-towed arty-flak arty-flak-bty
   arty-at arty-nebel arty-mortar arty-mortar4.2 arty-fortress-bn))

(add mot-arty-types image-name "arty-mot-bn")
;(add mot-arty-types image-name "arty-mot-co")

(add (arty-mortar arty-mortar4.2) image-name "mortar")

(add (arty-flak arty-flak-bty) image-name "flak")

(add mot-arty-types acp-per-turn 8)

(add mot-arty-types acp-to-fire 4)

(add mot-arty-types range (8 9 8 1 1 1 5 4 2 2))

(unit-type arty-nebel-bty (long-name "Nebelwerfer battery") (possible-sides "german"))
(unit-type arty-armored (long-name "armored artillery"))
(unit-type arty-hq-armored (long-name "tracked HQ artillery") (possible-sides "allied"))
(unit-type arty-sp-at (long-name "self-propelled anti-tank battalion"))
(unit-type arty-sp-at-co (long-name "self-propelled anti-tank company"))

(define tracked-arty-bn-types (arty-armored arty-sp-at))

(define tracked-arty-co-types (arty-nebel-bty arty-hq-armored arty-sp-at-co))

(define tracked-arty-types (append tracked-arty-bn-types tracked-arty-co-types))

(add tracked-arty-bn-types image-name "arty-armor-bn")
(add tracked-arty-co-types image-name "arty-armor-co")

(add tracked-arty-types acp-per-turn 6)

(add tracked-arty-types acp-to-fire 2)

(add tracked-arty-types range (8 4 8 1 1))

(define arty-types (append non-mot-arty-types mot-arty-types tracked-arty-types))

(add arty-types hp-max 12)

;;; Armor types.

(unit-type tank (long-name "tank battalion"))
(unit-type assault-gun (long-name "assault gun battalion"))
(unit-type tank-co (long-name "tank company"))
(unit-type assault-gun-co (long-name "assault gun company"))
(unit-type croc-co (long-name "crocodile company"))

(define armor-bn-types (tank assault-gun))

(define armor-co-types (tank-co assault-gun-co croc-co))

(define armor-types (append armor-bn-types armor-co-types))

(add armor-bn-types image-name "armor-bn")
(add armor-co-types image-name "armor-co")

(add armor-types acp-per-turn 6)
(add croc-co acp-per-turn 4)

(add armor-types hp-max 12)

;; Headquarters types.

(unit-type hq-nebel-brg (long-name "nebelwerfer brigade HQ"))
(unit-type hq-flak-reg (long-name "sturmflak regiment HQ"))

(unit-type hq-inf-div (long-name "infantry division HQ"))
(unit-type hq-mot-div (long-name "motorized infantry division HQ"))
(unit-type hq-para-div (long-name "airborne division HQ"))
(unit-type hq-gren-div (long-name "panzergrenadier division HQ"))
(unit-type hq-armor-div (long-name "armor division HQ"))

(unit-type hq-flak-korps (long-name "sturmflak korps HQ"))
(unit-type hq-pz-korps (long-name "panzer korps HQ"))
(unit-type hq-fsj-korps (long-name "fallschirmjaeger korps HQ"))

(unit-type hq-inf-corps (long-name "infantry corps HQ"))

(define german-hq-types
  (hq-inf-div hq-nebel-brg hq-flak-reg hq-gren-div
   hq-flak-korps hq-pz-korps hq-fsj-korps))

(add german-hq-types acp-per-turn 6)

(add german-hq-types possible-sides "german")

(define allied-hq-types
  (hq-mot-div hq-para-div))

(add allied-hq-types possible-sides "allied")

(add allied-hq-types acp-per-turn (8 6))

(define generic-hq-types
  (hq-armor-div hq-inf-corps))

(add generic-hq-types acp-per-turn 8)

(define hq-types (append german-hq-types allied-hq-types generic-hq-types))

(add hq-types image-name "inf-div-hq")
(add (hq-flak-korps hq-pz-korps hq-fsj-korps) image-name "inf-corps-hq")
(add hq-inf-corps image-name "inf-corps-hq")

(add hq-types hp-max 2)

(define corps-hq-types (hq-inf-corps hq-flak-korps hq-pz-korps hq-fsj-korps))

(define ground-unit-types (append inf-types cav-types arty-types armor-types hq-types))

(define mot-types (append mot-inf-types cav-types mot-arty-types hq-types))

(define tracked-types (append tracked-inf-types tracked-arty-types armor-types))

(define vehicle-types (append mot-types tracked-types))

(define bn-types (append inf-bn-types cav-bn-types))
(define co-types (append inf-co-types cav-co-types armor-co-types))

;;; Trains.

(unit-type train (possible-sides "german"))

(add train acp-per-turn 80)

;;; Ships.

(unit-type landing-ship (image-name "ap"))
(unit-type transport (image-name "ap"))
(unit-type destroyer (image-name "dd"))
(unit-type cruiser (image-name "ca"))
(unit-type battleship (image-name "bb"))

(define ship-types (landing-ship transport destroyer cruiser battleship))

(add ship-types possible-sides "allied")

;(add ship-types acp-per-turn 4)
;(add ship-types speed 99.00)
(add ship-types acp-per-turn 80)

(add (destroyer cruiser battleship) acp-to-fire 20)

(add (destroyer cruiser battleship) range (6 8 12))

(add ship-types hp-max (6 6 6 12 18))

;;; Aircraft.

(unit-type fighter-bomber (image-name "1e"))
(unit-type light-bomber (image-name "4e"))
(unit-type medium-bomber (image-name "4e"))
(unit-type heavy-bomber (image-name "4e"))

(define aircraft (fighter-bomber light-bomber medium-bomber heavy-bomber))

(add aircraft possible-sides "allied")

(add aircraft acp-per-turn 4)
(add aircraft speed 99.00)

;;; Places.

(unit-type fort (image-name "fortress"))
(unit-type fortification (image-name "walltown"))
(unit-type coast-strongpoint (image-name "fortress"))

(define fort-types (fort fortification coast-strongpoint))

(add fort-types hp-max 8)

;;; Supply-related units.

(unit-type mulberry (possible-sides "allied"))
(unit-type port)
(unit-type supply-depot)

(define supply-types (mulberry port supply-depot))

(unit-type village (image-name "town20"))
(unit-type town (image-name "city18"))

(define town-types (village town))

(add town-types see-always true)

(add town-types hp-max (24 48))

(define place-types (append fort-types supply-types town-types))

;;; Materials

(material-type supply)

;;; Terrain and its properties.

(terrain-type sea (color "dodgerblue") (char ".")
  (occurrence 10))
(terrain-type clear (color "pale green") (char "+") (image-name "")
  (occurrence 60))
(terrain-type bocage (color "green") (char "#")
  (occurrence 20))
(terrain-type hill (color "sienna") (image-name "mountains") (char "^")
  (occurrence 1))
(terrain-type forest (color "forest green") (char "%")
  (occurrence 8))
(terrain-type swamp (color "yellow green")
  )
(terrain-type flooded (color "light blue")
  (help "ground that is temporarily swampy")
  )
(terrain-type beach (color "tan") (image-name "desert")
  )
(terrain-type city (color "gray") (image-name "urban") (char "*")
  (occurrence 1))
(terrain-type river (color "blue")
  (subtype border) (occurrence 10))
(terrain-type road (color "gray")
  (subtype connection) (subtype-x road-x) (occurrence 10))
(terrain-type sec-road (color "dark gray")
  (subtype connection) (subtype-x road-x) (occurrence 100))
(terrain-type railroad (color "black")
  (subtype connection) (subtype-x road-x) (occurrence 50))

(define land (clear bocage hill forest swamp flooded city beach))

(define roads (road sec-road))

;;; Distances are all in meters.

(add t* elevation-min 0)
(add t* elevation-max 1000)
(add (sea swamp) elevation-max 0) 

(area (cell-width 2000))

;;; Static relationships.

(table vanishes-on
  (ground-unit-types sea true)
  ((arty-rr train) t* true)
  ((arty-rr train) railroad false)
  (ship-types land true)
  ;; Landing ships can go on the beach.
  (landing-ship beach false)
  (place-types sea true)
  ;; ...except for mulberries.
  (mulberry sea false)
  )

;; A cell has a capacity for basically six companies or two battalions.

(add t* capacity 6)

(table unit-size-in-terrain
  (bn-types t* 3)
  (co-types t* 1)
  (arty-types t* 1)
  (arty-sp-at t* 3) ; only large arty type
  (armor-bn-types t* 2)
  (hq-types t* 1)
  ;; A port takes up no additional space.
  (port t* 0)
  ;; A town fills up the cell.
  (town t* 6)
  )

(add landing-ship capacity 9)
(add transport capacity 9)
(add fort-types capacity 12)
(add town-types capacity 12)

(table unit-size-as-occupant
  (u* u* 100)
  (bn-types (landing-ship transport) 3)
  (co-types (landing-ship transport) 1)
  (arty-types (landing-ship transport) 1)
  (arty-sp-at (landing-ship transport) 3) ; only large arty type
  (armor-bn-types (landing-ship transport) 2)
  (hq-types (landing-ship transport) 1)
  ;; Stacking in towns is similar to stacking in terrain.
  (bn-types fort-types 3)
  (co-types fort-types 1)
  (arty-types fort-types 1)
  (arty-sp-at fort-types 3) ; only large arty type
  (armor-bn-types fort-types 2)
  (hq-types fort-types 1)
  (bn-types town-types 3)
  (co-types town-types 1)
  (arty-types town-types 1)
  (arty-sp-at town-types 3) ; only large arty type
  (armor-bn-types town-types 2)
  (hq-types town-types 1)
  )

(table unit-capacity-x
  (port transport 6)
  )

(table unit-storage-x
  ;; Any unit can get along for two days by itself.
  (u* supply 18)
  (co-types supply 6)
  (hq-types supply 180)
  (supply-depot supply 360)
  )

;;; Actions.

;; (Acp is defined with each subset of types.)

;; Units need some material to be able to act.

;; Night shuts things down considerably.

(table acp-night-effect
  ;; 1/2 of normal abilities.
  (u* t* 50)
  )
  
;;; Movement.

(table mp-to-leave-terrain
  (u* t* 1)
  (u* swamp 2)
  (u* flooded 2)
  (vehicle-types t* 2)
  ;; Armor can go cross-country.
  (armor-types (clear bocage) 1)
  (vehicle-types city 1)
  ;; Standing water is a problem for vehicles.
  (vehicle-types swamp 3)
  (vehicle-types flooded 4)
  ;; Advantage of roads.
  (vehicle-types roads 0)
  ;; Prevent land units from crossing the sea.
  (u* sea 9999)
  (ship-types sea 0)
  (aircraft sea 0)
  )

(table mp-to-enter-terrain
  (u* t* 1)
  (u* swamp 2)
  (u* flooded 2)
  (vehicle-types t* 2)
  (vehicle-types city 1)
  ;; Standing water is a problem for vehicles.
  (vehicle-types swamp 3)
  (vehicle-types flooded 4)
  ;; Extra cost to cross rivers.
  (u* river 1)
  (vehicle-types river 3)
  (tracked-types river 2)
  ;; Advantage of roads.
  (vehicle-types roads 0)
  ;; Rail-only units.
  ((arty-rr train) t* 9999)
  ((arty-rr train) railroad 1)
  ;; Accident prevention.
  (ground-unit-types sea 9999)
  (ship-types land 9999)
  (landing-ship beach 1)
  )

(table mp-to-traverse
  (vehicle-types roads 1)
  )

;; No freebies for landing, we have to be in a port or on a beach.

(table ferry-on-entry
  ((landing-ship transport) u* over-nothing)
  )

(table ferry-on-departure
  ((landing-ship transport) u* over-nothing)
  )

(add inf-types free-mp 1)
(add non-mot-inf-types free-mp 2)

;;; Production.

(table base-production
  ;; Corps HQs "produce" enough supply to support all the units that would
  ;; be under their command, for about one day or so.
  (corps-hq-types supply 300)
  )

(table base-consumption
  ;; Units are consuming all the time.
  (u* supply 3)
  (co-types supply 1)
  ;; ... but not forts or towns.
  (fort-types supply 0)
  (town-types supply 0)
  )

(table hp-per-starve
  ;; Starving units ultimately die or surrender, so end up out of the
  ;; game permanently.
  (u* supply 6.00)
  (fort-types supply 0)
  (town-types supply 0)
  )

;;; Combat.

;;; (Hp and range is defined with each set of types.)

(table acp-to-attack
  (u* u* 1)
  (vehicle-types u* 4)
  ;; Artillery can't attack things directly (can still defend though).
  (arty-types inf-types 0)
  (arty-types armor-types 0)
  (arty-types fort-types 0)
  (arty-types town-types 0)
  ((landing-ship transport) u* 0)
  )

(table hit-chance
  (u* u* 50)
  (hq-types u* 0)
  (u* hq-types 25)
  (u* fort-types 5)
  (u* town-types 100)
  ((landing-ship transport) u* 0)
  )

(table damage
  (u* u* 1d5)
  (hq-types u* 0)
  (u* fort-types 1)
  (u* town-types 1)
  ((landing-ship transport) u* 0)
  )

(table retreat-chance
  (inf-types inf-types 10)
  (armor-types inf-types 20)
  (inf-types arty-types 20)
  (armor-types arty-types 40)
  (u* hq-types 50)
  )

;;; Capture.

(table capture-chance
  ;; This is the chance to capture a mostly-abandoned fort.
  (inf-types fort-types 50)
  (armor-types fort-types 50)
  ;; Towns never offer any sort of resistance.
  (u* town-types 100)
  )

(table protection
  (inf-types fort-types 50)
  (arty-types fort-types 40)
  (armor-types fort-types 40)
  (inf-types town-types 50)
  (arty-types town-types 60)
  (armor-types town-types 40)
  )

(table occupant-combat
  (ground-unit-types (landing-ship transport) 0)
  )

;; Infantry bounces back if given a chance to rest.

(add inf-types hp-recovery 0.25)

;; Damage to forts is hard to repair.

(add fort-types hp-recovery 0.05)

;; The Germans may want to destroy ports etc.

(add port acp-to-disband 1)

;;; Backdrop.

(table out-length
  ;; Most consumers of supply never give any up automatically.
  (u* supply -1)
  ;; Supply sources can share things around.
  (hq-types supply 8)
  (supply-depot supply 12)
  )

(table in-length
  (u* supply 12)
  )

;;; Random game synthesis (for testing mostly, will usually use fixed map and units).

(set synthesis-methods
  '(make-random-terrain make-countries make-independent-units make-roads))

#|
(set synthesis-methods (append synthesis-methods '(make-random-date)))

(set initial-date-min "0 1 Jan 1944")
(set initial-date-max "0 1 May 1945")
|#

;; need a French name generator for villages.
;(add village independent-density 1000)

(set country-radius-min 8)

(add sea country-terrain-min 4)

(add sea country-terrain-max 10)

;; One of everything.

(add u* start-with 1)

(table favored-terrain
  (u* t* 100)
  (u* sea 0)
  (ship-types t* 0)
  (ship-types sea 100)
)

(table road-chance
  (town (village town) (50 100))
  (village village 20)
  )

(table road-into-chance
  (land land 100))

;; All units start out completely supplied.

(table unit-initial-supply (u* m* 9999))

;;; Scenario fillins.

;;; Getting the dates right.

(set calendar '(usual hour 8))

(world 25000 (day-length 3))

;;; The usual sides.

(set side-library '(
  ((name "Allies") (adjective "Allied") (class "allied")
   (color "blue") (emblem-name "white-star"))
  ((name "Germany") (adjective "German") (class "german")
   (color "black") (emblem-name "german-cross"))
  ))

(game-module (notes (
  "This is a generic battalion-level set of definitions, strongly oriented"
  "towards the Normandy invasion timeframe."
  ""
  "The basic map scale is 2km/hex, 8 hours/turn."
  )))

(game-module (design-notes (
  )))
