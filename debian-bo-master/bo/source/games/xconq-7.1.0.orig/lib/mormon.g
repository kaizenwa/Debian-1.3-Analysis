(game-module "mormon"
  (title "mormon")
  (blurb "The heroic age of the heroic Mormon pioneers")
  (variants (world-seen true) (see-all))
  )

(unit-type mormon (char "m") (image-name "man")
  (help "prolific settlers"))
(unit-type avenging-angel (name "avenging angel") (char "A") (image-name "trooper")
  (help "Mormon police/army/vigilantes"))
(unit-type prophet (char "P") (image-name "man")
  (help "prolific leader"))
(unit-type army (char "a") (image-name "cavalry")
  (help ""))
(unit-type indian (name "Indian") (char "u") (image-name "archer")
  (help ""))
(unit-type fort (char "/") (image-name "walltown")
  (help "where the army hangs out"))
(unit-type settlement (char "*") (image-name "walltown")
  (help ""))
(unit-type temple (char "@") (image-name "city18")
  (help ""))

(material-type water (help "very scarce in Utah"))

(terrain-type lake (char ".") (color "sky blue"))
(terrain-type river (char ",") (color "cyan"))
(terrain-type valley (char "+") (color "green"))
(terrain-type forest (char "%") (color "forest green"))
(terrain-type desert (char "~") (color "yellow"))
(terrain-type mountains (char "^") (color "sienna"))
(terrain-type salt-flat (char "_") (color "white"))
(terrain-type not-Utah (char ":") (color "black"))

(define places (fort settlement temple))

;;; Static relationships.

(table vanishes-on
  (u* (lake not-Utah) true)
  )

(add (settlement temple) capacity 8)

(table unit-capacity-x
  (fort army 6)
  ((settlement temple) army 1)
  )

(table unit-size-as-occupant
  (u* u* 100) ; disables occupancy usually
  ((mormon avenging-angel prophet) (fort settlement temple) 1)
  )

(table unit-size-in-terrain
  (u* t* 1)
  ((settlement temple) t* 4)
  )

(add t* capacity 4)

;;; Unit-material capacities.

(table unit-storage-x
  (u* water 2)
  (army water 4)
  (places water 100)
  )

;;; Actions.

;;;; scale should be 15 km?

(add (mormon avenging-angel army indian prophet) acp-per-turn (2 4 4 4 6))

(add (settlement temple) acp-per-turn 1)

;;; Movement.

(add places speed 0)

(table mp-to-enter-terrain
  (u* (lake not-Utah) 99)
  )

;;; Construction.

(add (settlement fort) cp 5)

(table acp-to-create
  (prophet mormon 6)
  (mormon mormon 2)
  (settlement mormon 1)
  (temple avenging-angel 1)
  (mormon settlement 2)
  (army fort 4)
  )

(table cp-on-creation
  (mormon settlement 1)
  (army fort 1)
  )

(table acp-to-build
  (mormon settlement 2)
  (army fort 4)
  )

(table cp-per-build
  (mormon settlement 1)
  (army fort 1)
  )

(table supply-on-creation
  ;; Minimal water supply is free.
  (u* water 1)
  )

(add u* hp-recovery 1.00)
(add army hp-recovery 0.50)

;;; Production (water).

(table base-production
  (u* water 1)
  )

(table productivity
  (u* t* 0)
  (u* (valley river) 100)  ; note that the lake is salt, so useless
  (u* forest 200)
  (army mountains 50)
  ;; Indians can get water anywhere but in salt flats.
  (indian t* 100)
  (indian salt-flat 0)
  (u* valley 100)
  )

(table base-consumption
  (u* water 1)
  )

(table hp-per-starve
  ;; Might be able to do without water, but not for long.
  (u* water 0.50)
  )

(table unit-initial-supply
  (u* water 9999)
  )

;"dies of thirst" (mormon avenging-angel a) starve-message

;;; Combat.

(add u* hp-max (4 4 2 4 2 16 16 16))

(table hit-chance
  (u* u* 50)
  (mormon army 5)
  ((army indian) mormon 95)  ; armies and indians are deadly
  ((army indian) prophet 50)  ; prophet is wily
  (army avenging-angel 30)
  (avenging-angel army 50)      ; avenging angels are best way to hit back
  )

(table damage
  (u* u* 1)
  (army u* 1d2+1)
  (indian u* 1d4+1) ; indians can be very destructive sometimes
  )

(table capture-chance
  (army (settlement temple) (10 5))
  )

;5000 army siege

(add u* initial-seen-radius 4)

;;; Scoring.

(scorekeeper (do last-side-wins))

(world 50)  ; hexagon would be better

(area 50 42)

(area (terrain
  "50h"
  "6ecfcfc2af33h3e"
  "2f5ecfcf2a2f32h2ef"
  "8ecfcfafd33h2e"
  "4e2a3ecfc2fdf32hef"
  "e2g2e2a2ecf2c2fd33he"
  "fe2ge3aeacf2cfdf32he"
  "2e3ge4aecf2cfd33h"
  "fe4ge3aecf3cdf32h"
  "he3gfge3aecf4c10dc3e18h"
  "he3gfge2ae2acf3c2d7f2dc2e18h"
  "2he5g2e3aecf2cd10fce2b17h"
  "2h2e4g3ea2ecf3c3d6fc2ebe17h"
  "3he3g3ef2efcf2cf3dc5ece2b2e16h"
  "3h2e3g3efefecfc2d3c7eb3e16h"
  "4h2e2g3ef2efacfcfdf9eb3e15h"
  "4h4eg3efefeacfcfd2f7eb3e15h"
  "5h7ef3eacfc2fd4f4ebe3f14h"
  "5h4ef7ecfc2efce4f2ebefdf14h"
  "6h4ef4efecfefefece4febefd2f13h"
  "6h2ef2ef4efcef2efe2c3efebef2df13h"
  "7h2efef6efefef5ec2ebefd3f12h"
  "7h2ef2ef2e2cef2efef8ebe2f2e12h"
  "8h2efef2e3c2ef2e2f5ecebe2f2eb11h"
  "8h2ef4e3cecfce2f7ebe2f2eb11h"
  "9h5ea6ece2f7ecbefe2be10h"
  "9h3efea5efc3ef7eb3eb2e10h"
  "10h2efea4ecfecfef8eb2eb3e9h"
  "10hefef2ef5ec11eb2ebefe9h"
  "11hefefef4e2f6ef5ebeb2efe8h"
  "11hef2ef4ec2ef5e2f5e2b2efe8h"
  "12hefe2f4ef2e2f4ef5eb6e7h"
  "12h2e2f6efef2ef2e2f4eb6e7h"
  "13h2f5ec2f5ef2ef3e2b3e2f2e6h"
  "13h5ec2e2f5ef2ef2e2b4e3fe6h"
  "14h6ec2f11eb6e2f2e5h"
  "14h3ef3ef11e2b10e5h"
  "15h2efe2cf11ebe5b6e4h"
  "15h2efec11e3b6e6b4h"
  "16h3ec11eb15e3h"
  "16h10ec4eb2e2f11e3h"
  "50h"
))

(side 1 (name "US"))

(side 2 (noun "Mormon"))

(side 3 (noun "Ute") (adjective "Ute"))

(fort 14 31 1 (n "Ft Douglas"))
(army 14 31 1 (in "Ft Douglas"))
(army 15 31 1)
(army 16 31 1)
(army 16 32 1)
(army 17 32 1)
(army 18 32 1)

(temple 14 30 2 (n "Salt Lake City"))
(prophet 14 30 2 (n "Brigham Young") (in "Salt Lake City"))
(avenging-angel 14 30 2 (in "Salt Lake City"))
(mormon 14 30 2 (in "Salt Lake City"))

(settlement 15 27 2 (n "Orem"))
(settlement 17 25 2 (n "Provo"))
(settlement 14 29 2 (n "Sandy"))
(settlement 12 32 2 (n "Ogden"))

(indian  9 39 3)
(indian  9 37 3)
(indian 14 37 3)
(indian 14 34 3)
(indian 22 28 3)
(indian 18 26 3)
(indian 17 23 3)
(indian 16 18 3)
(indian 21 16 3)

(game-module (instructions (
  "The Mormons should try to reproduce themselves "
  "and spread out as much as possible "
  "before the cavalry catches up with them."
  "Use the Avenging Angels to ambush the cavs."
  ""
  "The US cavalry just has to kill as many Mormons "
  "as fast as they can."
  )))

(game-module (notes (
  "Can the US army kill all the Mormons before they overpopulate Utah?"
  ""
  "(This is all a joke of course.)"
  )))

(game-module (design-notes (
  "Needs some cleanup and balancing work"
  )))
