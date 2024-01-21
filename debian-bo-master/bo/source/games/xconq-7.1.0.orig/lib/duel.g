(game-module "duel"
  (title "The Duel")
  (blurb "Tanks, one-on-one.")
  (base-module "tank")
  (variants
   (see-all true)
   ("More Tanks" (true (add u* start-with 1)))
   ) 
)

(set use-side-priority 1)

(area 20 20)

(area (terrain (by-name "+" plains)
  "20+"
  "20+"
  "20+"
  "20+"
  "20+"
  "20+"
  "20+"
  "20+"
  "20+"
  "20+"
  "20+"
  "20+"
  "20+"
  "20+"
  "20+"
  "20+"
  "20+"
  "20+"
  "20+"
  "20+"
))

;; Limit to two players only.

(set sides-min 2)
(set sides-max 2)

;; Two sides, no special properties needed (?).

(side 1)

(side 2)

;; Two tanks, one for each side, at predetermined locations.

(tank 10  7 1)

(tank 10 13 2)

(scorekeeper #|(when after-event)|# (do last-side-wins))
