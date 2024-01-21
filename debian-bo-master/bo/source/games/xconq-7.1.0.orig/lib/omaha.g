(game-module "omaha"
  (title "Omaha Beach Landings")
  (base-module "ww2-bn")
  (variants
   (see-all false)
   (sequential true)
   )
  (instructions (
   ))
  )

(scorekeeper (do last-side-wins))

(set initial-date "0:00 6 Jun 1944")

(set initial-day-part 0.50)

(set last-turn 9)

(add u* start-with 0)

(side 1 (name "Allies") (adjective "Allied") (class "allied")
   (color "blue") (emblem-name "white-star"))

(side 2 (name "Germany") (adjective "German") (class "german")
   (color "black") (emblem-name "german-cross"))

(set sides-min 2)
(set sides-max 2)

(area 40 19 (restrict 186 159 55 80))

(include "t-normandy") ; has to go here so towns get on German side

(unit-defaults (s 1))

(landing-ship 90 90)
(landing-ship 89 91)
(landing-ship 88 92)

(inf-mot 90 90)
(inf-mot 89 91)
(inf-mot 88 92)
