(game-module "tank"
  (title "Tank")
  (blurb "Simple tank battles")
  (variants
    (see-all false)
    (world-seen false)
    (world-size (20 20 10000))
	)
  )

(terrain-type plains (color "green") (char "+"))
(terrain-type hills (color "brown") (char "^"))

(unit-type tank (image-name "tank") (char "T")
  (start-with 1)
  (acp-per-turn 2)
  (hp-max 10)
  )

(table hit-chance
  (tank tank 100)
  )

(table damage
  (tank tank 1d4)
  )

(add tank vision-range 6)

(add t* occurrence (50 1))

(set synthesis-methods '(make-random-terrain make-countries))

;;; Don't want the world to wrap around.

;(world 10000) ; causes a warning, unclear if justified warning...

