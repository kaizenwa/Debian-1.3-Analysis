(game-module "standard"
  (title "Standard Game")
  (blurb "The standard Xconq game, loosely based on WW II ca 1945")
  (instructions "Take over the world before you get taken over!")
  (variants
    (world-seen false)
    (see-all false)
    (world-size (60 30 360))
    (sequential false)
    ("Mostly Land" mostly-land
      (true
        ;; Adjust so that sea is 20% instead of 70% of the world.
        (add sea alt-percentile-max 20)
        (add shallows alt-percentile-min 20)
        (add shallows alt-percentile-max 21)
        (add swamp alt-percentile-min 21)
        (add swamp alt-percentile-max 23)
        (add (desert plains forest) alt-percentile-min 21)
        ))
    ("All Land" all-land
      (true
        ;; Adjust sea and shallows out of existence, let swamp take all the low spots.
        (add sea alt-percentile-min 0)
        (add sea alt-percentile-max 0)
        (add shallows alt-percentile-min 0)
        (add shallows alt-percentile-max 0)
        (add swamp alt-percentile-min 0)
        (add swamp alt-percentile-max 2)
        (add swamp wet-percentile-min 0)
        (add swamp wet-percentile-max 100)
        (add (desert plains forest) alt-percentile-min 2)
        ;; Counterproductive to try to set up near water.
		(add sea country-terrain-min 0)
        ))
    ("Large Countries" large
     (true
       ;; This is the same as country separation.
       (set country-radius-max 48)
       ))
    ("Test" test
     (true
       ;; For testing.
	   (set unseen-image-name "question")
       (include "libimf")
       ))
    ("Noisy" noisy
 (true
(set action-messages '(
  (move (sound "pop"))
  ))

(set event-messages '(
  (side-lost (sound "thunder"))
  (unit-captured (sound "chirr"))
  (unit-completed (sound "chirr"))
  (unit-moved (sound "pop 2"))
  ))
     ))
    )
  )

(include "stdunit")

(include "nat-names")

(include "town-names")

(add (* @) namer "random-town-names")

(set feature-types (("peak") ("lake")))

(include "ng-weird")

(namer generic-lake-names (grammar root 10
  (root (or 5 (foo "Lake " generic-names)
            1 (generic-names " Lake")
            ))
  (foo "")  ; works around a bug
  ))

(set feature-namers (("lake" "generic-lake-names")))

(set advantage-min 1)
(set advantage-default 1)
(set advantage-max 5)

(scorekeeper
  (title "")
  (do last-side-wins)
  )

