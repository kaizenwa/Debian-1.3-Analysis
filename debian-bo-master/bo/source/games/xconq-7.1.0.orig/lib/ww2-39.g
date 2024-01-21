(game-module "ww2-39"
  (title "WWII Sep 39")
  (blurb "Full WWII, September 1939.  This is a monster game, based on the advanced ww2 unit types.")
  (base-module "ww2-adv")
  (variants
   (see-all true)
   )
  (instructions (
   "In this game you play the national leader of your country during WWII."
   ))
  )

;;; Define basic terrain.

(include "earth-1deg")

(set synthesis-methods '(name-units-randomly))

(include "ww2-sides")

;;; Define the nationalities of the people.

(include "p-e1-1938")

;;; Define the cities.

(include "u-e1-1938")

;; (add minor countries also?)

;; (set up agreements applying to particular dates)

;; not really correct

(scorekeeper (do last-side-wins))

;; Default to setting up at the historical outbreak of the war.

(set initial-date "Sep 1939")
