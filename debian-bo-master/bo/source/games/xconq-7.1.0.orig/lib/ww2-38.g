(game-module "ww2-38"
  (title "WWII Jan 38")
  (blurb "Full WWII, January 1938.  This is a monster game, based on the advanced ww2 unit types.")
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

;; Default to setting up at the beginning of 1938.

(set initial-date "Jan 1938")
