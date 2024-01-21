(game-module "ww2-stdunit"
  (title "WWII in Standard Game")
  (blurb "The standard game, modified for WWII scenarios")
  )

;; Note that although we *include* stdunit, it is not a base module,
;; since we want this module to be a base module for scenarios.
;; (This is an example of why base-module is a property and not
;; something calculated from inclusion.)
 
(include "stdunit")

;; Preset the sides, which restricts this to post-Dec 1941, when the
;; sides were clearly lined up.

(side 1 (name "Allies"))

(side 2 (name "Axis"))

(set sides-min 2)
(set sides-max 2)

;; The terrain of the world will always be known.

(set terrain-seen true)

(game-module (design-notes
  "This is a set of modifications to stdunit that adapt it to the WWII era."
  "As such, it is quite lacking in accuracy, but has the advantage that it is"
  "simpler and more familiar."
  ""
  "This should be included in specific scenarios that have associated dates."
  ))
