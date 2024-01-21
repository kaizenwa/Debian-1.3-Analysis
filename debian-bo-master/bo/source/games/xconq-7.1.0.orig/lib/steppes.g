(game-module "steppes"
  (title "The Steppes")
  (blurb "The wide-open spaces")
  (base-module "standard")
  (variants (see-all true))
  )

(add t* country-terrain-min 0)
(add t* country-terrain-max 1000)

;;; Uniform land (matches anything with a type called "plains").

(area 60 30)

(area (terrain (by-name "+" plains)
  "60+"
  "60+"
  "60+"
  "30+2a28+"  ; works around a bug in AI code?
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
  "60+"
))
