(game-module "relief"
  (blurb "A test game for displaying something resembling relief maps.")
  (variants (world-size))
)

(unit-type C (name "caveman") (char "C") (image-name "person"))

(terrain-type a (char "a") (color "midnight blue"))
(terrain-type b (char "b") (color "midnight blue"))
(terrain-type c (char "c") (color "navy blue"))
(terrain-type d (char "d") (color "cadet blue"))
(terrain-type e (char "e") (color "blue"))
(terrain-type f (char "f") (color "medium blue"))
(terrain-type g (char "g") (color "sky blue"))
(terrain-type h (char "h") (color "light blue"))
(terrain-type i (char "i") (color "aquamarine"))
(terrain-type j (char "j") (color "cyan"))
(terrain-type k (char "k") (color "forest green"))
(terrain-type l (char "l") (color "sea green"))
(terrain-type m (char "m") (color "green"))
(terrain-type n (char "n") (color "green yellow"))
(terrain-type o (char "o") (color "yellow green"))
(terrain-type p (char "p") (color "yellow"))
(terrain-type q (char "q") (color "khaki"))
(terrain-type r (char "r") (color "sienna"))
(terrain-type s (char "s") (color "gray"))
(terrain-type t (char "t") (color "white"))

;(set alt-smoothing 10)

(add t* alt-percentile-min ( 0  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90  95 ))
(add t* alt-percentile-max ( 5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100 ))
(add t* wet-percentile-min   0)
(add t* wet-percentile-max 100)

(add t* elevation-min 0)
(add t* elevation-max 1000)

(area (cell-width 100))

(add C acp-per-turn 1)

(set see-all true)

(set sides-min 1)

