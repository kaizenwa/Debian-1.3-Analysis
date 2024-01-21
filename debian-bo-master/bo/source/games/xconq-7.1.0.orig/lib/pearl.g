(game-module "pearl"
  (title "Pearl Harbor")
  (blurb "Japan launches its invasion of the Far East")
  (base-module "ww2-div-pac")
  (variants (see-all true))
  )

(area 30 20 (terrain
  "30a"
  "30a"
  "30a"
  "30a"
  "30a"
  "30a"
  "30a"
  "30a"
  "30a"
  "9ac20a"
  "13a2c15a"
  "16ac13a"
  "18ac11a"
  "20ac9a"
  "20a2c8a"
  "21ac8a"
  "30a"
  "30a"
  "30a"
  "30a"
  ))

(set initial-date "7 Dec 1941")

(set last-turn 1)

(set synthesis-methods nil)

(side 1 (name "IJN"))

(side 2 (name "USN"))

(ca 8 18 1 (n "Tone"))
(dd 8 18 1 (n "Kagero"))
(cv 8 18 1 (n "Akagi"))
(cv 8 18 1 (n "Hiryu"))
(cv 8 18 1 (n "Kaga"))
(cv 8 18 1 (n "Shokaku"))
(cv 8 18 1 (n "Soryu"))
(cv 8 18 1 (n "Zuikaku"))
(bb 8 18 1 (n "Hiei"))
(bb 8 18 1 (n "Kirishima"))

(town 14 9 2 (n "Honolulu") (# 100) (act (acp 0)))
(airbase 14 9 2 (in 100))
(bb 14 9 2 (n "Arizona") (in 100) (act (acp 0)))
(bb 14 9 2 (n "California") (in 100) (act (acp 0)))
(bb 14 9 2 (n "Maryland") (in 100) (act (acp 0)))
(bb 14 9 2 (n "Nevada") (in 100) (act (acp 0)))
(bb 14 9 2 (n "Oklahoma") (in 100) (act (acp 0)))
(bb 14 9 2 (n "Pennsylvania") (in 100) (act (acp 0)))
(bb 14 9 2 (n "Tennessee") (in 100) (act (acp 0)))
(bb 14 9 2 (n "West Virginia") (in 100) (act (acp 0)))

