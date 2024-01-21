(game-module "chess"
  (title "Xconq Chess")
  (blurb "Xconq's answer to chess")
  (instructions "This is more of an experiment than a real game."
    "Move your pieces to try to capture the other player's king.")
  )

(unit-type w-pawn)
(unit-type w-rook)
(unit-type w-knight)
(unit-type w-bishop)
(unit-type w-queen)
(unit-type w-king)

(define w-types (w-pawn w-knight w-bishop  w-queen w-king))

(add w-types possible-sides "white")

(unit-type b-pawn)
(unit-type b-rook)
(unit-type b-knight)
(unit-type b-bishop)
(unit-type b-queen)
(unit-type b-king)

(define b-types (b-pawn b-knight b-bishop  b-queen b-king))

(add b-types possible-sides "black")

;(add u* assign-numbers false)

(terrain-type edge (color "gray"))
(terrain-type row1 (color "red"))
(terrain-type row2 (color "orange red"))
(terrain-type row3 (color "orange"))
(terrain-type row4 (color "yellow"))
(terrain-type row5 (color "yellow green"))
(terrain-type row6 (color "green"))
(terrain-type row7 (color "turquoise"))
(terrain-type row8 (color "blue"))
(terrain-type row9 (color "violet"))
(terrain-type row10 (color "purple"))
(terrain-type row11 (color "purple4"))

;;; Vision.

(set see-all true)

;;; Actions.

(add (w-pawn b-pawn) acp-per-turn 1)
(add (w-rook b-rook) acp-per-turn 13)
(add (w-knight b-knight) acp-per-turn 2)
(add (w-bishop b-bishop) acp-per-turn 13)
(add (w-queen b-queen) acp-per-turn 13)
(add (w-king b-king) acp-per-turn 1)

(add (w-pawn b-pawn) speed 10.00)

(add (w-pawn b-pawn) free-mp 0)

(table mp-to-enter-terrain
  (u* t* 1)
  (w-pawn t* (10 5 5 4 4 3 3 2 2 1 1 0))
  (b-pawn t* (10 0 1 1 2 2 3 3 4 4 5 5))
  )

(table mp-to-leave-terrain
  (u* t* 0)
  (w-pawn t* (10  5  6 6 7 7 8 8 9 9 10 10))
  (b-pawn t* (10 10 10 9 9 8 8 7 7 6  6  5))
  )

(table acp-to-attack (u* u* 1))

(table hit-chance (u* u* 100))

(table damage (u* u* 1))

;;; Scoring.

(add u* point-value 0)
(add (w-king b-king) point-value 1)

(scorekeeper 1 (do last-side-wins))

(area 15 13)

(area (terrain
  (by-name
    (edge 0) (row1 1) (row2 2) (row3 3) (row4 4)
    (row5 5) (row6 6) (row7 7) (row8 8) (row9 9)
    (row10 10) (row11 11))
  "15a"
  "a8l6a"
  "a9k5a"
  "a10j4a"
  "a11i3a"
  "a12h2a"
  "a13ga"
  "2a12fa"
  "3a11ea"
  "4a10da"
  "5a9ca"
  "6a8ba"
  "15a"
))

(side 1 (name "White") (class "white"))

(side 2 (name "Black") (class "black"))

(w-pawn 5 2 1)
(w-pawn 6 2 1)
(w-pawn 7 2 1)
(w-pawn 8 2 1)
(w-pawn 9 2 1)
(w-pawn 10 2 1)
(w-pawn 11 2 1)
(w-pawn 12 2 1)
(w-pawn 13 2 1)
(w-rook 6 1 1)
(w-rook 13 1 1)
(w-knight 7 1 1)
(w-knight 12 1 1)
(w-bishop 8 1 1)
(w-bishop 11 1 1)
(w-queen 9 1 1)
(w-king 10 1 1)

(b-pawn 1 10 2)
(b-pawn 2 10 2)
(b-pawn 3 10 2)
(b-pawn 4 10 2)
(b-pawn 5 10 2)
(b-pawn 6 10 2)
(b-pawn 7 10 2)
(b-pawn 8 10 2)
(b-pawn 9 10 2)
(b-rook 1 11 2)
(b-rook 8 11 2)
(b-knight 2 11 2)
(b-knight 7 11 2)
(b-bishop 3 11 2)
(b-bishop 6 11 2)
(b-queen 4 11 2)
(b-king 5 11 2)

