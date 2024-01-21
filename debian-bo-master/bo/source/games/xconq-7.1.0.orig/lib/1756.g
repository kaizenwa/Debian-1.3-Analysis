(game-module "1756"
  (title "Europe 1756")
  (blurb "Central Europe, the beginning of summer, 1756.  Frederick must strike or die.")
  (base-module "fred")
  (instructions
   "Each side should attempt to capture as many of the other's fortresses "
   "as possible.  "
   )
)

(include "t-cent-eur")

;;; Modify the defaults in fred.

(add u* start-with 0)

;;; All scenarios include these sides, but only three were active in 1756.

(side 1 (name "Prussia") (adjective "Prussian") (emblem-name "arms-prussia"))
(side 2 (name "Hannover") (adjective "Hannoverian") (emblem-name "arms-hannover")
  (active false))
(side 3 (name "Austria") (adjective "Austrian") (emblem-name "arms-austria"))
(side 4 (name "Saxony") (adjective "Saxon") (emblem-name "arms-saxony"))
(side 5 (name "France") (adjective "French") (emblem-name "arms-france")
  (active false))
(side 6 (name "Russia") (adjective "Russian") (emblem-name "arms-russia")
  (active false))
(side 7 (name "Sweden") (adjective "Swedish") (emblem-name "arms-sweden")
  (active false))
(side 8 (name "Empire") (adjective "Empire") #|(emblem-name "arms-sweden")|#
  (active false))

(side 3 (trusts (3 1) (4 1)))
(side 4 (trusts (3 1) (4 1)))

(set sides-max 8)

(town (n "Danzig") (@ 35 35))
(town (n "Hamburg") (@ 12 32))
(town (n "Emden") (@ 8 30))
(town (n "Thorn") (@ 38 29))
(town (n "Bremen") (@ 13 28))
(town (n "Posen") (@ 36 25))
(town (n "Cassel") (@ 18 19))
(town (n "Leipzig") (@ 27 18))
(town (n "Chemnitz") (@ 29 16))
(town (n "Eger") (@ 31 12))
(town (n "Wurzburg") (@ 24 11))
(town (n "Donauworth") (@ 29 6))
(town (n "Regensburg") (@ 32 6))
(town (n "Passau") (@ 36 5))
(town (n "Ulm") (@ 27 4))

(fortress 22 23 1 (n "Magdeburg"))
(fortress (n "Berlin") (@ 25 26))
(fortress (n "Stettin") (@ 26 30))
(fortress (n "Kustrin") (@ 30 25))
(fortress 36 21 1 (n "Glogau"))
(fortress (n "Colberg") (@ 29 34))
(fortress (n "Schweidinitz") (@ 39 17) (s 1))
(fortress 40 18 1 (n "Breslau"))
(fortress (n "Glatz") (@ 40 16) (s 1))
(fortress (n "Brieg") (@ 42 17) (s 1))
(fortress (n "Neisse") (@ 43 15) (s 1))
(fortress (n "Munster") (@ 11 23))
(fortress (n "Minden") (@ 14 24))
(fortress (n "Hannover") (@ 17 25))
(fortress (n "Stade") (@ 12 31))
(fortress (n "Prague") (@ 35 12) (s 3))
(fortress (n "Brunn") (@ 44 7) (s 3))
(fortress (n "Olmutz") (@ 46 9) (s 3))
(fortress (n "Vienna") (@ 46 3) (s 3))
(fortress (n "Koniggratz") (@ 40 13) (s 3))
(fortress (n "Nuremberg") (@ 28 9))
(fortress (n "Torgau") (@ 28 19) (s 4))
(fortress (n "Dresden") (@ 31 17) (s 4))
(fortress (n "Metz") (@ 15 8))
(fortress (n "Strasbourg") (@ 20 5))
(fortress (n "Venlo") (@ 7 20))
(fortress (n "Roermond") (@ 9 18))
(fortress (n "Cologne") (@ 12 17))
(fortress (n "Coblenz") (@ 15 14))
(fortress (n "Dusseldorf") (@ 11 19))
(fortress (n "Mainz") (@ 18 12))
(fortress (n "Wesel") (@ 9 22))
(fortress (n "Frankfort") (@ 20 13))
(fortress (n "Konigsberg") (@ 38 38))
(fortress (n "Stralsund") (@ 20 34))

(king 22 23 1 (n "Frederick"))
(prince 22 23 1 (n "Ferdinand"))
(general 22 23 1 (n "Keith"))
(army 22 23 1 (hp 48))

(marshal 40 18 1 (n "Schwerin"))
(army 40 18 1 (hp 25))

(marshal 36 18 1 (n "Brunswick"))
(general 36 18 1 (n "Maurice"))
(army 36 18 1 (hp 15))
(depot 36 18 1)

(army 42 17 1 (hp 1))
(army 43 15 1 (hp 1))
(army 40 16 1 (hp 1))
(army 39 17 1 (hp 1))
(army 36 21 1 (hp 1))

(marshal 46 9 3 (n "Browne"))
(general 46 9 3 (n "Lascy"))
(army 46 9 3 (hp 50))

(marshal 39 12 3 (n "Picolomini"))
(general 39 12 3 (n "Nadasy"))
(army 39 12 3 (hp 25))

(marshal 35 12 3 (n "Serbeloni"))
(army 35 12 3 (hp 10))

(king 31 17 4 (n "Rutkowski"))
(army 31 17 4 (hp 20))

(army 28 19 4 (hp 1))

(scorekeeper
  (title "VP")
  (initial 0)
  (do (set (sum point-value)))
  )

(set initial-date "Jun 1756")

;;; Game only lasts until the end of the year (armies would
;;; invariably go into winter quarters from December thru March).

(set last-turn 12)
