(game-module "1805"
  (title "Austrian campaign, 1805")
  (base-module "napoleon")
  (variants (see-all true))
  (notes (
  "The French must try to capture Vienna, the Austrians must try"
  "to keep it, all within 5 months (turns)."
  ))
  )

;; No unknown terrain in Europe at this time.

(set terrain-seen true)

(include "eur-50km")

(add u* assign-number false)

(table acp-to-create (u* u* 0))

;; Suppress random unit generation.

(add u* start-with 0)
(table independent-density 0)

(set sides-min 6)
(set sides-max 6)

(side 1 (name "France") (adjective "French") (emblem-name "flag-france"))
(side 2 (name "England") (adjective "English") (emblem-name "flag-uk")
  (active false))
(side 3 (name "Spain") (adjective "Spanish") (emblem-name "flag-spain")
  (active false))
(side 4 (name "Austria") (adjective "Austrian") (emblem-name "arms-austria"))
(side 5 (name "Prussia") (adjective "Prussian") (emblem-name "arms-prussia")
  (active false))
(side 6 (name "Russia") (adjective "Russian") (emblem-name "arms-russia"))

(set initial-date "Jul 1805")

(set initial-year-part 6)

(set last-turn 5)

(add u* point-value 0)
(add capital point-value 1)

;;; Owner of Vienna only should be the winner.

(scorekeeper 
  (title "VP")
  (initial 0)
  (do (set (sum point-value)))
  )

(capital 70 31 0 (n "Rome"))
(capital 56 69 0 (n "Stockholm"))

(inf 56 55 1)
(inf 49 55 1)
(inf 49 55 1)
(inf 61 50 1)
(inf 57 42 1)
(inf 57 49 1)
(inf 38 48 1 (hp 4))
(inf 67 34 1 (hp 4))
(inf 53 50 1)
(inf 63 38 1 (hp 3))
(inf 55 49 1 (hp 8))
(inf 53 53 1 (hp 3))
(inf 56 51 1 (hp 3))
(guards 56 49 1 (hp 2))
(cav 61 50 1)
(cav 54 51 1)
(cav 55 49 1)
(cav 55 49 1)
(ldr 53 53 1 (n "Bernadotte"))
(ldr 53 50 1 (n "Davout"))
(ldr 55 49 1 (n "Lannes"))
(ldr 56 51 1 (n "Marmont"))
(ldr 63 38 1 (n "Massena"))
(ldr 55 48 1 (n "Napoleon"))
(ldr 56 49 1 (n "Ney"))
(ldr 53 51 1 (n "Soult"))
(ldr 49 55 1)
(ldr 61 49 1)
(ldr 61 50 1)
(city 49 55 1 (n "Amsterdam"))
(city 57 45 1 (n "Basel"))
(city 38 48 1 (n "Brest"))
(city 50 52 1 (n "Brussels"))
(city 46 52 1 (n "Calais"))
(city 41 50 1 (n "Cherbourg"))
(city 56 51 1 (n "Coblenz"))
(city 53 53 1 (n "Cologne"))
(city 67 34 1 (n "Florence"))
(city 57 42 1 (n "Geneva"))
(city 57 54 1 (n "Hanover"))
(city 45 49 1 (n "Le Havre"))
(city 48 51 1 (n "Lille"))
(city 54 50 1 (n "Metz"))
(city 63 38 1 (n "Milan"))
(city 43 45 1 (n "Nantes"))
(city 52 49 1 (n "Rheims"))
(city 56 48 1 (n "Strasbourg"))
(city 57 49 1 (n "Stuttgart"))
(city 50 36 1 (n "Toulouse"))
(city 61 50 1 (n "Wurzburg"))
(capital 49 48 1 (n "Paris"))

(city 43 53 2 (n "Dover"))
(city 41 58 2 (n "Norwich"))
(city 38 53 2 (n "Plymouth"))
(city 41 53 2 (n "Portsmouth"))
(capital 42 54 2 (n "London"))

(capital 42 31 3 (n "Madrid"))

(inf 65 43 4 (hp 3))
(inf 72 44 4)
(inf 68 48 4 (hp 1))
(inf 66 47 4 (hp 1))
(inf 66 38 4)
(inf 66 38 4 (hp 4))
(inf 60 48 4)
(cav 59 47 4)
(cav 60 47 4)
(cav 65 43 4)
(cav 60 48 4)
(ldr 66 38 4 (n "Charles"))
(ldr 60 48 4 (n "Ferdinand"))
(ldr 66 38 4 (n "Hiller"))
(ldr 60 48 4 (n "Mack"))
(ldr 65 43 4)
(ldr 72 44 4)
(city 65 43 4 (n "Innsbruck"))
(city 68 48 4 (n "Prague"))
(city 66 47 4 (n "Ratisbon"))
(city 59 48 4 (n "Ulm"))
(city 67 38 4 (n "Venice"))
(capital 78 42 4 (n "Budapest"))
(capital 63 47 4 (n "Munich"))
(capital 72 44 4 (n "Vienna"))

(inf 62 54 5 (hp 1))
(capital 62 54 5 (n "Berlin"))

(inf 74 51 6 (hp 7))
(inf 73 57 6 (hp 3))
(inf 74 54 6)
(guards 74 55 6 (hp 2))
(cav 75 51 6)
(cav 74 57 6)
(cav 75 54 6)
(ldr 74 51 6 (n "Bagration"))
(ldr 73 57 6 (n "Benningsen"))
(ldr 74 54 6 (n "Buxhowden"))
(ldr 74 54 6 (n "Constantine"))
(ldr 74 51 6 (n "Kutuzov"))
(city 74 54 6 (n "Brest-Litovsk"))
(city 73 57 6 (n "Grodno"))
(city 70 59 6 (n "Kovno"))
(city 74 51 6 (n "Lublin"))
(city 78 47 6 (n "Lvov"))
(city 68 60 6 (n "Memel"))
(capital 86 62 6 (n "Moscow"))


(area (aux-terrain road
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "66acu92a"
  "68a@91a"
  "63ai5a>su88a"
  "63aj8aD87a"
  "63aj8aj87a"
  "61aeaf2ai5ab87a"
  "62a>sN{r93a"
  "57acu6aj94a"
  "59a@5aj94a"
  "60a@4aj94a"
  "61a<3ab94a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  ))

(area (aux-terrain river
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "68ami90a"
  "68ab?}i88a"
  "70ab?y87a"
  "54ami10agq4ady86a"
  "54ab?}i8acT5ab86a"
  "56ab?}i7ahq91a"
  "48a5mi4ab?y6acXi90a"
  "48ab4=?}i4ady6ab?y89a"
  "54ab?y4ad}i6ad}i87a"
  "56ahq4ab?}i5abCq86a"
  "56agL6ab?}i4agL86a"
  "56agLae4miab?y3agL86a"
  "56agLagM3=?}iab4a<86a"
  "56agL2a<4ab?y92a"
  "56acXmi7ad}2mi88a"
  "57ab2=8ab2=?y87a"
  "72ad}2mi83a"
  "73ab2=?y82a"
  "77ady81a"
  "78ahq80a"
  "78acT80a"
  "79ady79a"
  "62ae4mi12ahq9aemi66a"
  "63a5=12acT9agM=66a"
  "81ad}8moL67a"
  "82ab9=<67a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  "160a"
  ))
