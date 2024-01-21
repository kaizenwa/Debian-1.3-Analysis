(game-module "red-october"
  (title "October Revolution")
  (blurb "Russian revolution of October 1917")
  (base-module "russian-rev")
  (variants
    (see-all false)
  )
)

(set terrain-seen true)

(include "u-rus-1910")

(set synthesis-methods '(make-initial-materials))

(set calendar '(usual "day" 2)) ; ?
(set initial-date "8 Nov 1917") ; but October, old style

(side 1 (name "Provisional Government") (noun "Governative") ; Kerenskij
  (color "blue") (emblem-name "flag-russia")) ; which emblem ?
(side 2 (adjective "Bolshevik")
  (color "red") (emblem-name "hammer-and-sickle"))
(side 3 (name "German Empire") (adjective "German") 
  (color "gray,black,white") (emblem-name "german-cross"))
(side 4 (name "Ukraine") (noun "Ukrainian")
  (color "yellow,blue") (emblem-name "flag-ukraine"))

(unit "Riga" (s 1))
(unit "Minsk" (s 1))
(unit "Odessa" (s 1))
(unit "Tallin" (s 1))
(unit "Sevastopol'" (s 1))
(fleet 25 11 1 (n "Black Sea fleet"))
(infantry 24 13 1)
(infantry 13 28 1)
(infantry 10 35 1) 
(infantry  8 33 1)

(unit "Petrograd" (s 2))
(unit "Helsingfors" (s 2))
(militia 10 40 2 (n "1st Red Guard"))
(militia 10 40 2 (n "2nd Red Guard"))
(militia 10 40 2 (n "3rd Red Guard"))
(fleet 9 40 2 (n "Baltic fleet"))
(militia 9 40 2 (n "Baltic fleet sailors"))

(unit "Warszawa" (s 3))
(unit "Danzig" (s 3))
(unit "Konigsberg" (s 3))
(unit "Warszawa" (s 3))
(unit "Krakow" (s 3))
(unit "Lodz" (s 3))
(unit "Lvov" (s 3))
(unit "Katowice" (s 3))
(infantry 21 13 3)
(infantry 16 18 3)
(infantry 16 21 3)
(infantry 13 24 3)
(infantry 10 29 3)
(infantry  9 31 3)
(infantry  7 32 3)
(train 8 24 3)
(train 6 29 3)
(fleet 5 29 3)

(unit "Kiev" (s 4))
(unit "Rostov" (s 4))
(militia 19 22 4)
(militia 19 22 4)
(cavalry 35 14 4 (n "1st Cossack"))
(cavalry 36 14 4 (n "2nd Cossack"))

(train 22 32 0)
(train 128 7 0)

(game-module (design-notes (
  "This scenario is under development;"
  "Massimo Campostrini (campo@sunthpi3.difi.unipi.it)."
  )))

(game-module (notes (
  "The Governatives should try to crush the Revolution"
  "and contain the Germans at the same time."
  )))
