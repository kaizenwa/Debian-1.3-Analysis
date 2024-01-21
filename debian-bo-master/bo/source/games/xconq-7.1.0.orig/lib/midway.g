(game-module "midway"
  (title "Midway")
  (blurb "Battle of Midway")
  (base-module "ww2-div-pac")
  (variants
   (see-all false)
   )
  )

(set synthesis-methods nil)

(set initial-date "1 Jun 1942")

(scorekeeper (do last-side-wins))

(world 800)

(area 100 80)

(area (terrain
  (by-name
    (sea 0) (atoll 1) (clear 2) (jungle 3) (hills 4)
    (mountains 5) (road 6) (river 7) (reef 8))
  "12a6f32a4f46a"
  "11a8f31a3f2a3c42a"
  "11a7f32a2f3a3c42a"
  "10a8f4ac27af49a"
  "10a8f6ac24ac50a"
  "10a8f30ac51a"
  "10a7f11ac16acac52a"
  "10a6f16ac9acac55a"
  "11a5f20ac3ac59a"
  "11a4f85a"
  "12a3f85a"
  "12a3f85a"
  "12a2f86a"
  "100a"
  "100a"
  "100a"
  "12af87a"
  "100a"
  "100a"
  "11af88a"
  "100a"
  "3ac5af90a"
  "3a2c95a"
  "a5c94a"
  "2a6c92a"
  "2a5c93a"
  "3a3c94a"
  "100a"
  "2a2c96a"
  "3a2c95a"
  "3a3c94a"
  "3a3c94a"
  "4a3c93a"
  "4a3c93a"
  "4a3c93a"
  "4a4c92a"
  "4a4c92a"
  "2a7c91a"
  "a8c91a"
  "9c91a"
  "a6c93a"
  "5a2c93a"
  "3a2c95a"
  "4a2c94a"
  "5ac94a"
  "100a"
  "100a"
  "100a"
  "100a"
  "100a"
  "100a"
  "100a"
  "100a"
  "100a"
  "100a"
  "100a"
  "100a"
  "63ac36a"
  "27ac72a"
  "100a"
  "100a"
  "100a"
  "100a"
  "100a"
  "80ac19a"
  "83ac16a"
  "85ac14a"
  "87ac12a"
  "87a2c11a"
  "48ac51a"
  "100a"
  "100a"
  "100a"
  "100a"
  "36ac63a"
  "100a"
  "100a"
  "100a"
  "40ac59a"
  "100a"
))

(side 1 (name "Allies") (adjective "Allied") (emblem-name "flag-usa")
   (class "allied") (names-locked true))

(side 2 (name "Japan") (adjective "Japanese") (emblem-name "flag-japan")
   (class "japanese") (names-locked true))


(cv 81 16 1 (n "Enterprise"))
(cv 82 15 1 (n "Hornet"))
(cv 80 17 1 (n "Yorktown"))
(airbase 63 22 1 (n "Midway"))
(port 48 74 1 (n "Dutch Harbor"))
(town 83 14 1 (n "Honolulu"))

(ca 5 37 2 (n "Nachi"))
(ca 5 37 2 (n "Takao"))
(cv 3 38 2 (n "Akagi"))
(cv 3 38 2 (n "Hiryu"))
(cv 3 38 2 (n "Kaga"))
(cv 3 38 2 (n "Soryu"))
(bb 4 38 2 (n "Fuso"))
(bb 4 38 2 (n "Haruna"))
(bb 4 38 2 (n "Hiei"))
(bb 4 38 2 (n "Hyuga"))
(bb 4 38 2 (n "Ise"))
(bb 4 38 2 (n "Kirishima"))
(bb 4 38 2 (n "Kongo"))
(bb 4 38 2 (n "Mutsu"))
(bb 4 38 2 (n "Nagato"))
(bb 4 38 2 (n "Yamashiro"))
(bb 4 38 2 (n "Yamato"))
(airfield 40 1 2 (n "Guam"))
(airfield 27 21 2 (n "Marcus Island"))
(airfield 36 5 2 (n "Saipan"))
(airbase 48 10 2 (n "Wake Island"))
(port 40 1 2 (n "Guam"))
(port 2 39 2 (n "Kure"))
(port 3 51 2 (n "Ominato"))
(port 36 5 2 (n "Saipan"))
(city 8 40 2 (n "Tokyo"))


(game-module (design-notes (
  "The map extends from 15 to 55 deg N, 130 E to 130 W."
  )))
