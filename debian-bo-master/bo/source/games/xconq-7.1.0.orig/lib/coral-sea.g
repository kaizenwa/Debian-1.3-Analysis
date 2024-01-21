(game-module "coral-sea"
  (title "Coral Sea")
  (blurb "Battle in the Coral Sea")
  (base-module "ww2-div-pac")
  (variants
   (see-all false)
   )
  )

(set synthesis-methods nil)

(set initial-date "1 May 1942")

(scorekeeper (do last-side-wins))

;; Real goal is Port Moresby...

(add u* point-value 0)
(add port point-value 10)
(add cv point-value 2)

(world 800)

(area 130 90)

(area (terrain
  "130a"
  "130a"
  "18a2c110a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "9a2c119a"
  "9a3c118a"
  "130a"
  "130a"
  "3ac34ac91a"
  "3a2c125a"
  "60a3b67a"
  "58aca3b67a"
  "56a2b72a"
  "56a2b72a"
  "56a2b72a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "69a5b22ac33a"
  "69a5b22a2c32a"
  "67a5b2a4b52a"
  "67a5bca4b52a"
  "67a5bac4b52a"
  "7a4d58a5b56a"
  "7a5d2a2d53a5b56a"
  "7a6d33ad83a"
  "8a6d4a6d106a"
  "9a6d3a10d14ad6ad70a2b8a"
  "10a5d2a14d20a2d67a3b7a"
  "3a3d8a6d6f8d19a2d75a"
  "3a4d8a8d6f7d19ad74a"
  "19a8d4f7d15a2dad73a"
  "22a7d3f9d13a2dad72a"
  "23ab8d4f7d2a4b5a2d74a"
  "23a3b7d5f7dabda7d7ad66a"
  "23a4ba6d4f8d2ab2a4d9a2d64a"
  "23a6b7d7f6d16a2d51ac11a"
  "23a6b8d7f4d18a2dad48a2c10a"
  "20ad6ab19dfd21a2da2d43a2c10a"
  "20a3d4ab19d2f2d24a2dad40ac10a"
  "9a5bda4b9a4b9d3b4d3f18ada2d5ad50a"
  "9a5b2d4b9a5ba6d6b4d3f20ad2a2dad49a"
  "9a3b2a2b18a6b9a5dfd6ad60a2c5a"
  "9a3b2a2b18a6b9a2b6d3ad21a2d40ac5a"
  "40abd3b9a6d2ad67a"
  "40a5b13a4d68a"
  "16ad23a3bda4b14ad16ac49a"
  "16a2d2a3d17a3b2d4b81a"
  "16adcda3d4a2d11a3b2d4b81a"
  "18a12d3ab6a3b3d6b78a"
  "18ac13d2b6a3b4d5b78a"
  "18ac2d3c8d2b9ab4da7b74a"
  "18ac2d4c8db9a2b5d6b74a"
  "18a3d5c8d4a7bd2c3da6b72a"
  "19ad7c7d4a7b2d2c3d6b72a"
  "20a9c6d11bd4c2da6b44ac25a"
  "21a10c5d10bd5cda6b70a"
  "22a10c5dc7bc2d4c2da5b43ad26a"
  "23a11c4d7bcd6cda5b43a2d25a"
  "24a12c3d6b9cda4b44a2d2ac21a"
  "25a18c4b8cdac7b65a"
  "26a17c4b9cda7b42acac20a"
  "27a30cd2a5b65a"
  "28a30cdb2a3b45ac19a"
  "29a33c2a5b61a"
  "30a33c3a3b43ac17a"
  "31a34c2a7b56a"
  "32a35c2a5b28ac5ac5ac15a"
  "33a35c2a6b27a2c5ac19a"
  "34a35c2a5b29a2c23a"
  "35a36ca6b29a2c21a"
  "36a36ca5b52a"
  "37a37c4b52a"
  "38a38c7b47a"
  "39a38c6b47a"
  "40a38cba3b47a"
  "41a39c2ab47a"
  "42a39c2a4b43a"
  "43a39ca4b43a"
  "44a39c2b45a"
  "130a"
))

(area (features (
   (1 "continent" "Australia")
   (2 "island" "Bougainville")
   (5 "island" "Choiseul")
   (6 "island" "Christmas")
   (7 "island" "Viti Levu")
   (8 "island" "Vanua Levu")
   (9 "island" "Guadalcanal")
   (10 "island" "New Caledonia")
   (11 "island" "New Guinea")
   (12 "island" "Samoa")
   (13 "island" "Santa Catalina")
   (23 "island" "New Britain")
   (24 "island" "New Ireland")
   (25 "island" "Santa Isabel")
   (26 "island" "Malaita")
   (27 "island" "San Cristobal")
   (28 "island" "New Georgia")
   (29 "island" "Espiritu Santo")
   (30 "island" "Efate")
   (31 "island" "Ponape")
   (32 "island" "Malekula")
   (33 "island" "Trobriand")
   (34 "island" "Fergusson")
   (35 "island" "Tagula")
   (35 "island" "Woodlark")
   (36 "island" "Rennell")
  )
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "53ay76a"
  "130a"
  "130a"
  "130a"
  "130a"
  "52ax77a"
  "38al26ac64a"
  "130a"
  "130a"
  "70af59a"
  "130a"
  "130a"
  "77aaj51a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "53ab76a"
  "104ak25a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
  "130a"
))

(side 1 (name "Allies") (adjective "Allied") (emblem-name "flag-usa")
   (class "allied") (names-locked true))

(side 2 (name "Japan") (adjective "Japanese") (emblem-name "flag-japan")
   (class "japanese") (names-locked true))

(a-inf 50 38 1)
(dd 106 10 1)
(ca 63 15 1 (n "Kent"))
(ca 107 9 1 (n "New Orleans"))
(ca 63 15 1 (n "Northampton"))
(ca 107 9 1 (n "Portland"))
(cv 108 9 1 (n "Lexington"))
(cv 108 9 1 (n "Yorktown"))
(airfield 41 36 1 (n "Thursday Island"))
(airfield 51 37 1)
(port 55 20 1 (n "Cairns"))
(port 51 37 1 (n "Port Moresby"))
(town 107 10 1 (n "Noumea"))
(town 63 14 1 (n "Townsville"))

(j-inf 53 49 2)
(aa 52 49 2)
(dd 39 75 2)
(cl 54 50 2)
(cl 54 50 2)
(ca 52 50 2 (n "Aoba"))
(ca 52 50 2 (n "Furutaka"))
(ca 37 75 2)
(cvl 53 50 2 (n "Shoho"))
(cvs 67 42 2 (n "Chitose"))
(cvs 53 50 2)
(cv 39 74 2 (n "Shokaku"))
(cv 38 74 2 (n "Zuikaku"))
(airfield 51 52 2 (n "Kavieng"))
(airfield 47 43 2 (n "Lae"))
(port 38 75 2 (n "Truk"))
(town 54 49 2 (n "Rabaul"))
