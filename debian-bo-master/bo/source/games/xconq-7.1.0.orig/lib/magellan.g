(game-module "magellan"
  (title "Magellan")
  (blurb "Try to circumnavigate the world and bring back treasure")
  (base-module "voyages")
  (instructions (
  "Take your fleet and crew and try to sail around the world."
  ""
  "Be careful not to get lost or wreck your ships,"
  "don't run out of food and water, and look for treasure."
  ))
  )

;;; Modify the basic game to increase the amount of what's already known.

(add city initial-seen-radius 10)

(add fleet image-name "caravel-fleet-spain")

;;; Use the big map of the world.

(include "earth-1deg")

;;; One specific side only.

(set sides-max 1)

(side 1 (name "Spain") (noun "Spaniard") (adjective "Spanish")
  (emblem-name "flag-spain-old")
  (names-locked true)
  )

;;; Suppress creation of any other units.

(add u* start-with 0)

;;; Magellan starts out near Madrid.

(explorer 125 105 1 (n "Magellan")
  (x (remark "Our hero"))
  )

(crew 123 103 1)

(fleet 124 102 1)   ; should start in Cadiz perhaps

;;; Set up some European cities of Magellan's time.

(city 124 103 1 (n "Cadiz"))
(city 126 106 1 (n "Madrid"))
(city 122 104 0 (n "Lisbon"))
(city 125 104 1 (n "Cordoba"))
(city 128 107 1 (n "Zaragoza"))
(city 131 107 1 (n "Barcelona"))

(side 1 (terrain-view
  "360a"
  "360a"
  "360a"
  "130a3b227a"
  "120a15b2a4b219a"
  "102ab16a23b218a"
  "99a12b4a2ba25b217a"
  "97a49b214a"
  "93a54b213a"
  "91a57b212a"
  "90a59b211a"
  "90a60b210a"
  "90a61b209a"
  "90a62b208a"
  "76a2b12a63b207a"
  "76a4b10a64b206a"
  "76a6b9a64b205a"
  "92a65b203a"
  "94a66b200a"
  "95a66b199a"
  "96a66b198a"
  "96a67b197a"
  "93a70b197a"
  "88a76b196a"
  "79a87b194a"
  "72a24b3a73b188a"
  "66a26b6a76b186a"
  "65a18b14a78b185a"
  "64a12b20a73ba6b184a"
  "62a7b26a82b183a"
  "62a7b25a83b183a"
  "62a5b26a84b183a"
  "61a6b22a83b188a"
  "60a6b23a12ba70b188a"
  "62a2b23a12b2a71b188a"
  "86a12b2a72b188a"
  "84a14ba73b188a"
  "83a7b3a3b2a73b189a"
  "81a9b7a74b189a"
  "80a15ba40b5a30b189a"
  "76a59b7a29b189a"
  "76a54b13a28b189a"
  "74a54b16a27b9a5b175a"
  "39a13b22a18b2a34b20a24b8a7b173a"
  "39a21b13a18b4a33b21a24b8a8b171a"
  "38a24b10a18ba37b35a11b8a12b166a"
  "38a29b5a55b37a10b9a13b6a2b156a"
  "36a34b2a55b40a7b10a21b51a2b102a"
  "36a89b43a7b10a23b48a3b101a"
  "34a91b44a7b10a26b42a5b101a"
  "35a90b43a9b10a26b38a8b101a"
  "35a90b43a10b12a4ba19b12a5b19a9b101a"
  "36a88b45a3b2a7b16a18b9a8b17a8b103a"
  "37a87b45a4b2a7b14a19b8a10b16a7b104a"
  "38a12b2a73b45a4b2a7b12a20b7a13b15a3b107a"
  "40a10b2a74b45a4b2a8b10a21b6a14b14a4b106a"
  "43a7b2a74b46a3b4a7b5a25b4a16b14a4b106a"
  "49ab2a41b5a29b46a3b4a7b4a26b3a20b10a4b106a"
  "53a40b4a30b47a3b4a37b2a21b9a3b107a"
  "58a36b2a31b48a2b5a37ba9ba11b10a3b106a"
  "59a70b55a19b5a12ba7b5a9ba2b6a3b106a"
  "60a40b2a28b55a17b8a11ba6b7a13b3a4b105a"
  "59a40b2a29b56a16b11a15b8a13b3a3b105a"
  "59a38b4a30b56a7ba7b12a8ba6b8a18b105a"
  "60a10b2a3ba22b3a32b56a3b3a7b13a14b9a17b105a"
  "61a10b2a2b9a45ba4b61a6b16a13b9a16b105a"
  "62a9b15a43b2a5b59a6b17a13b9a13b107a"
  "68a4b15a42b3a5b8a7b43a6b19a11b10a10b109a"
  "69a3b17a39b5a20b2a2b38a6b21a10b10a10b108a"
  "94a34b8a22b36a7b26a5b10a3ba7b107a"
  "96a31b10a12b3a7b35a7b42a11b16a5b85a"
  "97a29b14a3b10a6b34a7b44a13b10a9b84a"
  "98a28b31a3b32a8b45a14b8a10b83a"
  "99a26b33a3b30a9b45a5ba9b7a12b81a"
  "99a26b33a3b30a8b47a15b6a15b78a"
  "100a25b33a3b29a9b48a6ba8b5a16b77a"
  "101a2b3a18b35a4b27a8b52a4b2a29b75a"
  "108a15b37a4b27a7b53a4b2a29b74a"
  "112a11b38a4b26a6b55a34b74a"
  "114a9b40a4b24a5b58a33b73a"
  "117a5b42a4b23a5b59a34b71a"
  "107ab10a3b44a3b24a5b60a33b70a"
  "118a4b43a3b25a5b62a30b70a"
  "118a4b44a3b25a6b61a26b73a"
  "118a4b45a3b25a7ba4b59a20b74a"
  "117a4b46a4b25a12b59a17b76a"
  "117a4b46a4b25a13b151a"
  "117a4b47a3b26a13b150a"
  "117a3b48a3b26a15b148a"
  "118a3b47a3b26a9ba6b147a"
  "118a3b48a3b25a8b2a7b146a"
  "118a4b47a3b22a10b3a8b145a"
  "118a5b47a3b21a10b3a8b145a"
  "119a5b47a3b20a11b3a10b142a"
  "119a6b47a3b19a11b3a11b141a"
  "118a8b47a3b19a10b3a11b141a"
  "116a12b45a4b19a10b2a11b141a"
  "114a7b2a10b41a4b18a11ba4b148a"
  "114a7b3a22b29a3b17a16b149a"
  "113a6b5a26b26a3b16a16b149a"
  "113a4b8a29b22a3b15a17b149a"
  "114a3b9a2b2a3b8a3b2a14b17a3b14a12ba4b149a"
  "114a3b33a12b16a4b12a7b159a"
  "113a4b37a12b13a4b12a5b160a"
  "113a4b42a12b9a4b11a4b161a"
  "112a5b44a14b2a2b2a4b9a5b161a"
  "108a2b2a4b50a15ba3b9a4b162a"
  "108a8b54a16ba11b162a"
  "109a6b60a22b163a"
  "110a5b66a16b163a"
  "186a4b170a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
  "360a"
))
