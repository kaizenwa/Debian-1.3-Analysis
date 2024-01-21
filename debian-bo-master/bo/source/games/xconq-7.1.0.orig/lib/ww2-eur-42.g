(game-module "ww2-eur-42"
  (title "WWII Europe Jan 42")
  (blurb "The situation in Europe, January 1942")
  (base-module "ww2-adv")
  (variants (see-all true) (sequential false))
  )

;; No random syntheses needed (except for unit numbering/naming).

(set synthesis-methods '(name-units-randomly))

(include "eur-100km")

(include "ww2-sides")

(area (people-sides
  "a67X22a"
  "26Xb10X4e7a10XhX9h21a"
  "25Xb11X4e6a7X4eh2X9h20a"
  "25X3b9X3eX7a7X4e12h19a"
  "25X3b10X2e2X6a2Xa2XeX5eh2e9h18a"
  "26X2b13XeX5a5XeX6e12h17a"
  "27X3b11X2eX2a7X12e9h16a"
  "24X2b2X3b11X3e8X12e10h15a"
  "24Xa2b2X4b9X2e2Xe2X2eX14e10h14a"
  "24X3a2X5b5X28e10h13a"
  "24X3a2X6b3X30e10h12a"
  "25Xa5X4b2X33e9h11a"
  "30X2b4X34e10h10a"
  "33XeX35e11h9a"
  "31X41e10h8a"
  "d32X11e3a26eX9h7a"
  "d34X9e3a4f18eXe3X8hXh6a"
  "d2Xd31X9e7fX15e3Xe2hX9hXh5a"
  "dX3d31X9e6f2X14e4Xh3X9hXh4a"
  "5d25X8j8e3X3f3X12e10X8hXh3a"
  "5d25X12j6Xe2X3f3X9ek12X7hXh2a"
  "5d26X2a9j7Xf2X4f2X7e3k3X4k5X7hXha"
  "6d25X2a8j8X2f3X4fX5e2XkX17k3bXh"
  "a6d24X2a7j3Xj6Xf5Xf3X3e5X18k4b"
  "2a5d11Xa13X2a6j17Xf4X3e2XkX17k4b"
  "3a4d10Xa15Xa6j17Xf4X2e2XfX19k3b"
  "4a3Xd27Xb9X5a3f2X3f6Xe5XkXkX2k2Xk3a5k3b"
  "5a37X8a3f3X2f8X2eXf8X5a7b"
  "6a30X3j12a2f22X2b2X3a8b"
  "7a28X16a3f4Xb20X2a9b"
  "8a27X17a3f9X3f12X11b"
  "9a26X17a6f6Xb4f5X2b3X11b"
  "10a19Xj4X19a8f3Xfb4f20b"
  "11a24X18a18f11b5X3b"
  "12a23X19a17f8bXb8Xb"
  "13a18Xj3X19a18f8b10X"
  "14a16Xj3X21a17f9b9X"
  "15a18X22a18f9b8X"
  "16a17X23a17f10b7X"
  "17a16X23a18f9b7X"
  "18a15X24a17f10b6X"
  "19a14X24a18f10b5X"
  "20a13X25a17f11b4X"
  "21a13X24a18f11b3X"
  "22a68X"
))

;; Sides not participating.

(side fr (active false))
(side jp (active false))
(side zh (active false))
(side es (active false))
(side tr (active false))

;; Allies.

(side uk (trusts (uk 1) (fr 1) (us 1) (su 1)))
(side us (trusts (uk 1) (fr 1) (us 1) (su 1)))
(side su (trusts (uk 1) (fr 1) (us 1) (su 1)))

(side fr (trusts (uk 1) (fr 1) (us 1) (su 1)))

;; Axis.

(side de (trusts (de 1) (it 1)))
(side it (trusts (de 1) (it 1)))

(scorekeeper (do last-side-wins))

;; (set up agreements applying to particular dates)

(set initial-date "Jan 1942")

(infantry 33 33 uk)
(infantry 32 33 uk)
(infantry 32 34 uk)
(infantry 65 12 uk)
(interceptor 34 33 uk)
(armor 33 33 uk)
(armor 64 12 uk)
(convoy 34 34 uk)
(fleet 31 40 uk)
(fleet 25 29 uk)
(fleet 57 15 uk)

(infantry  1 22 us)
(convoy 4 25 us)

(infantry 37 32 de)
(infantry 37 33 de)
(infantry 31 30 de)
(infantry 72 29 de)
(infantry 71 29 de)
(infantry 71 27 de)
(infantry 68 32 de)
(infantry 67 34 de)
(infantry 65 36 de)
(infantry 64 38 de)
(infantry 62 40 de)
(infantry 61 40 de)
(infantry 60 25 de)
(armor 60 40 de)
(armor 63 39 de)
(armor 70 27 de)
(armor 72 30 de)
(armor 70 30 de)
(armor 69 31 de)
(armor 68 33 de)
(armor 66 35 de)
(armor 65 37 de)
(armor 64 11 de (n "Afrika Korps"))
(infantry 59 40 de)
(infantry 58 41 de)
(infantry 57 42 de)
(sub-fleet 34 29 de)
(sub-fleet 35 31 de)

(infantry 63 11 it)

(infantry 65 39 su)
(infantry 60 41 su)
(infantry 74 30 su)
(infantry 73 31 su)
(infantry 68 35 su)
(infantry 67 36 su)
(infantry 67 37 su)
(infantry 66 38 su)
(infantry 74 29 su)
(infantry 72 27 su)
(infantry 62 41 su)
(infantry 72 31 su)
(infantry 70 31 su)
(infantry 70 32 su)
(infantry 68 34 su)
(infantry 66 37 su)
(infantry 65 38 su)
(infantry 63 40 su)
(infantry 61 41 su)
(infantry 59 41 su)
(armor 64 39 su)
(armor 71 31 su)
(armor 69 33 su)
(armor 66 36 su)
(fleet 70 26 su)

(town 36 13 0 (n "Marrakech"))
(base 79 14 0 (n "Haifa")) ; but British mandate
(base 80 16 0 (n "Damascus"))
(town 80 14 0 (n "Jerusalem")) ; but British mandate
(base 79 18 0 (n "Aleppo"))
(town 87 20 0 (n "Tabriz"))
(town 32 20 0 (n "Lisbon"))
(town 26 35 0 (n "Dublin"))
(town 35 15 0 (n "Casablanca"))
(town 42 17 0 (n "Oran"))
(town 31 23 0 (n "Porto"))
(town 45 18 0 (n "Algiers"))

(base 34 33 uk (n "Dover"))
(base 35 18 uk (n "Gibraltar"))
(base 58 15 uk (n "Malta"))
(base 26 43 uk (n "Scapa Flow"))
(base 34 34 uk (n "Southend"))
(base 77 12 uk (n "Port Said"))
(base 78 11 uk (n "Suez"))
(town 74 13 uk (n "Alexandria") (tp (convoy 36)))
(town 80 6 uk (n "Aswan") (tp (infantry 6)))
(town 78 8 uk (n "Asyut") (tp (interceptor 24)))
(town 25 37 uk (n "Belfast") (tp (interceptor 24)))
(town 31 34 uk (n "Bristol") (tp (asw-fleet 36)))
(town 76 10 uk (n "Cairo") (tp (armor 24)))
(town 30 34 uk (n "Cardiff") (tp (infantry 6)))
(town 28 38 uk (n "Edinburgh") (tp (infantry 6)))
(town 31 36 uk (n "Hull") (tp (convoy 36)))
(town 30 37 uk (n "Newcastle") (tp (bomber 24)))
(city 30 35 uk (n "Birmingham") (tp (infantry 6)))
(city 27 38 uk (n "Glasgow") (tp (fleet 36)))
(city 29 36 uk (n "Manchester") (tp (armor 24)))
(capital 33 34 uk (n "London") (tp (fleet 36)))

(town 5 20 us (n "Atlanta"))
(town 2 26 us (n "Buffalo"))
(town 6 20 us (n "Charleston"))
(town 3 21 us (n "Memphis"))
(town 5 22 us (n "Norfolk"))
(city 4 26 us (n "Boston") (tp (fleet 36)))
(city 4 19 us (n "Houston"))
(city 5 19 us (n "New Orleans") (tp (infantry 1)))
(city 4 25 us (n "New York") (tp (convoy 36)))
(city 1 24 us (n "Chicago") (tp (interceptor 24)))
(city 4 24 us (n "Philadelphia") (tp (fleet 36)))
(city 3 24 us (n "Pittsburgh") (tp (bomber 24)))
(city 2 24 us (n "Detroit") (tp (armor 12)))
(city 2 22 us (n "St. Louis") (tp (infantry 6)))
(capital 4 23 us (n "Washington") (tp (cv-fleet 36)))

(town 64 20 de (n "Athens") (os 0))
(town 58 26 de (n "Belgrade") (os 0))
(town 36 27 de (n "Bordeaux") (os fr))
(town 38 32 de (n "Brussels") (os 0))
(town 64 26 de (n "Bucharest") (tp (infantry 6)))
(town 55 29 de (n "Budapest") (tp (infantry 6)))
(town 54 32 de (n "Cracow") (os 0))
(town 51 35 de (n "Danzig") (tp (convoy 36)))
(town 69 30 de (n "Dnepropetrovsk") (os su))
(town 42 33 de (n "Dortmund") (tp (armor 24)))
(town 41 33 de (n "Essen") (tp (interceptor 24)))
(town 44 32 de (n "Frankfurt") (tp (infantry 6)))
(town 53 36 de (n "Konigsberg") (tp (infantry 6)))
(town 58 32 de (n "L'vov") (os 0))
(town 35 31 de (n "Le Havre") (os fr))
(town 47 32 de (n "Leipzig") (tp (bomber 24)))
(town 41 27 de (n "Lyon") (os fr))
(town 43 25 de (n "Marseilles") (os fr))
(town 60 35 de (n "Minsk") (os su))
(town 47 30 de (n "Munich") (tp (bomber 24)))
(town 34 29 de (n "Nantes") (os fr))
(town 45 25 de (n "Nice") (os fr))
(town 67 28 de (n "Odessa") (os su) (tp (convoy 36)))
(town 40 42 de (n "Oslo") (os 0) (tp (convoy 36)))
(town 49 31 de (n "Prague") (os 0) (tp (armor 24)))
(town 55 38 de (n "Riga") (os 0))
(town 62 24 de (n "Sofia") (os 0))
(town 45 30 de (n "Stuttgart") (tp (infantry 6)))
(town 63 22 de (n "Thessaloniki") (os 0))
(town 39 25 de (n "Toulouse") (os fr))
(town 52 30 de (n "Vienna") (tp (infantry 6)))
(town 58 36 de (n "Vilnius") (os 0))
(town 55 34 de (n "Warsaw") (os 0) (tp (armor 24)))
(town 53 27 de (n "Zagreb") (os 0))
(city 44 37 de (n "Copenhagen") (os 0))
(city 42 35 de (n "Hamburg") (tp (sub-fleet 36)))
(city 65 32 de (n "Kiev") (os su))
(city 38 34 de (n "Rotterdam") (os 0))
(capital 47 34 de (n "Berlin") (tp (interceptor 24)))
(capital 37 30 de (n "Paris") (os fr))

;; should be British-owned in 1/42?
(town 64 13 it (n "Benghazi") (tp (infantry 6)))
(town 46 26 it (n "Genoa") (tp (armor 24)))
(town 55 18 it (n "Palermo") (tp (infantry 6)))
(town 44 27 it (n "Torino") (tp (infantry 6)))
(town 60 12 it (n "Tripoli") (tp (infantry 6)))
(town 52 18 it (n "Tunis"))
(city 47 27 it (n "Milan") (tp (interceptor 24)))
(capital 52 23 it (n "Rome") (tp (infantry 6)))

(town 81 28 su (n "Astrakhan") (tp (infantry 6)))
(town 86 23 su (n "Baku") (tp (infantry 6)))
(town 80 23 su (n "Batum") (tp (infantry 6)))
(town 67 35 su (n "Br'ansk") (tp (interceptor 24)))
(town 68 39 su (n "Jaroslavi") (tp (interceptor 24)))
(town 73 37 su (n "Kazanh") (tp (infantry 6)))
(town 69 32 su (n "Kharkov") (tp (armor 24)))
(town 75 27 su (n "Maikop") (tp (infantry 6)))
(town 71 26 su (n "Sevastopol") (tp (infantry 6)))
(town 77 31 su (n "Stalingrad") (tp (armor 24)))
(town 71 30 su (n "Stalino") (tp (infantry 6)))
(town 83 23 su (n "Tbilisi") (tp (infantry 6)))
(city 71 38 su (n "Gorky") (tp (infantry 6)))
(city 75 35 su (n "Kuybyshev") (tp (interceptor 24)))
(city 58 42 su (n "Leningrad") (tp (infantry 6)))
(city 73 30 su (n "Rostov") (tp (armor 24)))
(capital 67 38 su (n "Moscow") (tp (armor 24)))

(base 35 20 es (n "Cordoba"))
(base 30 8 es (n "Las Palmas"))
(base 36 16 es (n "Tangiers"))
(town 41 23 es (n "Barcelona"))
(town 35 25 es (n "Bilbao"))
(town 39 21 es (n "Valencia"))
(town 38 23 es (n "Zaragoza"))
(capital 36 22 es (n "Madrid"))

(town 72 21 tr (n "Ankara"))
(town 81 21 tr (n "Erzurum"))
(town 68 20 tr (n "Izmir"))
(town 84 21 tr (n "Jerevan"))
(town 76 19 tr (n "Adana"))
(city 67 23 tr (n "Istanbul"))

;(town 88 3 0  (n "Mecca")) ; a distraction to Egyptian forces

