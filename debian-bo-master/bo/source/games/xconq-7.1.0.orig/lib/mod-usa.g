(game-module "mod-usa"
  (title "Modern Times - USA")
  (base-module "modern")
  (variants (see-all true))
  )

(area 150 60 (restrict 800 320 0 210))

(unit-defaults (hp 0))

(include "earth-50km")

(area (restrict reset))

(unit-defaults reset)

(set sides-min 1)
(set sides-max 1)

(side 1 (name "USA"))

(unit-defaults (s 1) (hp 1))

(unit "Albuquerque")
(unit "Atlanta")
(unit "Birmingham")
(unit "Boston")
(unit "Buffalo")
(unit "Chicago")
(unit "Cincinnati")
(unit "Cleveland")
(unit "Dallas")
(unit "Denver")
(unit "Detroit")
(unit "El Paso")
(unit "Ft Worth")
(unit "Houston")
(unit "Indianapolis")
(unit "Jacksonville")
(unit "Kansas City")
(unit "Los Angeles")
(unit "Miami")
(unit "Milwaukee")
(unit "Minneapolis-St. Paul")
(unit "Nashville")
(unit "New Orleans")
(unit "New York")
(unit "Norfolk")
(unit "Oklahoma City")
(unit "Omaha")
(unit "Philadelphia")
(unit "Phoenix")
(unit "Pittsburgh")
(unit "Portland")
(unit "Rochester")
(unit "Sacramento")
(unit "Salt Lake City")
(unit "San Antonio")
(unit "San Diego")
(unit "San Francisco")
(unit "St. Louis")
(unit "Seattle")
(unit "Tampa")
(unit "Tucson")
(unit "Tulsa")
(unit "Washington DC")
(unit "Wichita")

(unit-defaults reset)

(oil-platform 28 22 1)
(oil-field 32 21 1)
(oil-field 28 24 1)

(area (aux-terrain road
  "150a"
  "31ai118a"
  "31af118a"
  "32aD117a"
  "32aj117a"
  "32aj117a"
  "32aj117a"
  "32aj117a"
  "9aksu6ak7su5aj19ak6su90a"
  "9aj2aB5sr8a>3suj14ak4sr7a>2su13ae13ae58a"
  "9aj3a@18aC14sr16a>2su10aD13a@36ai20a"
  "9aj4a>u17a@34a>su7aj14a@34akr20a"
  "9aj6a@17aD36a>su4af15a@33aj21a"
  "9ah7sNu15af39a>u3aD8ae6a@32aj21a"
  "10a@8a>su13aD40a>uaf9aD6a@31af21a"
  "11aD10a>su10af42a>uD8af7a@19ae11aD20a"
  "11af13a>7su2a@43aGsu7aD7a@19aD9akr20a"
  "12a@21a>uaD42af2a>su4af8a@16aeaf9aj21a"
  "13aD22a>v43a<4a>su2aD3ag2swsRy15aDaD8aj21a"
  "13af13aku9a@50aFuf4aD2aFsQ11ak3sxsx8s~21a"
  "14a@9ak2sra>u8a@49afa>P3al2sx2sT9akr4aDaD4ak2sr<20a"
  "15a@7akr6a>u7aD49aF2sX2aj3aDaf6ak{sr5afadu2akr24a"
  "16aD6aj9a>6sv32ae16af2alRst3sx2sN2swswrf8aHaoXkr25a"
  "16af6aj17aD32a@16a@kra@5aD5a>sRsV7s|NxWM26a"
  "17a@4akr17aj33a@16aGu2a@4aj8a>xu6ajakU27a"
  "18a@3aj18aj34a@14akra>ua@3aj10aD>2s{2st{tr27a"
  "19aDakr18af11ak2sw3su16aD12akr4a>sPakr10aj3afaksr29a"
  "19afkr20aD6ak3sr3aD3a>4su10af11akr8aDj11aj4a@j31a"
  "20aI21al6sr7af9a>3sw5s{N4su4akr9alr10akr5aE31a"
  "20aj@20aj15a@13aD3akr6a>3s;u9aj9aksr6af31a"
  "20aba@19aj16aD12af2akr8ak2srD>8st9sr9aH30a"
  "23a@17akr16aj13aFsr7aksr3af29af>sq27a"
  "24a@15akr17aj13af7aksr6a@29aD29a"
  "25a@13akr17akr14aD4aksr9aD4ak4su18aj29a"
  "26a>u8ak2sr15ak2sr15af2aksr11af2aksr5a@16akr29a"
  "28a@4ak2st5sw12s|19sRsr14a@kr8a@14akr30a"
  "29a@3aj9aD11af20a@16aA10a@12akr31a"
  "30a>wstsu7aj12aD20a@16aD10a@10akz32a"
  "32a>u2a>2su3af12aj21a@15af11a@8akrj32a"
  "34a>u4a>suaD11aj22a@15aD11a@6akraj32a"
  "36a@6a>v11af23a@14aj12aJ5sr2aj32a"
  "37aD7a@11a@23a@13aj9ak2sr@7aj32a"
  "37ad8sNsu9a@23a@12aj7aksr4a>w5sz32a"
  "49a@ak7sP23a@11aj5aksr8a>su2af32a"
  "50a>r8a>4su18aD10af5aj5ak7sR2sP31a"
  "66a>6su10af3ak7sR{3st5sr8a@2a@30a"
  "74a>3su6aDksr8a=19a@2aD29a"
  "79a@5afj31a@af29a"
  "80a>5sQ32aDa@28a"
  "87a<31af2aH27a"
  "120aDkr@26a"
  "120ahr2aD25a"
  "121a@2af25a"
  "122a>2sP24a"
  "126a<23a"
  "150a"
  "150a"
  "150a"
  "150a"
  "150a"
))
