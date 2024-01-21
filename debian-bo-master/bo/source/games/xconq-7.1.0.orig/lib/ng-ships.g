;; Namers for World War II ships.
;; ===== Please report corrections, additions, and improvements to
;; ===== Massimo Campostrini (campo@sunthpi3.difi.unipi.it)
;
;; Namers for carriers, escort carriers, battleships, and cruisers. 
;; do we need namers for smaller units?

(namer japanese-cv-names (random
   "Akagi" "Hiryu" "Hiyo" "Junyo" "Kaga" "Shokaku" "Soryu" "Taiho"
   "Zuikaku"
))

; actually the following "cve" weren't so small!
; they were smaller then previous "cv", anyway.
(namer japanese-cve-names (random
   "Amagi" "Aso" "Chitose" "Chiyoda" "Haiyo" "Kasagi" "Katsuragi" "Ryujo"
   "Shinyo" "Shoho" "Taiyo" "Unryu" "Zuiho"
))

(namer japanese-bb-names (random
   "Fuso" "Haruna" "Hiei" "Hyuga" "Ise" "Kongo" "Musashi" "Nagato"
   "Shinano" "Yamashiro" "Yamato"
))

(namer japanese-ca-names (random
   "Abukuma" "Ashigara" "Atago" "Chikuma" "Chokai" "Haguro" "Isuzu"
   "Kinugasa" "Kumano" "Maya" "Mikuma" "Mogami" "Myoko" "Nachi" "Nagara"
   "Noshiro" "Oyoda" "Suzuya" "Takao" "Tama" "Tone" "Yahagi"
))

(namer us-cv-names (random
   "Bunker Hill" "Cabot" "Enterprise" "Essex" "Franklin" "Hancock" "Hornet"
   "Intrepid" "Lexington" "Midway" "Saratoga" "Wasp" "Yorktown"
))

(namer us-cve-names (random
   "Bataan" "Belleau Wood" "Bismarck Sea" "Block Island" "Bogne" "Cabot"
   "Card" "Chenago" "Core" "Cowpens" "Fanshaw Bay" "Franks" "Gambier Bay"
   "Guadalcanal" "Haggard" "Heerman" "Hoel" "Independence" "Kadashan Bay"
   "Kalinin Bay" "Kitkun Bay" "Langley" "Manila Bay" "Marcus Island"
   "Mc. Cord" "Monterey" "Mormackland" "Onmaney Bay" "Petrof Bay"
   "Princeton" "Saginaw Bay" "San Jacinto" "Sangamon" "Santee"
   "Savo Island" "St. Lo" "Suwanee" "Trathen" "White Plains"
))

(namer us-bb-names (random
   "Alabama" "California" "Indiana" "Iowa" "Maryland" "Massachusetts"
   "Mississippi" "Missouri" "New Jersey" "New York" "North Carolina" 
   "Pennsylvania" "South Dakota" "Tennessee" "Texas" "Washington" 
   "Wisconsin" "West Virginia"
))

;; (This mixes up ca and cl names...)
(namer us-ca-names (random
   "Baltimore" "Biloxi" "Birmingham" "Boise" "Boston" "Canberra"
   "Chattanooga" "Chester" "Cleveland" "Columbia" "Denver" "Des Moines"
   "Galveston" "Houston" "Indianapolis" "Louisville" "Miami" "Minneapolis"
   "Mobile" "Montpelier" "New Orleans" "Northampton" "Oakland" "Pensacola"
   "Phoenix" "Portland" "Reno" "Salem" "Salt Lake City" "San Diego" 
   "San Francisco" "San Juan" "Santa Fe" "Tacoma" "Vincennes" "Wichita"  
))

(namer us-cl-names (random
  "Denver" "Chattanooga" "Cleveland" "Des Moines" "Galveston" "Tacoma"
  "Atlanta" "Juneau" "San Diego" "San Juan"
  "Chester" "Birmingham" "Salem"
))

(namer british-cv-names (random
   ""Audacious" "Ark Royal" "Corageous" "Formidable" "Gibraltar" "Glorious"
   "Illustrious" "Implacable" "Indefatigable" "Indomitable"
   "Victorious"
))

(namer british-cve-names (random
   "Archer" "Argus" "Audacity" "Avenger" "Biter" "Casablanca" "Colossus"
   "Commencment Day" "Dasher" "Eagle" "Emperor" "Fencer" "Furious" "Hermes"
   "Long Island" "Majestic" "Pursuer" "Searcher" "Venerable" "Vengeance"
))

(namer british-bb-names (random
   "Anson" "Colossus" "Duke of York" "Howe" "King George V" "Lion" "Nelson"
   "Prince of Wales" "Queen Elizabeth" "Ramillies" "Revenge" "Rodney"
   "Temeraire" "Thunderer" Valiant" "Vanguard" "Warspite" 
))

; (This doesn't distinguish ca and cl names)
(namer british-ca-names (random
   "Bristol" "Cairo" "Ceres" "Cardiff" "Chatham" "Cornwall" "Coventry"
   "Curacoa" "Curlew" "Dorsetshire" "Dublin" "Glasgow" "Gloucester" "Hood"
   "Kenya" "Liverpool" "Manchester" "Newcastle" "Nigeria" "Renown"
   "Repulse" "Sheffield" "Shropshire" "Southampton"  
))

(namer british-cl-names (random
  "Bristol" "Glasgow" "Gloucester" "Liverpool" "Newcastle"
  "Ceres" "Cardiff" "Coventry" "Curacoa" "Curlew"
  "Chatham" "Dublin"
))

(namer german-cv-names (random
   "Graf Zeppelin"
))

(namer german-bb-names (random
   "Bismarck" "Schleisich" "Schleswig-Holstein" "Tirpitz"
   ;; actually battle cruisers & pocket battleships
   "Admiral Graf Spee" "Admiral Scheer" "Deutschland" "Gneisenau" "Lutzow"
   "Scharnhost"
))

(namer german-ca-names (random
   "Admiral Hipper" "Blucher" "Emden" "Karlsruhe" "Koln" "Konigsberg"
   "Nurnberg" "Prinz Eugen Seydlitz"
))

(namer italian-bb-names (random
   "Littorio" "Roma" "Vittorio Veneto"
))

(namer italian-ca-names (random
   "Andrea Doria" "Bolzano" "Caio Duilio" "Conte di Cavour" "Fiume"
   "Giulio Cesare" "Gorizia" "Pola" "Trento" "Trieste" "Zara"
))   

(namer french-bb-names (random
   "Dunkerque" "Jean Bart" "Richelieu"
))
