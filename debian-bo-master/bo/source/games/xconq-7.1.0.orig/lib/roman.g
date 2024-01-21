(game-module "roman"
  (title "Ancient Rome")
  (blurb "Ancient Rome, ca 100 BC to 200 AD")
  (notes 
   ("Period under construction."
    ""
    "The Roman Republic and Empire from 100 BC to 200 AD."
    ""
    "Massimo Campostrini (campo@sunthpi3.difi.unipi.it)"
    ))
  (variants
   (world-seen false)
   (see-all false)
   (world-size (60 30 360))
   (sequential false)
   ("Mostly Land" mostly-land
    (true
     ;; Adjust so that sea is 20% instead of 50% of the world.
     (add sea alt-percentile-max 20)
     (add shallows alt-percentile-min 20)
     (add shallows alt-percentile-max 21)
     (add swamp alt-percentile-min 21)
     (add swamp alt-percentile-max 23)
     (add (desert plains forest) alt-percentile-min 21)
     ))
   ("All Land" all-land
    (true
;; Adjust sea and shallows out of existence, let swamp take all the low spots.
     (add sea alt-percentile-min 0)
     (add sea alt-percentile-max 0)
     (add shallows alt-percentile-min 0)
     (add shallows alt-percentile-max 0)
     (add swamp alt-percentile-min 0)
     (add swamp alt-percentile-max 2)
     (add swamp wet-percentile-min 0)
     (add swamp wet-percentile-max 100)
     (add (desert plains forest) alt-percentile-min 2)
     ;; Counterproductive to try to set up near water.
     (add sea country-terrain-min 0)
     ))
   ("Large Countries" large
    (true (set country-radius-max 100))
    )
   )
  )


(unit-type infantry (char "i") (image-name "pikeman")
  (help "light infantry")
  (notes "pay attention to wood supply before building")
)
(unit-type cavalry (char "c") (image-name "cavalry")
  (help "light cavalry")
)
(unit-type archer (char "a") (image-name "archer")
  (help "archers and slingers")
  (notes "pay attention to arrow supply")
)
(unit-type legion (char "L") (image-name "legion")
  (possible-sides "roman")
  (help "\"division\" of armored soldiers")
  (notes "pay attention to wood supply before building")
)
(unit-type bireme (char "B") (image-name "bireme")
  (help "small two-decked ship")
)
(unit-type trireme (char "T") (image-name "trireme")
  (possible-sides (not "barbarian"))
  (help "three-decked ship - mainstay of the navies")
)
(unit-type barge (char "U") (image-name "barge")
  (help "troop transport, slow and weak")
)
(unit-type siege-engine (name "siege engine")
  (char "S") (image-name "catapult")
  (possible-sides (not "barbarian"))
  (help "built by infantry for attacking cities")
)
(unit-type camp (char "/") (image-name "tents")
  (help "fortified camp")
)
(unit-type tribe(char "X") (image-name "tribe")
; (possible-sides (or "barbarian" "independent")) ; is this a good idea?
  (help "wandering barbarian tribe")
)
(unit-type oppidum(char "V") (image-name "village-2")
  (help "village")
)
(unit-type civitas (char "*") (image-name "walltown")
  (help "typical city")
)
(unit-type urbs (char "@") (image-name "parthenon")
  (possible-sides (not "barbarian"))
  (help "large city")
)

(material-type food (help "what everybody has to eat"))
(material-type wood (help "raw materials for building things"))
(material-type stones (help "ammo for siege engines"))
(material-type arrows (help "ammo for archers"))

(define i infantry)
(define c cavalry)
(define a archer)
(define L legion)
(define B bireme)
(define T trireme)
(define U barge)
(define S siege-engine)
(define / camp)
(define X tribe)
(define V oppidum)
(define * civitas)
(define @ urbs)

(include "stdterr")
; more land, more forest
;                                sea sha swa des pla for mou ice
(add cell-t* alt-percentile-min (  0  48  50  50  51  51  90  99))
(add cell-t* alt-percentile-max ( 48  50  51  90  90  90  99 100))
(add cell-t* wet-percentile-min (  0   0  20   0  20  60   0   0))
(add cell-t* wet-percentile-max (100 100 100  20  60 100 100 100))

(define cities (V * @))
(define makers (X V * @))
(define places (/ V * @))
(define troops (i c a L S))
(define ships (B T U))
(define movers (i c a L B T U S X))
(define water (sea shallows))
(define land (swamp plains forest desert mountains))

(table road-chance
  (* (* @) ( 1  5))
  (@ (* @) (10 90))
  )

(table road-chance ((* @) (* @) (10 90)))

;;; Static relationships.

(table vanishes-on
  (troops water true)
  (places water true)
  (ships land true)
  (ships road true)
  (u* ice true)
)

;; Unit-unit capacities.

(table unit-size-as-occupant
  (u* u* 500)
;             i c a L B T U  S   X
  (movers u* (1 2 1 4 2 4 6 500 500))
)
(add (B T U S  /  V  X  *   @) capacity 
     (1 2 4 4  8 12 16 25 100)
)
(table occupant-max
  (u* u* 100)
  (S  u* 1)
  (ships ships 0)
)

;;; Unit-terrain capacities.

;; Limit units to 4 in one cell.
;;  Places cover the entire cell, however.
  
(table unit-size-in-terrain
  (u* t* 4)
  (L t* 8)
  (places t* 16)
)
(add t* capacity 16)

;;; Unit-material capacities.

(table unit-storage-x
  (u* m* 0)
;               i c a  L  B  T  U S  /  X  V  *   @ 
  (u* food   (  5 8 5 10 10 20 40 5 10 30 30 50 200))
  (u* wood   ( 16 4 4 24 12 16 25 0 20 20 30 50 150))
  (u* stones (  2 1 1  4  4  6 12 5 10 10 30 50 150))
  (u* arrows (  2 1 4  4  4  6 10 0 10 15 20 30 100))
)

;; A game's starting units will be full by default.
(table unit-initial-supply (u* m* 9999))

; i c a L B T U S / X V * @ 

(add places already-seen 100)
(add X already-seen 100)

(add cities see-always true)

(table visibility
  (u* t* 100)
  (movers (mountains forest) 10)
  ((i a) (swamp plains desert) 30)
  ((S X) (mountains forest) 30)
)
;                    i c a L B T U S /  X  V   * @ 
(add u* stack-order (1 3 2 4 5 7 6 8 9 10 11 12 13))

;;; Actions.

;                      i  c  a  L  B  T  U  S / X V * @ 
(add u* acp-per-turn ( 2  8  2  4  4  4  2  1 1 2 1 2 4))
(add movers acp-min  (-2 -8 -2 -4 -4 -4 -3 -3  -2))

;;; Movement.

(add places speed 0)
(add movers speed 100)
(table mp-to-enter-unit
   (u* u* 0)
   ((i c a L) ships (2 8 2 4))
)

(table mp-to-enter-terrain 
  (u* t* 99)
  (troops land 2)
  (L land 4)
  (c (forest mountains) 8)
  ((S X) land 2)
  (ships water 1)
  (troops road 0)
;                i c a L  S
  (troops river (2 8 2 4 99))
  (X river 2)
)
(table mp-to-traverse
  (u* (river road) 99)
  (ships river 2)
  (troops road 1)
  ((L c S X) road 2)
)

;;; Construction.

;            i  c  a  L  B  T  U  S  / X V * @ 
(add u* cp (12 30 20 40 32 48 24 20 16 1 1 1 1))
(table acp-to-create
  ((i L) (S /) 1)
  (cities troops 1)
  (cities ships 1)
  (X troops 1)
  (X ships 1)
  ((X V) (L T) 0)
)
(table cp-on-creation
  ((i L) (S /) 2)
  (cities troops 2)
  (cities ships 2)
  (X (i B U) 1)
  (V (c B U) 1)
)
(table acp-to-build
  ((i L) (S /) 1)
  (cities troops 1)
  (cities ships 1)
  (X troops 1)
  (X ships 1)
  ((X V) (L T) 0)
)
(table cp-per-build
  ((i L) (S /) 2)
  (cities troops 2)
  (cities ships 2)
  (X (i B U) 1)
  (V (c B U) 1)
)
(table occupant-can-construct
  (u* u* false)
)

(table consumption-on-creation
  ((B T U S /) wood (15 25 15 8 8))
)

;; Automatic repair work.
(add troops hp-recovery 0.25)
(add ships  hp-recovery 0.25)
(add cities hp-recovery 0.50)

;;; Production.

;; should actually get food and wood from terrain
(table base-production
  (troops food 1)
  (c food 2)
  ((/ X V * @) food   (5 5 10 20 40))
  ((/ X V * @) wood   (1 3  2  3  5))
  ((/ X V * @) arrows (1 1  1  2  3)) 
  ((/ X V * @) stones (1 1  1  2  3)) 
)

(table productivity
  (u* t* 0)
  (troops (plains forest) 100)
  (c forest 50)
  (/ land-t* (30 100 50 30))
  (X land-t* (30 100 50 30))
  (V land-t* (30 100 50 30))
  (* land-t* (30 100 50 30))
  (@ land-t* (30 100 50 30))
)

(table base-consumption
  ((i a L) food 1)
  (c food 2)
)
(table hp-per-starve
  (movers food 0.5)
)

;;; Combat.

(table acp-to-attack
  (u* u* 1)
  ((a S) u* 0)
)
(table acp-to-defend
  (u* u* 1)
  (u* a 0)
)
(add (a S) range 1)
(add (a S) acp-to-fire 1)

;               i c a L B T U S /  X  V  *  @ 
(add u* hp-max (2 3 2 8 3 6 2 2 5 20 20 50 200))

(table hit-chance 
   (u* u* 50) ; basic hit chance
   (troops ships 20)
   (ships troops 20)
   (a c 25)
   (S movers 0)
   (U u* 0)
)

(table damage
  (u* u* 1)
  (S (V * @) 4)
)

(table capture-chance
  (i (/ X V * @) (20 15 15 10  5))
  (c (/ X V * @) (20 20 10  5  2))
  (L (/ X V * @) (40 40 40 20 10))
)
(table independent-capture-chance
  (i (/ X V * @) (40 20 30 20 10))
  (c (/ X V * @) (40 30 20 10  5))
  (L (/ X V * @) (70 50 70 35 20))
)

(table ferry-on-entry
   (u* u* over-own)
   (ships movers over-border)
)
(table ferry-on-departure
   (u* u* over-own)
   (ships movers over-border)
)

(table protection 
  (/ movers 80)
  (V movers 65)
  (* movers 50)
  (@ movers 25)
)

(table consumption-per-attack 
  (a arrows 1)
  (S stones 1)
)
(table hit-by
  (u* arrows 1)
  (places stones 1)
  (ships stones 1)
)

(table surrender-range (u* u* 1))
(add u* acp-to-change-side 1)

(add u* point-value 0)
(add makers point-value (1 1 3 9))

(table favored-terrain
  (u* t* 0)
  (u* plains 100)
  (u* forest 20)
  (ships t* 0)
  (ships water 100)
  (X (plains desert forest mountains) (30 20 100 30))
  (V (desert forest mountains) (20 50 30))
  (* (desert forest mountains) (10 30 20))
)
(table independent-density
;                     X   V  *  @
  (makers plains    (20 120 60 15))
  (makers forest    (80  80 30  1))
  (makers mountains (20  40 10  1))
  (makers desert    (10  20  5  1))
)

;; Initial setup.
 
(set country-radius-min 3)
(set country-separation-min 16)
(set country-separation-max 48)

(add makers start-with (1 3 2 1))

;; Naming.

(namer urbs-names (random
  "Alexandria" "Antiochia" "Athenae" "Chartago" "Constantinopolis"
  "Ecbatana" "Roma" "Seleucia"
))
(add urbs namer "urbs-names")

(namer civitas-names (random
  "Apollonia" "Aquileia" "Artaxata" "Berytus" "Bysanthium"
  "Caesarea" "Capua" "Corinthus" "Cyrene" "Durcotorum" "Dyrrachium"
  "Eburacum" "Ephesus" "Gazaca" "Ierusalem" "Leptis Magna" "Lugudunum"
  "Magontiacum" "Massilia" "Mediolanum" "Memphis" "Messana" 
  "Narbo Martius" "Nicaea" "Nicomedia" "Nova Carthago" "Numantia"
  "Palmyra" "Pergamum" "Petra" "Ravenna" "Rhodos" "Sais" "Salamis"
  "Salonae" "Sinope" "Susa" "Syracusae" "Tarentum" "Tarraco" "Thebae"
  "Thessalonica" "Tingis" "Trapezus" "Tyrus" "Utica"
))
(add civitas namer "civitas-names")

(namer oppidum-names (random
  "Aduatuca" "Aelana" "Aleria" "Alesia" "Amisus" "Ammonium" "Ancona"
  "Ancyra" "Aquincum" "Attalea" "Baracara Augusta" "Boreum"
  "Brundisium" "Caralis" "Carana" "Catabathmus Maior" "Cenabum"
  "Cidamus" "Colonia Agrippina" "Damascus" "Delphi" "Dioscurias"
  "Gades" "Gaza" "Gergovia" "Glevum" "Gortyna" "Halicarnassus"
  "Heliopolis" "Heraclea" "Hermopolis Magna" "Hippo Regius" "Hispalis"
  "Iconium" "Ilerda" "Lancia" "Lix" "Londinium" "Luni" "Lutecia"
  "Lysimachia" "Melitene" "Naissus" "Nicopolis" "Nicopolis" "Noreia"
  "Olbia" "Olisipo" "Panormus" "Panticapaeum" "Patavium" "Pelusium"
  "Philippopolis" "Pisae" "Ptolemais Hermiu" "Rhegium" "Rusaddir"
  "Sabrata" "Sala" "Sirmium" "Smyrna" "Sparta" "Tacapae" "Tanais"
  "Tarsus" "Teredon" "Thapsacus" "Thapsus" "Toletum" "Tolosa" "Tomis"
  "Volubilis"
))
(add oppidum namer "oppidum-names")

(namer tribe-names (random
  "Aestii" "Alani" "Amadoci" "Bastarnae" "Batavi" "Burgunti" "Chatti"
  "Chauci" "Cimbri" "Daci" "Frisii" "Getae" "Gothi" "Hiberni"
  "Iazyges" "Langobardi" "Marcomanni" "Massagetae" "Picti" "Roxolani"
  "Rugii" "Sarmati" "Siraces" "Suebi" "Sugambri" "Tauri" "Venedae"
))
(add tribe namer "tribe-names")

;; Sides.

(set sides-min 2)

(scorekeeper (do last-side-wins)) ; for now

; the side library really needs work (what about emblems?)
(set side-library '(
  ((name "Marius")
   (class "roman"))
  ((name "Sulla")
   (class "roman"))
  ((name "Parthia") (long-name "Regnum Parthorum") (adjective "Parthian")
   (class "civilized"))
  ((name "Macedonia") (long-name "Regnum Macedoniae") (adjective "Macedonian")
    (class "civilized"))
  ((name "Carthago") (adjective "Punic")
   (class "civilized"))
  ((name "Aedui") (adjective "Aeduan")
   (class "barbarian"))
  ((name "Teutones") (adjective "Teutonian")
   (class "barbarian"))
  ))
