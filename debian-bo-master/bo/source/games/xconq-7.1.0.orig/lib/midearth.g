(game-module "midearth"
  (title "Middle Earth")
  (blurb "Tolkien's Middle Earth")
  (version "2.0.0")
  (variants 
    (see-all false)
    (world-seen false)
  )
)


;;;  Units

(unit-type hobbit (image-name "hobbit") (possible-sides "good")
    (char "h") (help "cheap, good explorers")
    (notes ("Easily produced, hobbits are good explorers"
	    "but not very good fighters.")))
(unit-type dwarf (image-name "dwarf") (possible-sides (or "good" "evil"))
    (char "d") (help "dig and run mines")
    (notes ("Dwarves are irreplaceable in digging,"
	    "running and conquering mines.")))
(unit-type elf (image-name "elf") (possible-sides "good")
    (char "e") (help "fast explorers, swim rivers")
    (notes ("Good and fast explorers, elves can swim across rivers.")))
(unit-type orc (image-name "orc") (possible-sides "evil")
    (char "o") (help "cheap, cannon fodder")
    (notes "Orcs are easily produced, but get slaughtered in battle."))
(unit-type man (image-name "swordman") (possible-sides (or "good" "evil"))
    (char "m") (help "the backbone of an army")
    (notes ("Men are the solid core of armies.")))
(unit-type rider (image-name "cavalry") (possible-sides (or "good" "evil"))
    (char "R") (help "fast, can explore and fight")
    (notes ("The fast-moving riders can explore and fight"
	    "(best used for shock-effect).")))
(unit-type wizard (image-name "sorceror") (possible-sides "good")
    (char "W")
  )

(unit-type ship (image-name "frigate") (possible-sides (or "good" "evil"))
    (char "S") (help "carry troops on sea")
    (notes ("Ships can quickly carry land forces across water and along"
	    "rivers, but are no good for fighting (except by themselves).")))

(unit-type nazgul (image-name "nazgul") (possible-sides "evil")
    (char "N") (help "fly, great fighters")
    (notes ("Ringwraiths or Nazguls are evil creatures moving very quickly"
	    "upon winged steeds.  They are superb fighters,"
	    "but are vulnerable to hobbits!")))
(unit-type troll (image-name "troll") (possible-sides "monsters")
    (char "T") (help "hide in forest and mountains")
    (notes "Trolls lurk in forests and mountains,"
	   "and are best left to themselves."))
(unit-type dragon (image-name "dragon-3") (possible-sides "monsters")
    (char "D") (help "the nastiest beast, avoid it!")
    (notes ("Dragons fly and can destroy cities."
	    "Leave them alones if you can")))

(unit-type mine (image-name "mine-2") (possible-sides (or "good" "evil"))
    (char "M") (help "make your gold")
    (notes ("Gold is obtained in quantity only in mines."
	    "A lot of gold is needed to produce nazguls."
	    "Therefore try to build or capture mines.")))
(unit-type village (image-name "walltown") (possible-sides (or "good" "evil"))
    (char "*") (help "small production center")
    (notes ("Villages are small population centers;"
	    "they can produce everything.")))
(unit-type town (image-name "castle") (possible-sides (or "good" "evil"))
    (char "@") (help "large production center")
    (notes ("Towns are larger population centers;"
	    "they produce faster then villages.")))
(unit-type ruins (image-name "ruins") (possible-sides (not "monsters"))
    (char "r") (help "hiding place"))
(unit-type the-ring (image-name "ring")
    (char "O"))

(define h hobbit)
(define d dwarf)
(define e elf)
(define o orc)
(define m man)
(define R rider)
(define W wizard)
(define S ship)
(define N nazgul)
(define T troll)
(define D dragon)
(define M mine)
(define * village)
(define @ town)
(define r ruins)

(add (* @) wrecked-type ruins)
(add (T D N M * @) point-value (1 1 1 10 10 20))
(add the-ring point-value 1000)

;;; Materials

(material-type food (help "everybody needs it"))
(material-type gold (help "everybody wants it"))


;;; Terrains

(terrain-type sea (color "deep sky blue") (image-name "sea") (char ".")
	      (help "sea or lake"))
(terrain-type swamp (color "medium aquamarine") (image-name "swamp")
	      (char "="))
(terrain-type wasteland (color "yellow") (image-name "desert") (char "~"))
(terrain-type plains (color "green") (image-name "plains") (char "+"))
(terrain-type forest (color "forest green") (image-name "forest") (char "%"))
(terrain-type mountains (color "sienna") (image-name "mountains") (char "^") )
(terrain-type ice (color "azure") (image-name "ice") (char "_")
	      (help "any inaccessible terrain"))

(terrain-type road (color "gray") (char ">")
	      (subtype connection) (subtype-x road-x)
	      (help "road or bridge"))
(terrain-type river (color "blue") (char "<")
	      (subtype border) (subtype-x river-x))
(terrain-type ford (color "sandy brown") (char "|")
	      (subtype border))

(set edge-terrain ice)

(define land-t* (wasteland plains forest mountains))
(define cell-t* (sea swamp wasteland plains forest mountains ice))
(define land-forces (h d e o m R W))
(define monst (T D))
(define movers (h d e o m R W S N T D))
(define places (M * @ r))
(define makers (* @))

; lots of forests in this period:
;                                sea swa pla for des mou ice
(add cell-t* alt-percentile-min (  0  40  40  40  40  90  98))
(add cell-t* alt-percentile-max ( 40  90  90  90  90  98 100))
(add cell-t* wet-percentile-min (  0  97  12  47   0   0   0))
(add cell-t* wet-percentile-max (100 100  47  97  12 100 100))

;;; River generation.

(add t* elevation-min 0)
(add t* elevation-max 2000)
(add (sea swamp) elevation-min 0)
(add (sea swamp) elevation-max (0 10))
(add mountains elevation-min 2000)
(add mountains elevation-max 9000)
(add ice elevation-min 9000)
(add ice elevation-max 12000)
;; The elevations above are consistent with 100-km-across cells. */
(area (cell-width 100000))

; Rivers are most likely to start in the mountains or forests.
(add (plains forest mountains ice) river-chance (5.00 8.00 8.00 50.00))
; Rivers empty into lakes if they don't reach the sea.
(set river-sink-terrain sea)

;;; Road generation.

; (Ice isn't really a liquid, but this keeps rivers out of icefields.)
(add (sea ice) liquid true)

(table road-into-chance
  (land-t* land-t* 100)
  ; No roads across ice fields.
  (land-t* ice 0)
  ; Try to get a road back out into the plains.
  (cell-t* plains 100)
  ; Be reluctant to run through hostile terrain.
  (plains (wasteland forest mountains) (10 25 15))
  )

(table road-chance 
   (* @ 5)
   (@ * 10)
   (@ @ 60)
)

;;; unit-unit & unit-terrain interactions

;(add u* stack-order (1 4 2 3 5 6 10 7 8 9 11 13 14 12))

(table vanishes-on
  (u* (sea ice) true)
  (S t* true)
  ((D N S) sea false)
)

(table unit-size-as-occupant
  (u* u* 100) ; disables occupancy usually
  (land-forces places 2)
  (land-forces S 2)
  (R places 4)
  (R (r S) (2 4))
  (N places 2)
  (S places 5)
  (u* M 100)
  ((h o d m) M (5 3 2 3))
  )

(add places capacity (10 10 20 4))
(add ship capacity 6)

(table unit-capacity-x
  (u* the-ring 1)
  (the-ring the-ring 0)
  )

;;; Unit-terrain capacities.

; Limit units to 4 in one cell.  Places cover the entire cell, however. 
(table unit-size-in-terrain
  (u* t* 1)
  (places t* 4)
  )
(add t* capacity 4)
(add road capacity 2)

;;; Unit-material capacities.

(table unit-storage-x
;            h   d  e   o   m   R  W   S  N  T   D    M   *    @ r O
  (u* food ( 6  12  8  10  12  18  6 100 12 12  20    0 200  500 0 0))
  (u* gold (50 200 50 100 100 100  0 200 50  0 900 5000 800 2000 0 0))
)

;;; Initial set-up

;  bad placement of monsters, but better then nothing
(add (@ T D) start-with (1 3 1)) 

(table independent-density
;                     M  *  @  r
  (places wasteland (15 10  0 30))
  (places plains    ( 5 80 50 50))
  (places forest    (15 20  0 30))
  (places mountains (30 20  0 30))
)

(table favored-terrain
  (u* t* 0)
  (M (plains forest mountains wasteland) (20 20 100 20))
  (* (plains forest mountains) (100 40 20))
  (@ (plains forest) (100 20))
  (r (plains forest mountains) (100 40 20))
  (T (forest mountains) 100)
  (D (plains forest mountains wasteland) (20 40 40 100))
)

(set country-radius-min 7)
(set country-separation-min 20)

(add (* @ r) see-always true)

;;; Repair.

(add (m R S N D T) hp-recovery 0.25)

(table acp-to-repair
  ((* @) u* 1)
)

(table hp-per-repair
  ((* @) u* 1)
)

(table acp-to-attack
  (movers u* 2)
)

;;; Production.

(table base-production
  (land-forces food 2)
  (rider food 3)
  (monst food 3)
  (makers food (10 20))
  (makers gold (1 2))
  (mine gold 20)
  )

(table productivity
  (u* t* 0)
  (u* plains 100)
  (u* forest 50)
  (monst t* 100)
  (monst sea 0)
  (makers (wasteland mountains) 25)
  (mine t* 100)
)
; (table productivity-max (u* m* 999)) ; hack
; (table productivity-min (u* m*   0)) ; hack

(table base-consumption
  (u* food 1)
  ((R T D N *) food 2)
  (@ food 4)
  ((r M) food 0)
)

; unrealistic, but otherwise mining is impossible:
(table consumption-as-occupant (u* food 0))

(table unit-initial-supply (u* food 999))

(table hp-per-starve
  (u* food 0.50)
  ((d D) food 0.25)
)
;                         h  d  e  o  m  R  W  S  N  T  D
(add movers acp-per-turn (2  2  4  2  2  4  4  4  9  2  5))
(add movers acp-min     (-2 -2 -4 -2 -2 -4 -4 -4 -9 -2 -5))

(add makers acp-per-turn (1 2))

(add makers speed 0)

(table mp-to-enter-unit
  (u* u* 0)
)
(table mp-to-enter-terrain
  (u* t* 99)
  (land-forces land-t* 2)
  (land-forces mountains 4)
  (land-forces ford 2)
  (T land-t* 4)
  (T (forest mountains ford) 2)
  (h swamp 2)
  (e (swamp mountains river ford) (4 4 8 1))
  (R (plains wasteland forest mountains ford) (1 2 4 4 4))
  (land-forces road 0)
  (S sea 1)
  ((N D) t* 1)
  ((N D) (river ford road) 0)
  ((N D) ice 99)
)

(table mp-to-traverse
  (movers (ford river) 99)
  (S river 2)
  ((N D) river 0)
  ((N D) ford 0)
  (movers road 1)
  (S road 99)
)

(table can-enter-independent
  (movers ruins true)
  )

;;; Construction parameters.

(add ( h  d  o  e  m  R  S  N  M) cp
     (16 24 16 24 24 40 40 50 80))

(table acp-to-create
  (d M 1)
  ((* @) movers 1)
  (u* W 0)
  )
(table acp-to-build
  (d M 1)
  ((* @) movers 1)
  (u* W 0)
  )
(table cp-per-build
  (d M 1)
  ((* @) movers 1)
  (u* W 0)
  )
(table cp-on-creation
  ((* @) movers 1)
  (d M 1)
  (u* W 0)
)

(table consumption-on-creation
  ((R S N) gold (20 100 500))
)

;;; Combat parameters

;               h d e o m R S N W T D  M  *  @ r O
(add u* hp-max (1 1 1 1 2 2 3 4 4 4 4 20 20 40 1 1))

(table hit-chance
;             h  d  e  o  m  R  W  S  N  T  D 
  (h movers (30 20 20 40 20 20  0  0 50 20 10))
  (d movers (50 50 40 60 40 40  0 10 10 30 15))
  (e movers (50 40 30 70 40 30  0 10 10 40 30))
  (o movers (30 20 10 40 30 20  0 10 10 20 10))
  (m movers (50 40 40 40 50 50  0 15 20 25 25))
  (R movers (50 40 30 40 50 60  0 20 30 50 25))
  (W movers (50 40 30 90 50 60  0 20 70 50 25))
  (N movers (70 90 60 90 80 70  0 50 50 60 60))
  (T movers (30 50 20 60 50 50  0 30  0 50 30))
  (D movers (40 50 30 70 50 50  0 50 20 40 50))
  (S movers 0)
  (S S 40)
  (u* places 100)
  (places u* 0)
  (D places 50)
)

(table defend-terrain-effect 
  (u* t* 100)
  (e forest 50)
  (d mountains 50)
  (h swamp 200)
  (T (plains wasteland) 200)
)

(table attack-terrain-effect 
  (h swamp 0)
  (e swamp 50)
  (T (plains wasteland) 25)
)

(table damage
  (u* u* 1)
  (u* places 0)
  ((T N D) u* 2)
  ((T N D) r 0)
  (h N 2)
  (D (M * @) 5)
)

(table capture-chance
  (h (M * @) (20 30 20))
  (d (M * @) (50 30 20))
  (e (M * @) ( 0 50 35))
  (o (M * @) (30 20 15))
  (m (M * @) (40 50 35))
  (R (M * @) ( 0 30 20))
  ((h d e o m R S) r 80)
  ;; Anyone can take possession of the ring.
  (movers the-ring 100)
  )
(table acp-to-capture
  ((h d e o m R) (M * @ r) 1)
  (S r 1)
  )

; try these for now.  Actually SEEING a N should be enough to flee...
(table withdraw-chance-per-attack
  (u* orc 50)
  (N u* 75)
  (N h 25)
  (N (N e) 0)
  )

; (table can-enter-independent (movers r true)) ; not working

;;; Vision

(add u* see-occupants false)

(table visibility
   (u* t* 100)
   (M t* 10)
   (movers mountains 50)
   (movers forest 25)
;              ~  +  %  ^
   (h land-t* (50 50 25 25))
   (e land-t* (50 50 15 25))
   ((R T D) t* 100)
   (N t* 50)
)

;;; Misc stuff

(table ferry-on-entry
   (u* u* over-own)
   (S movers over-border)
)
(table ferry-on-departure
   (u* u* over-own)
   (S movers over-border)
)

(add u* acp-to-disband 1)
(add places acp-to-disband 0)

(include "town-names")
(add (* @) namer "random-town-names")  ;  for now

(scorekeeper
  (title "")
  (do last-side-wins)
  )

;;; Players and sides

#| monster side broken again :-(
(set sides-min 3)

; monster always played by ai
(player 1 (ai-type-name "mplayer"))
(side 1 (name "monsters") (noun "") (plural-noun "") (class "monsters")
        (color "red") (emblem-name "none") (player 1))
|#

(set side-library '(

  (99 (name "Mordor") (noun "Sauron's slave") (adjective "Mordorian")
      (color "black,red") (class "evil") (emblem-name "evil-eye"))
  (99 (name "The Shire") (plural-noun "Shire folk") (adjective "Shire's")
      (color "gray,black") (class "good") (emblem-name "feather"))
  (99 (noun "Dunadan") (adjective "Dunadan") (plural-noun "Dunedain") ;
      (class "good") (emblem-name "seven-stars"))

  (9 (noun "Corsair") (adjective "corsair") (class "evil")
     (emblem-name "pirate"))
  (9 (name "Gondor") (adjective "Gondorian") (class "good")
     (emblem-name "tree-and-crown"))

  ((name "Numenor") (adjective "Numenorean") (class "good")
   (emblem-name "star"))
  ((name "Rhovanion") (adjective "Rhovanian") (class "evil"))	
  ((name "Arnor") (adjective "Arnorian") (class "good"))
  ((name "Rohan") (plural-noun "Rohirrim") (class "good"))
  ((name "Lindon") (adjective "Lindonian") (class "good") (emblem-name "swan"))
  ((noun "Southron") (class "evil"))
  ((noun "Easterling") (class "evil"))
  ((noun "Sindar") (adjective "Sindarin") (class "good") (emblem-name "star"))
  ((name "Bree") (noun "Breelander") (class "good"))
  ((noun "Variag") (class "evil"))		
  ((name "Harad") (noun "Haradrim") (class "evil"))
))

(game-module (notes
  "Period under construction."
  ""
  "This period tries to reproduce Tolkien's Middle Earth."
  "Wizards and RINGS are not implemented (for the moment)."
  ""
  "Human players should not get \"monsters\" sides;"
  "dragons are too powerful."
  ""
  "Fords need work."
  ""
  "Please send comments, suggestions, and fixes to the author:"
  "Massimo Campostrini (campo@sunthpi1.difi.unipi.it)"
  "Dipartimento di Fisica,  Piazza Torricelli 2,"
  "I-56126 Pisa,  Italy"
))

(game-module (instructions ("Welcome to Middle Earth."
              "Explore, capture towns, and stay clear of dragons.")))
