(game-module "fantasy"
  (title "Swords and Sorcery")
  (blurb "Fantasy warfare period...")
  (variants
   (see-all false)
   (world-seen false)
   (world-size)
   )
)

; movers & fighters
(unit-type W (name "wizard") (char "W")
  (help "Magician, wielder awesome magic"))
(unit-type i (name "light infantry") (char "i")
  (help "Foot soldiers and mercenaries."))
(unit-type b (name "barbarian") (char "b")
  (help "Rugged, ferocious, lightly armoured warriors."))
(unit-type k (name "heavy infantry") (char "k")
  (help "Foot soldiers in heavy armour"))
(unit-type P (name "paladin") (char "P")
  (help "Holy warrior, extremely tough and well equipped"))
(unit-type R (name "longbowmen") (char "R")
  (help "Archers, scouts, rangers"))
(unit-type H (name "priest") (char "H")
  (help "The Spiritual guides of the land"))
(unit-type s (name "serfs") (char "s")
  (help "Mobs of Peasant serfs and levies"))
(unit-type D (name "dragon") (char "D")
  (help "Dragon, lays waste to the lands"))
(unit-type u (name "undead") (char "u")
  (help "Undead warriors, raised by priests or wizards"))
(unit-type h (name "Horse Cavalry") (char "h")
  (help "Knights that ride horses."))

; magical effects
(unit-type A (name "Armageddon spell") (char "A")
  (help "spell of awesome destruction"))
(unit-type L (name "Lightning spell") (char "L")
  (help "spell good for killing tough things"))
(unit-type F (name "Fireball spell") (char "F")
  (help "spell great for causing lots of damage to soft targets"))
(unit-type M (name "Mass Charm") (char "M")
  (help "spell that charms units over to the caster's side!"))
(unit-type E (name "Wizard Eye") (char "E")
  (help "spell for spying on things"))

; transport
(unit-type G (name "wagon") (char "G")
  (help "Wagons, logistical unit"))
(unit-type C (name "flying carpet") (char "C")
  (help "Magical flying carpet"))
(unit-type cloudkeep (char "K") (image-name "city30")
  (help "wizard's floating keep"))
(unit-type S (name "sailing ship") (char "S")
  (help "ship, for traveling the seas"))
(unit-type g (name "galley") (char "g")
  (help "fast, light, not so seaworthy warship"))

; forts, bases and other special places
(unit-type w (name "wall") (char "w")
  (help "fortified walls"))
(unit-type c (name "castle") (char "c")
  (help "walled castle"))
(unit-type tower (char "T")
  (help "fortified mage's tower"))
(unit-type / (name "camp") (char "/")
  (help "campsite, practically a mobile village"))
(unit-type temple (char "t") (image-name "parthenon")
  (help "holy place of spiritual power"))

; cities
(unit-type V (name "village") (char "V")
  (help "small town"))
(unit-type * (name "town") (char "*")
  (help "smaller than a city"))
(unit-type @ (name "city") (char "@")
  (help "Large city, seat of a city-state"))

; magical items
(unit-type r (name "ring of regeneration") (char "r")
  (help "magical ring that makes its wearer heal faster"))
(unit-type B (name "crystal ball") (char "B")
  (help "magical device that scans the local area"))
(unit-type p (name "ring of protection") (char "p")
  (help "protective magical device"))
(unit-type f (name "everfull plate") (char "f")
  (help "magical wondor that creates food!"))
(unit-type a (name "amulet of power") (char "a")
  (help "generates mana for its wearer"))

(material-type food (help "Foodstuffs: an army marches on its stomach!"))
(material-type mana (help "Magical power"))	; used to produce magical effects
(material-type metal (help "Steel, metal, manufacturing capacity for armour and weapons"))

(terrain-type sea (char ".") (color "sky blue"))
(terrain-type swamp (char "=") (color "yellowgreen"))
(terrain-type desert (char "~") (color "yellow"))
(terrain-type plains (char "+") (color "green"))
(terrain-type forest (char "%") (color "forest green"))
(terrain-type hills (char "(") (color "khaki"))
(terrain-type mountains (char "^") (color "sienna"))
(terrain-type ice (char "_") (color "white"))
(terrain-type void (char ":") (color "black"))

(define t temple)
(define T tower)
(define K cloudkeep)

(add W image-name "wizard")
(add i image-name "hoplite")
(add b image-name "viking")
(add k image-name "knight")
(add P image-name "paladin")
(add R image-name "archer")
(add H image-name "holy-man")
(add s image-name "serf")
(add D image-name "dragon")
(add u image-name "undead")

(add A image-name "mininuke")
(add L image-name "lightning")
;"FireBall" F icon-name
(add F image-name "fire")
(add M image-name "charm-spell")
(add E image-name "eye")

(add G image-name "wagon-2")
(add C image-name "flying-carpet")
(add h image-name "cavalry")
(add S image-name "frigate")
(add g image-name "trireme")

(add w image-name "walltown")
(add c image-name "castle")
(add / image-name "camp")
(add V image-name "village")
(add * image-name "town20")
(add @ image-name "city20")

(add r image-name "ring-2")
(add p image-name "ring-2")
(add B image-name "crystal-ball")
(add f image-name "plate")
(add a image-name "amulet")

(define bases ( w c tower temple cloudkeep / V * @ ))
(define cities ( V * @ ))
(define spells ( A L F M E ))
(define forts ( w c tower cloudkeep ))
(define flyers ( D C cloudkeep ))
(define ships ( S g ))
(define ltfoot ( H R b i s w ))
(define hvyfoot ( k P W ))
(define hoof ( h G ))
(define magickers ( W D ))
(define movers ( W i b k P R H s D u G C cloudkeep h S g ))
(define foot ( W i b k P R H s u ))
(define transport ( G C cloudkeep S g ))
(define magicitems ( r B p f a ))

(define water ( sea ))
(define land ( swamp plains forest desert hills mountains ))

;;; Static relationships.

;; Unit-unit.

;  What units can carry which others
;
; format:
; (#) (units) unit1 capacity -- # of units unit1 can hold.

;FIX unit-size-as-occupant is U1 U2 -> N U1's size as occupant of U2
(table unit-size-as-occupant add (spells u* 1))

;FIX occupant-max is U1 U2 -> N upper limit on occupants by type
(table occupant-max add (W spells 6))
(table occupant-max add (u* magicitems 6))
(table occupant-max add (W u 1))	; wizards can hold the undead they make.
;FIX capacity is N, upper limit on total occupants by size
(add W capacity 10)

(table occupant-max add (D spells 1))
(table occupant-max add (D ( L F ) 3))
(table occupant-max add (D ( W k P ) 1))	; dragonriders?
(add D capacity 3)	; yes, riders take up one of a dragon's spell slots
; odd, but necessary

(table occupant-max add (G ( W i b k P R H s u ) 2))
(add G capacity 2)

(table occupant-max add (H u 1))	; so they can be made!
(add H capacity 1)

(table occupant-max add (S ( W i b k P R H s G h u ) 5))
(add S capacity 5)

(table occupant-max add (C ( W i k P R H s ) 1))
(add C capacity 1)

(table occupant-max add (g ( W i b k P R H s u ) ( 1 1 4 1 1 2 1 1 3 )))
(add g capacity 4)

; bases capacity
(table occupant-max add (w ( W i b k P R H s G h u C ) 2))
(add w capacity 2)

(table occupant-max add (K ( W k P R s D u C ) 4))
(add cloudkeep capacity 8)

(table occupant-max add (c ( W i b k P R H s G h S g u ) 3))
(add c capacity 4)

(table occupant-max add (T ( W i b k P R H s G h u C ) 2))
(add tower capacity 4)

(table occupant-max add (/ ( W i b k P R H s G h S g C ) 6))
(add / capacity 12)

(table occupant-max add (V ( W i b k P R H s G h S g C ) 8))
(add V capacity 16)

(table occupant-max add (t ( W i b k P R H s G h S g C ) 6))
(add temple capacity 10)

(table occupant-max add (* ( W i b k P R H s G h S g C ) 8))
(add * capacity 20)

(table occupant-max add (@ ( W i b k P R H s G h S g C ) 15))
(table occupant-max add (@ D 1))
(add @ capacity 30)

(table occupant-max add (( T cloudkeep ) spells 2))
; 1 A ( T cloudkeep ) capacity ; armageddon spell is 'bigger'?

(table unit-size-as-occupant add (movers u* 1))
(table unit-size-as-occupant add (( D G h S G ) u* ( 3 2 2 4 3 )))	;  the exceptions
(table unit-size-as-occupant add (spells u* 1))

; 90 foot h alter-mobility ; h slowed by holding things?

(table unit-size-as-occupant add (magicitems u* 0))
(table occupant-max add (u* magicitems 5))	; rule of 5 magic items, but everything can hold them
(table occupant-max add (spells magicitems 0))	; except spells
(table occupant-max add (magicitems u* 0))	; and other magic items!

;; Unit-terrain.

;; Unit-material.

(table unit-storage-x 
  (W m* ( 10 100 6 ))
  (i m* ( 16  0 10 ))
  (b m* ( 16  0  6 ))
  (k m* ( 16  0 12 ))
  (P m* ( 12  4  6 ))
  (R m* ( 16  0  6 ))
  (H m* ( 10  8  6 ))
  (s m* ( 10  0  6 ))
  (D m* ( 200 30 6 ))	; dragons can eat alot!
  (u m* (  0  0  4 ))
  (spells food 0)
  (spells metal 0)
  (spells mana 3)
  (L mana 2)
  (E mana 8)	; it has extra duration
  (C m* (  0  10 2 ))
  (G m* ( 150 0  6 ))
  (K m* ( 50 200 10 ))
  (h m* ( 50  0 20 ))
  (S m* ( 100 0 30 ))
  (g m* (  50 0 15 ))
  (w m* (  20 0  6 ))
  (c m* ( 200 0 30 ))
  (T m* ( 100 60 20 ))
  (/ m* ( 100 0 25 ))
  (t m* ( 100 8 20 ))
  (V m* ( 150 0 15 ))
  (* m* ( 300 0 30 ))
  (@ m* ( 600 0 60 ))
  (r m* 10)
  (f food 10)
  (a mana 100)	; holds lots of power, might have it all when found!
  )

;;; Vision.

(add ( V * @ ) see-always 1)
(add D see-always 1)	; Dragons are hard to miss...

;(add u* vision-at 1)
;(add spells vision-at 0)
;(add E vision-at 4)	; the spy-eye!
;(add flyers vision-at 3)
;(add bases vision-at 2)
;(add cloudkeep vision-at 5)
;(add T vision-at 4)	; Towers are good for spotting things
;(add w vision-at 2)	; can see a little ways from walls.
;(add @ vision-at 3)	; cities have tall structures, and people wandering.
;(add * vision-at 2)
;(add c vision-at 3)
;(add R vision-at 2)	; Archers are good at knowing the surrounding land.
;(add ( g S ) vision-at ( 2 3 ))	; ships can see far over flat water

; visibility of stuff
; non 0/1 visibility must be harsh on CPU! keep # of units with this down
;FIX visibility is U T -> N, U's % visibility in T
(table visibility add (K t* 20))	; hard to spot in them clouds!
(table visibility add (C t* 5))	; hard to spot in the clouds
(table visibility add (R t* 50))	; Archers are good at hiding on the land.
(table visibility add (E t* 0))	; invisible spy-eye
(table visibility add (magicitems t* 20))	;  magic items are sometimes hard to spot.

; small forces hidden by rough terrain
;FIX conceal is now visibility, you should subtr these numbers from that table
;FIX ( 80 75 60 40 ) ( forest swamp mountains hills ) R conceal

;;; Actions.

;                         ( W i b k P R H s  D u G C  K h S g ) "movers"
(add movers acp-per-turn (  1 2 3 2 2 3 2 1 10 1 5 12 4 6 5 7 ))

;                         ( A L F M E ) "spells"
(add spells acp-per-turn (  2 9 5 3 6 ))

(add cities acp-per-turn 1)

;;; Movement.

(add bases speed 0)
(add cities speed 0)

(table mp-to-enter-terrain
  (u* t* 99)
  (bases land 1)	; so you can build bases anywhere?
  (ships sea 1)	; seas  -- ships Ok, air OK, most others, no go.
  (flyers t* 1)	; flyers can go almost anywhere.
  (spells t* 1)	; same for spells
  (ships water 1)	; sea is pretty straightforward...
  ; movement on terrain...
  ;( W i b k P R H s  D u A L F G C K h S g ) "movers"
  ; ground units and land terrain:
  ; ( swamp plains forest desert hills mountains ) "land"
  ; ground units ant how they handle terrain:
  (W land (2 1 1 1 1 2))
  (i land (2 1 1 1 1 2))
  (b land (2 1 2 1 1 2))
  (k land (2 1 2 2 2 2))
  (P land (1 1 1 1 2 2))
  (R land (1 1 1 1 1 2))
  (H land (2 1 2 2 2 2))
  (s land (2 1 2 1 2 2))
  (u land (1 1 1 1 1 2))
  (G land (99 1 2 2 3 4))
  (h land (3 1 2 2 3 4))
  (( W b G P R u ) ice 1)	; units that can handle ice!
  (u* void 99)	; nothing can go into the void!
  )

(table consumption-per-move add (movers food 1))	; all units take 1 fuel to move
(table consumption-per-move add (s food 4))	; this is actually lost food production
; Magical things don't require food to move....
(table consumption-per-move add (u food 0))	; undead dont eat!
(table consumption-per-move add (K food 0))
(table consumption-per-move add (K mana 1))	; floating castle requires mana to move
(table consumption-per-move add (spells food 0))
(table consumption-per-move add (C food 0))
(table consumption-per-move add (S food 0))	; they sail!

;;; Construction.

; starting stockpiles
;FIX this used to be % now it is a raw number.
(table supply-on-completion add (u* m* 0))	; we don't start with any resources
(table supply-on-completion add (u* food 100))	; except food...

(table supply-on-completion add (bases food 50))	; bases are only half full. filled units?
(table supply-on-completion add (magickers mana 30))	; magic casters get 30% of their mana capacity

; production by units.
; Unit production time should be starting at a base value, and modified
; by how good the producer is at that particular unit...
; Base times:
;  W  i  b  k  P  R  H  s  D  u  A  L  F  M  G  h  C  K  S  g
; 25  5  8 10 15  9 15  5 30  2  2  2  2  2
; note: short times for spells compensated by large mana costs
;  w  c  T  /  t  V  *  @
;  5 20 15  5 12 30 35 40
;
;
; magickers production
;FIX make is replaced by acp-to-create, cp-per-build and friends
;FIX 2 spells W make
;FIX ( 25 2 10 30 10 ) ( D u C K T ) W make	; wiz's make magic stuff
;FIX 2 ( F L ) D make

; first, production by city-type units
;FIX ( 2 2 30 20 ) ( A L D W ) K make

;FIX 15 temple P make	; paladins can make shrines, eventually

;FIX 7 / R make	; Archers can camp...

;FIX ( 11 5 ) ( temple u ) H make	; 'holy' men can make undead?
;FIX ( 6 25 15 6 10 30 35 40 ) ( w c T / t V * @ ) s make	;serfs make lots of stuff

;FIX ( 10 17 5 15 ) ( k P i h ) c make	; Castles

;FIX ( 12 20 ) ( H P ) temple make
;FIX (  8 25 6 ) ( w c / ) i make


;      i b  R  H s G  h  S  g  W  k  P ) make vector
;FIX ( 6 8 10 18 6 7 18 19 16          ) ( i b R H s G h S g       ) V make
;FIX ( 5 5 11 15 5 7 16 14 13 32  9    ) ( i b R H s G h S g W k   ) * make
;FIX ( 4 4 11 13 5 6 14 13 11 25  7 20 ) ( i b R H s G h S g W k P ) @ make

(add ( i b  R  H s G  h  S  g  W  k  P )
  cp ( 6 8 10 18 6 7 18 19 16 30  9 20))

(table acp-to-create
  (T W 1)
  (cities ( i b  R  H s G  h  S  g ) 1)
  (( * @ ) ( W  k  P ) 1)
  )

(table cp-on-creation
  (T W 1)
  (cities ( i b  R  H s G  h  S  g  ) 1)
  ;; Towns and cities build faster by having new units more complete.
  (* ( i b  R  H s G  h  S  g  W  k  P )
     ( 2 2  1  4 2 1  3  6  4  1  1  1 ))
  (@ ( i b  R  H s G  h  S  g  W  k  P )
     ( 3 3  1  6 2 2  5  7  8  6  3  1 ))
  )

(table acp-to-build
  (T W 1)
  (cities ( i b  R  H s G  h  S  g  W  k  P ) 1)
  )

(table cp-per-build
  (T W 1)
  (cities ( i b  R  H s G  h  S  g  W  k  P ) 1)
  )

;; Material needed to make units.

;  mana required for magical stuff
; typical mage should get 2 to 4 mana per turn, dep on site...
(table consumption-per-build
  ;                A  L   F  M  E  spells
  (spells mana (  50 14  12 20  6 ))
  (( D u cloudkeep C ) mana ( 200 10 400 80 ))
  ; most units don't really take food to make...
  ; but armoured ones take metal...
  (i metal 3)
  (k metal 15)
  (P metal 15)
  (G metal 5)
  (S metal 40)
  (g metal 25)
  (w metal 20)
  (c metal 80)
  (T metal 20)
  (/ metal 3)
  (V metal 45)
  (* metal 60)
  (@ metal 120)
  (magicitems mana ( 30 20 25 10 60 ))
  )

; mages must first figure out how to cast their spells!
;FIX 200 spells research
;FIX 300 M research
;FIX 1000 A research

;;; Repair.

; do this by what a unit may repair
;FIX hp-per-repair is U1 U2 -> .01HP that U1 restores to U2 per repair action
; ...and you have to add...
; acp-to-repair is U1 U2 -> ACP to do one repair action
;(table acp-to-repair add ( W 1))
;(table hp-per-repair add ( W ( 33 33 33 25 50 33 33 33 33 50 33 33 33 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 )))
(table acp-to-repair add (i bases 1))
(table hp-per-repair add (i bases 50))
(table acp-to-repair add (b g 1))
(table hp-per-repair add (b g 50))
(table acp-to-repair add (b bases 1))
(table hp-per-repair add (b bases 33))
(table acp-to-repair add (k c 1))
(table hp-per-repair add (k c 50))
(table acp-to-repair add (P bases 1))
(table hp-per-repair add (P bases 50))
(table acp-to-repair add (P P 1))
(table hp-per-repair add (P P 50))	; paladin's healing powers!
(table acp-to-repair add (R ( R / ) 1))
(table hp-per-repair add (R ( R / ) 50))
(table acp-to-repair add (s s 1))
(table hp-per-repair add (s s 33))
(table acp-to-repair add (H foot 1))
(table hp-per-repair add (H foot 100))
(table acp-to-repair add (s bases 1))
(table hp-per-repair add (s bases 100))
(table acp-to-repair add (s G 1))
(table hp-per-repair add (s G 100))
(table acp-to-repair add (s S 1))
(table hp-per-repair add (s S 50))
(table acp-to-repair add (D D 1))
(table hp-per-repair add (D D 100))
(table acp-to-repair add (h h 1))
(table hp-per-repair add (h h 25))
(table acp-to-repair add (u* u 1))
(table hp-per-repair add (u* u 100))	; undead can't be repaired...
(table acp-to-repair add (W cloudkeep 1))
(table hp-per-repair add (W cloudkeep 50))	;  Wizards slowly repair cloud castles
(table acp-to-repair add (G foot 1))
(table hp-per-repair add (G foot 50))

(table acp-to-repair add (bases u* 1))
(table hp-per-repair add (bases u* 100))
(table acp-to-repair add (forts u* 1))
(table hp-per-repair add (forts u* 100))
(table acp-to-repair add (cities u* 1))
(table hp-per-repair add (cities u* 100))
(table acp-to-repair add (forts forts 1))
(table hp-per-repair add (forts forts 25))	; forts have a few people who can fix them
(table acp-to-repair add (cities cities 1))
(table hp-per-repair add (cities cities 25))	; cities have the people to maintain themselves...
(table acp-to-repair add (c ( i cloudkeep h ) 1))
(table hp-per-repair add (c ( i cloudkeep h ) 100))
(table acp-to-repair add (T ( W ) 1))
(table hp-per-repair add (T ( W ) 100))
(table acp-to-repair add (t foot 1))
(table hp-per-repair add (t foot 100))

(table mp-to-enter-unit add (flyers u* 20))	; aircraft can't sortie again until next turn
(table mp-to-enter-unit add (ships bases 20))
(table mp-to-leave-unit add (ships bases 1))

;;; Combat.

; HP of various units.
(define units ( W i b k P R H s D u h A L F M E G C  cloudkeep S g  w  c  T  /  t  V  *  @ ))
(add units hp-max ( 2 2 2 3 4 2 2 2 8 4 3 1 1 1 2 1 2 1 10 4 3 10 20  8  4  8 15 30 45 ))
;FIX in addition you can set hp-to-repair and other things to cripple a unit
;(add units hp-at-min-speed ( 1 1 1 1 1 0 1 0 3 0 2 1 1 1 1 1 1 0  4 2 1  2  3  4  2  2  8 16 25 ))
; NOTE: crippled is at <= value, not <

(add spells hp-max 2)	; not 1 so that they can survive an attack when attacking.
;(add spells hp-at-min-speed 0)
; 1 M hp ; So mass charm survives the attack to capture!

; split the matrix in half to make it fit on an 80 col screen!
(define u1 (  W   i   b   k   P   R   H   s   D   u   G   C   h ))
(define u2 (  A   L   F   cloudkeep   S   g   w   c   T   /   t   V   *   @ ))
; (chances) u# unit hit
; chances for unit to hit member of u#

; so, here we define the units by how good they are at hitting others
; ranges:  30% is typical attack percent.
; it ranges up and down from there...
;( W   i   b   k   P   R   H   s   D   u   G   C   h ) u1
(table hit-chance add (W u1 ( 25  35  10  30  10  35  20  40  40  30  40  40  30 )))
(table hit-chance add (i u1 ( 50  30  40  20  15  60  25  45  10  20  40  30  20 )))
(table hit-chance add (b u1 ( 45  45  35  25  25  55  35  45  15  30  50  20  25 )))
(table hit-chance add (k u1 ( 35  40  45  25  20  65  30  50  20  30  30  15  30 )))
(table hit-chance add (P u1 ( 40  40  45  35  30  80  30  30  40  50  30  15  30 )))
(table hit-chance add (R u1 ( 30  45  50  25  20  65  30  40  20  15  30  30  25 )))
(table hit-chance add (H u1 ( 30  20  35  20  20  50  35  30  10  45  30  10  20 )))
(table hit-chance add (s u1 ( 20  10  15   5   3  10  10  30   1  10  30   5  10 )))
(table hit-chance add (D u1 ( 40  50  55  35  20  70  40  80  30  40  60  40  80 )))
(table hit-chance add (u u1 ( 20  30  35  20  10  50  10  50  10  30  40  30  30 )))
(table hit-chance add (A u1 ( 50  99  60  90  70  90  80  99  60  99  99  99  99 )))
(table hit-chance add (L u1 ( 50  80  50  90  60  80  80  99  70  99  99  99  99 )))
(table hit-chance add (F u1 ( 50  90  50  80  60  90  80  99  60  99  99  99  99 )))
(table hit-chance add (G u1 (  0   0   0   0   0   0   0   0   0   0   0   0   0 )))	; noncombatants
(table hit-chance add (C u1 (  0   0   0   0   0   0   0   0   0   0   0   0   0 )))	; noncombatants
(table hit-chance add (K u1 ( 10   5   5   5   5   5   5   5  25   5  15  20   5 )))	; Cloudkeep can drop rocks!
(table hit-chance add (h u1 ( 50  45  50  30  30  70  40  60  25  40  50  15  30 )))	; Cavalry
(table hit-chance add (S u1 (  0   0   0   0   0   0   0   0   0   0   0   0   0 )))	; ships can only attack other ships
(table hit-chance add (g u1 (  0   0   0   0   0   0   0   0   0   0   0   0   0 )))
(table hit-chance add (w u1 ( 11  30  35  20  10  40  30  40  20  30   0   0  20 )))
(table hit-chance add (c u1 ( 10  40  40  25  12  40  35  40  25  35   0   0  20 )))
(table hit-chance add (T u1 ( 10  35  35  20  10  40  30  40  20  30   0   0  20 )))
(table hit-chance add (/ u1 (  5  20  17  15  10  25  20  30  10  20   0   0  10 )))
(table hit-chance add (t u1 (  8  25  30  20  18  35  30  40  15  35   0   0  15 )))
(table hit-chance add (V u1 (  9  25  30  20  15  30  30  40  15  20   0   0  25 )))
(table hit-chance add (* u1 ( 10  30  35  25  20  40  35  45  20  25   0   0  30 )))
(table hit-chance add (@ u1 ( 15  35  40  30  25  50  40  50  25  30   0   0  30 )))

;( A   L   F   K   S   g   w   c   T   /   t   V   *   @ ) u2
(table hit-chance add (W u2 (  1   1   1  50  30  40  25  20  25  30  35  40  40  40 )))
(table hit-chance add (i u2 (  1   1   1  10  10  10  10  10  10  25  20  20  20  20 )))
(table hit-chance add (b u2 (  1   1   1  10  10  10   8   5   8  40  30  50  50  50 )))
(table hit-chance add (k u2 (  1   1   1  10  10  10   9   9  10  20  25  30  30  30 )))
(table hit-chance add (P u2 (  1   1   1  11   7   8  15  15  15  20  40  20  20  20 )))
(table hit-chance add (R u2 (  1   1   1   8   5   5   8   8   8  30  30  30  30  30 )))
(table hit-chance add (H u2 (  1   1   1   3   4   4   4   4   4  20  50   5   5   5 )))
(table hit-chance add (s u2 (  1   1   1   1   2   2   1   2   2  20  15  20  20  20 )))
(table hit-chance add (D u2 (  1   1   1  20  60  50  30  25  28  50  40  60  60  60 )))
(table hit-chance add (u u2 (  1   1   1   2  10  10  11  11  10  30  20  30  30  30 )))
; there is some value in chances >100%, since may be adjusted downwards by other factors,
; but for now they're not allowed.
;(table hit-chance add (A u2 (  1   1   1  30 199 199 199 150 150 299  70 130 110 100 )))
(table hit-chance add (A u2 (  1   1   1  30 100 100 100 100 100 100  70 100 100 100 )))
(table hit-chance add (L u2 (  1   1   1  30  40  40  80  80  80  99  70  90  90  80 )))
(table hit-chance add (F u2 (  1   1   1  30  99  99  60  50  69  99  75  90  90  90 )))
(table hit-chance add (G u2 (  1   1   1   0   0   0   0   0   0   0   0   0   0   0 )))	; noncombatat
(table hit-chance add (C u2 (  1   1   1   0   0   0   0   0   0   0   0   0   0   0 )))	; noncombatant
(table hit-chance add (K u2 (  1   1   1  40   0   0   0   0   0   0   0   0   0   0 )))
(table hit-chance add (h u2 (  1   1   1  10  10  10   6   6   9  40  35  30  30  30 )))	;
(table hit-chance add (S u2 (  1   1   1   0  20  10   0   0   0   0   0   0   0   0 )))	; ships ramming each other!
(table hit-chance add (g u2 (  1   1   1   0  50  30   0   0   0   0   0   0   0   0 )))
(table hit-chance add (w u2 (  1   1   1   0   0   0   0   0   0   0   0   0   0   0 )))	; matrix here and below  is all non-movers vs non-movers!
(table hit-chance add (c u2 (  1   1   1   0   0   0   0   0   0   0   0   0   0   0 )))
(table hit-chance add (T u2 (  0   1   0   0   0   0   0   0   0   0   0   0   0   0 )))
(table hit-chance add (/ u2 (  0   0   0   0   0   0   0   0   0   0   0   0   0   0 )))
(table hit-chance add (t u2 (  0   0   0   0   0   0   0   0   0   0   0   0   0   0 )))
(table hit-chance add (V u2 (  0   0   0   0   0   0   0   0   0   0   0   0   0   0 )))
(table hit-chance add (* u2 (  0   0   0   0   0   0   0   0   0   0   0   0   0   0 )))
(table hit-chance add (@ u2 (  0   0   0   0   0   0   0   0   0   0   0   0   0   0 )))

(table hit-chance add (E u* 0))
(table hit-chance add (M u* 10))	; can do some damage as it attempts to capture

; and this allows ti to even attack...
(table hit-chance add (movers spells 50))	;  spells can be killed by attacking them, but
(table hit-chance add (movers M 10))	;
; they shouldn't have much time where they can be attacked.
; they are either in-flight, or stored on someone!

; terrain effects
(table defend-terrain-effect add (foot ( forest swamp mountains hills ) 85))
(table defend-terrain-effect add (hvyfoot ( forest swamp mountains hills ) 90))

(table defend-terrain-effect add (R ( forest swamp mountains hills desert ) (25 40 40 60 90)))
; archers are good at using terrain to hide in.

(table defend-terrain-effect add (forts ( hills mountains ) 60))
(table defend-terrain-effect add (T ( hills mountains ) (80 50)))
(table defend-terrain-effect add (c ( hills mountains ) (80 50)))

(table damage
  (u* u* 1)	; default
  (D u* 2)
  (u* magicitems 0)	;  otherwise, items are always destroyed by capture
  (D bases 3)
  (D cities 4)
  (b bases 3)	; barbarians torch them cities!
  (R D 4)	; rangers with those special arrows!
  (g ships 2)
  (( P H ) u 3)
  (h ltfoot 1)
  (F ships 4)
  (F cities 4)
  (F u* 2)
  (F / 4)
  (L u* 3)
  (M u* 1)	; it might do some damage as the victim struggles
  (A u* 21)	; Armageddon spells are nuclear!
  (magicitems u* 0)	; magic items can't hurt anything
  )

;FIX true capturemoves	; gotta secure newly captured units first
; false capturemoves is buggy?

;FIX true counterattack

(table acp-to-defend add (spells u* false))	; they're specifically targeted!
(table acp-to-defend add (w u* false))	; walls don't fight back! They're just walls!
(add spells acp-to-detonate 1)


; stuff that can change sides when captured
(add u* acp-to-change-side 0)	; most units are fanatically loyal beings
(add transport acp-to-change-side 100)	;  inanimate units that may be captured
(add magicitems acp-to-change-side 100)	;
(add ( W i b s D ) acp-to-change-side ( 20 20 40 80 20 ))

; these units may be persuaded to go traitor

(table capture-chance
  (W ( W i k s D u A L F G K T ) ( 20 20 20 40 20 40 30 30 30 20 50 40 ))

  ;                                 ( s  G h  S  g  w c  T  /  t  V  *  @ )
  (i ( s G h S g w c T / t V * @ ) ( 40 50 5 30 30 10 5  8 30 20 40 25 15 ))
  (b ( s G h S g w c T / t V * @ ) ( 40 50 5 45 40  8 3  7 40 25 20 10  7 ))
  (R ( s G h S g w c T / t V * @ ) ( 10 20 5 10 10  8 3  7 40 25 45 25 10 ))
  (k ( s G h S g w c T / t V * @ ) ( 45 40 5 15 10 12 6  9 35 25 45 25 13 ))
  (P ( s G h S g w c T / t V * @ ) ( 50 30 8 20 20 11 7 10 36 45 40 20 10 ))
  (h ( s G h S g w c T / t V * @ ) ( 50 40 5  5  5  5 1  5 60 30 40 20 10 ))
  (H ( s G h           / t V * @ ) ( 60 10 10              20 35 40 15 15 ))
  (u ( G S   S g w c T / t V * @ ) ( 40 50   25 30 11 6  9 20 10 15 10  5 ))

  (H ( i b u ) ( 20 15 40 ))	; priests can convert people!
  (s ( s G cloudkeep / V * @ ) ( 20 30 45 30 30 20 15 ))

  ; the Charm Spell...
  (M u* 100)	; the standard default

  ; the 200% of M vs / is not accepted, though perhaps it should be.
  (M ( w c T / t V * @ ) ( 100 50 100 100 #|200|# 25 60 50 40 ))
  ; works better against low populations

  (M ( W P H D cloudkeep T ) ( 50 10 40 50 25 80 ))
  (u* magicitems 100)	;  magical items don't have loyalty
  )

;FIX ferry-on-entry is U1 U2 -> FTYPE how much terrain U2 crosses to board U1
(table ferry-on-entry add (u* u* over-all))	; lets everything attack everywhere: no terrain immunity

; protect: # u1 u2 protect: protection offered to u1 by u2
(table protection add (bases u* 70))	; general bases protect their occupants ok
(table protection add (forts u* 20))	; forts prottect their occupants very well
(table protection add (/ movers 80))
(table protection add (V movers 80))
(table protection add (* movers 70))
(table protection add (@ movers 60))
(table protection add (( i k P W ) forts 50))
(table protection add (u forts 60))
(table protection add (( i k P W ) bases 70))
(table protection add (H temple 50))	; priests and their temples...
(table protection add (W ( cloudkeep tower ) 20))	; wiz's protect their towers and keeps very well

;FIX true u* neutral
;FIX false i neutral

;FIX U1 U2 -> % that U2 withdraws before U1 attacks
(table withdraw-chance-per-attack add (u* movers 10))
(table withdraw-chance-per-attack add (u* R 33))	; those rangers like to run away!
(table withdraw-chance-per-attack add (u* ships 0))	; boats can't retreat!

(table consumption-per-attack add (D food -5))	; dragon feeds on enemies!
(table consumption-per-attack add (W mana 1))	; mages use spells in combat!
(table consumption-per-attack add (K metal 1))	; Cloudcastles throw stuff down to attack

;FIX spy-chance is now on a per unit basis...
; and spy-range specifies how far a unit can spy
(add u* spy-chance false)	; we don't spontaneously see all the enemies
;FIX spy-quality is U1 U2 -> % that U1 returns info about U2
(table spy-quality (u* u* false))	; and we don't see any of them if we do!

;FIX I guess nuked is damaged-terrain now, T1 T2 -> N
;FIX t* t* nuked	; most terrain won't actually change when nuked
;FIX desert ( plains forest ) nuked	; except forest, plains turn to desert.
;FIX mountains ice nuked	; ice gets nuked into mountains.

;FIX "defeats" land destroy-message
;FIX "sinks" ships destroy-message
;FIX "knocks down" flyers destroy-message
;FIX "sacks" cities destroy-message
;FIX "slays" ( D h ) destroy-message

; NIY? 100 u* max-quality ;  what's the range of this?
; NIY? 100 u* veteran ;
; NIY? 1000 u* max-morale ;  what's the range of this?
;FIX 50 u* efficiency	; disbanding units for their parts?

;FIX true u* neutral	;everything can become neutral

;FIX hp-per-disband is U1 U2 -> HP lost in a disband action performed by U2
; you might add U1 acp-to-disband U2 as well, U1 U2 -> ACP
; (table hp-per-disband add (u* u* 100))	; perhaps they're not so fanatic?
; (table hp-per-disband add (( W i b k D u A L F G h C S g / ) u* 100))

(add magicitems hp-max 1)	;

; combat stats should be irrelevant, since they're always occupants
(table hit-chance add (u* magicitems 100))	; everything can hit them
(table hit-chance add (magicitems u* 0))	; they can't hit back!
(table acp-to-defend add (magicitems u* 0))
(table capture-chance add (u* magicitems 100))	; just pick them up!

(add magicitems acp-to-change-side 100)
(table protection add (u* p 80))	; ring of protection, reduces hit-chance by 20%

; (table hp-per-disband add (magicitems u* 100))	; they can' be destroyed!
;FIX true magicitems neutral

;;; Scoring.

(add bases point-value ( 1 20 8 8 30 2 8 10 50 ))	;  territory values for victory pts.
(add s point-value 5)	; serfs are valuable!

(scorekeeper (do last-side-wins))

;( r B p f a ) magicitems
; magic items stats tacked on at the end!
(table independent-density add (magicitems t* 5))	; some random ones to be lying around easy to capture
(table favored-terrain add (magicitems sea 0))	; they'd sink!
(table supply-on-completion add (magicitems m* 100))	; they're usually statically full
;FIX ( 10 6 8 6 15 ) magicitems W make	; wizards make them only
;FIX ( 12 12 9 4 0 ) magicitems H make	; priests can make some items
;FIX 50 magicitems research	; discover the magic formulae?
;FIX 25 magicitems magicitems research-contrib

(table acp-to-repair add (r u* 1))
(table hp-per-repair add (r u* 100))	; ring of regeneration magically fixes everything!
; ring must be able to hold supplies to perform repairs! Sheesh!

(table base-production add (r m* 1))	; ring produces everything, just in case...
(table base-production add (f food 6))	; FOOD!
(table base-production add (a mana 6))	; amulet of power generates mana

(table productivity add (magicitems t* 100))

(table consumption-as-occupant add (magicitems m* 0))	; they dont consume anyway!
(table mp-to-enter-terrain add (magicitems sea 99))
(table mp-to-enter-terrain add (magicitems land 1))

;(add B vision-at 10)	; that's what its for!

;;; Backdrop economy.

(table base-production add (W mana 5))	; * very terrain dependent
(table base-production add (D mana 4))	; *
(table base-production add (H mana 2))
(table base-production add (K mana 12))
(table base-production add (T mana 5))	; *
(table base-production add (t mana 2))
(table base-production add (P mana 1))

; food production
; i b k P R H  s c T / t V  *  @ K )
(table base-production add (( i b k P R H s c T / t V * @ K ) food ( 2 4 2 3 6 4 10 2 1 3 6 8 12 16 3 )))
(table base-production add (W food 1))

; metal production
(table base-production add (( H s T c V * @ ) metal ( 2 4 1 2 4 8 16 )))

(table productivity add (u* t* 100))	;the default anyway!
(table productivity add (foot sea 0))
(table productivity add (hoof sea 0))
(table productivity add (( H P ) sea 50))

; sea swp des pln for hil mtn ice vac
;(  .   =   -   +   %   ~   ^   _  : ) t*
(table productivity add (W t* ( 60  60  60  40  60  80 100 120  0 )))	; 1 per 20%
(table productivity add (D t* ( 75  50  50  50  75  75 100 125  0 )))	; 1 per 25%
(table productivity add (T t* (  0  60  40  40  60  80 100 120  0 )))	; 1 per 20%
; considerable advantage for mana production on 'ice' and other
; difficult terrain.
; ice is usually highest mountain.
; note, building a T on ice is a pain, as a wiz must do it!

; swamp/plains/forest/desert/hills/mountains = land
(table productivity add (i land (  50 100  90 20  25  50 )))	; one arg must be a scalar...
(table productivity add (b land (  50 100  90 20  25  50 )))
(table productivity add (k land (  00 100  90 20  25  50 )))
(table productivity add (P land (  50 100 100 50 100  75 )))
(table productivity add (R land ( 100 100 100 50 100 100 )))
(table productivity add (H land (  75 100 100 50  75  75 )))
(table productivity add (s land (  50 100  90 70  75  50 )))
(table productivity add (/ land (  50 100  90 70  75  50 )))
(table productivity add (t land (  50 100  90 70  75  50 )))
(table productivity add (V land (  50 100  90 25  75  50 )))
(table productivity add (* land (  50 100  90 25  75  50 )))
(table productivity add (@ land (  50 100  90 25  75  50 )))

(table productivity add (ships sea 100))
(table productivity add (K t* 100))


; units' comsumption
(table base-consumption add (movers food 1))	; all consume 1 food per turn, except...
(table base-consumption add (( i k ) food 2))	; many infantry per unit, knights are pigs
(table base-consumption add (D food 2))	; dragons are voracious!
(table base-consumption add (undead food 0))	; just skeletons, not ghouls!
(table base-consumption add (spells food 0))
(table base-consumption add (C food 0))
(table base-consumption add (( h G ) food 2))	; horses eat a lot
(table base-consumption add (g food 1))	; crew needs to eat more when active

(table base-consumption add (spells mana 1))	; spells burn mana in-flight
(table base-consumption add (C mana 1))	; magic carpet burns mana in flight.

; mutual supply rules:
; consume-as-occupant is cool!  1 means they do consume!
(table consumption-as-occupant add (u* m* 1))
(table consumption-as-occupant add (spells m* 0))	; spells in storage require no mana support
(table consumption-as-occupant add (ships m* 0))	; ships not at sea need no food...

; inlength is how far away a unit can receive fuel from
; out-length is how far away a unit can deliver fuel.  Should be kept
; small, as it can waste much CPU time
;
;set the supply line the defaults...

(table out-length add (movers m* 0))	; most movables can't supply other things.
(table out-length add (bases m* 1))	; bases somewhat supply neighbors
(table out-length add (cities m* 2))	; cities trade with surrounding lands
(table out-length add (W mana 0))	; mages selfishly hoard magic to themselves.
(table out-length add (G food 2))	; wagons are supply vehicles
(table out-length add (s m* 1))	; serfs will trade supplies to units near them
(table out-length add (K m* ( 0 3 0 )))	; flying citadel magically transmits its mana
(table out-length add (magicitems m* 0))
(table out-length add (a mana 2))

(table in-length add (movers m* 1))	; but they can be supplied by nearby things.
(table in-length add (bases m* 1))	; bases can recieve things from neighbors.
(table in-length add (spells mana -1))	; you have to charge the spells yourself
(table in-length add (s m* 1))
; 0 mana bases in-length ; They generate it, not take it!
; leave it with normal  in-length -- bases can exchange with each other.
(table in-length add (magicitems m* 0))
(table in-length add (a mana -1))	; it doesn't take mana, just give it!

;FIX survival is now hp-per-starve U M -> HP
(table hp-per-starve add (( R b ) m* 75))	; archer, barbarian can live off the land.
(table hp-per-starve add (( H P ) m* 50))	; holy dudes are used to fasting
(table hp-per-starve add (s m* 33))	; peasants don't need the food that badly... :-)

;;; Random events.

; disasters: units are in HUNDREDTHS of a percent each TURN
(add magickers revolt-chance 5)	; sure, tell wizards and dragons orders! Good luck!
(add b revolt-chance 5)	; those crazy barbarians
(add s revolt-chance 100)	; Serfs dont' enjoy being oppressed

;FIX surrender is U1 U2 -> .01N chance that U1 surrenders to U2
; and surrender-range is distance at which it can occur
(table surrender-chance add (forts u* 50))
(table surrender-chance add (cities u* ( 100 60 30 )))
;FIX 1500 bases siege	; most things could be sieged easily
;FIX 350 forts siege	; rare for a siege to take a fort...
;FIX ( 1500 3000 3000 ) ( V * @ ) siege	; medieval cities fell easily
;FIX 0 cloudkeep siege
(table surrender-chance add (K u* 0))

; attrition. Mostly ships at sea
;FIX attrition is U T -> .01HP (rate of loss, hp/turn)
(table attrition add (ships sea ( 200  1000  )))
;FIX "gets caught in a storm" ships attrition-message


;FIX accident is U T -> .01N chance that an accident happens to U
; and you can set accident-damage
(table accident-hit-chance add (ships sea ( 10 50 )))	; ships arent too seaworthy...
;FIX "lost in a storm" ships accident-message

;heavy-foot units should have accidents in swamp and/or mountains
(table attrition add (foot ( mountains  swamp ) 100))
(table accident-hit-chance add (foot ( mountains  swamp ) 50))
(table attrition add (hvyfoot ( mountains swamp ) 200))
(table accident-hit-chance add (hoof ( mountains swamp ) 100))
(table attrition add (hoof ( mountains swamp ) 300))
;FIX "fell down and couldn't get up!" foot accident-message
;FIX "broke down and had to be destroyed." hoof accident-message


;and magickers should have 'accidents'
(table attrition add (magickers t* 50))
;FIX "had a dangerous Arcane accident" magickers attrition-message
(table accident-hit-chance add (magickers t* 5))
;FIX "returns to its lair to hibernate and disappear into legend" D accident-message
;FIX "disappears into a puff of smoke" W accident-message

;;; Setup.

; sea swp des pln for hil mtn ice vac
;(  .   =   -   +   %   ~   ^   _  : )
(add t* alt-percentile-min (   0  29  70  70  70  75  93  99  0 ))
(add t* alt-percentile-max (  69  71  93  93  93  98  99 100  0 ))

(add t* wet-percentile-min (   0  50   0  20  80   0   0   0  0 ))
(add t* wet-percentile-max ( 100 100  20  80 100  99 100 100  0 ))
; ranges at which we see the various terrain types
; units are altitude
; first is for altitude
; next is for  wetness

;FIX this is broken into alt-blob-size, etc. etc.
;FIX 45 alt-roughness	; 0-100  the higher this is, the more altitude roughness
;FIX this is broken into wet-blob-size, etc. etc.
;FIX 80 wet-roughness	; Similar to alt-roughness

(set edge-terrain void)	; Edge of the world: you'll fall off!

(set country-radius-min 4)	;  the radius of a 'country'
(set country-separation-min 17)	;  the closest a country may be
(set country-separation-max 90)	;  the furthest apart a country may be.

(add cities start-with ( 1 2 1 ))	; units player starts with
(add ( c s temple W ) start-with ( 1 4 1 1 ))	; some more misc units

(table independent-density 
  (V land 60)
  (* land 30)
  (@ land 15)
  ; some rare freaks to find
  (K t* 1)
  (s plains 50)
  (W land 2)
  (C land 5)
  (T land 10)
  (t land 20)
  (D (plains mountains) 5)
  )

(table favored-terrain 
  (u* sea 0)
  (u* land 30)

  (forts land 100)

  (K t* 100)	; the flying citadel(s) could be anywhere!
  (K void 0)	; but the void, of course

  (magickers land 30)
  (magickers hills 60)
  (magickers mountains 100)

  (s land ( 20 100 80 10 40 20 ))

  (hoof mountains 0)	; just in case...

  (cities land 20)
  (cities plains 100)

  (u* void 0)	; nothing goes in void
  )

(add u* already-seen 100)
(add cloudkeep already-seen 0)	; the K's shouldn't be seen

(add u* initial-seen-radius 5)

(include "ng-weird")

(add (W D) namer "short-generic-names")
(add cities namer "short-generic-names")

;(set random-events (units-revolt units-revolt units-revolt))

;;; Documentation.

(game-module (instructions (
  )))

(game-module (notes (
  )))

(game-module (design-notes (
  )))
