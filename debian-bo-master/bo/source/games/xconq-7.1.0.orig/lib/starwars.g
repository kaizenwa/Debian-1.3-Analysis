(game-module "starwars"
  (title "Rebel and Empire")
  (blurb "Star Wars, sort of")
  (variants
   (world-seen true)
   (see-all false)
   )
)

(unit-type stormtrooper (char "s") (image-name "trooper")
  (help ""))
(unit-type walker (char "w") (image-name "walker")
  (help ""))
(unit-type hovercraft (char "h") (image-name "hovercraft")
  (help ""))
(unit-type x-wing (char "x") (image-name "xwing")
  (help "X-wing fighter"))
(unit-type tie-fighter (char "t") (image-name "tie-fighter")
  (help ""))
(unit-type y-wing (char "y") (image-name "ywing")
  (help "Y-wing fighter-bomber"))
(unit-type cruiser (char "C") (image-name "imperial-cruiser")
  (help ""))
(unit-type death-star (name "Death Star") (char "D") (image-name "death-star")
  (help "galaxy's most feared weapon"))
(unit-type town (char "*") (image-name "town22")
  (help "ordinary town"))
(unit-type city (char "@") (image-name "city30")
  (help "immense floating city, can move around"))

(define s stormtrooper)
(define w walker)
(define h hovercraft)
(define x x-wing)
(define t tie-fighter)
(define y y-wing)
(define C cruiser)
(define D death-star)
(define * town)
(define @ city)

(material-type fuel (help "basic motive power"))

(terrain-type sea (char ".") (color "sky blue"))
(terrain-type plains (char "+") (color "green"))
(terrain-type forest (char "%") (color "forest green"))
(terrain-type desert (char "~") (color "yellow"))
(terrain-type mountains (char "^") (color "sienna"))
(terrain-type ice (char "_") (color "white"))
(terrain-type vacuum (char ":") (color "black"))

(define cities ( * @ ))
(define makers ( D * @ ))
(define movers ( s w h x t y C D @ ))
(define water ( sea ))
(define land ( plains forest desert mountains ))

;;; Static relationships.

(table vanishes-on
  (stormtrooper vacuum true)
  (walker sea true)
  (cities vacuum true)
  )

;; Unit-unit capacities.

(table unit-size-as-occupant
  (u* u* 100) ; disables occupancy usually
  (movers cities 1)
  (@ cities 100)
  ;; Have to build death stars out in the open.
  (D cities 100)
  (( s w h x t y ) C ( 3 3 1 1 1 3 ))
  (( s w h x t y ) D ( 3 3 1 1 1 3 ))
  )

(add cities capacity (10 20))
(add (C D) capacity (6 24))

;;; Unit-terrain capacities.

;; Limit units to 16 in one cell, for the sake of playability and
;; and drawability.  Places cover most of a cell, however.
  
(table unit-size-in-terrain
  (u* t* 1)
  (cities t* 12)
  )

(add t* capacity 16)

;;; Unit-material capacities.

(table unit-storage-x (u* fuel ( 30 30 30 20 22 35 200 500 200 400 )))

;;; Vision.

(add cities see-always 1)

;;; Actions.

;                      s w h x t y C D * @
(add u* acp-per-turn ( 3 4 6 9 8 7 6 2 1 1 ))

;;; Movement.

(add town speed 0)

(table mp-to-enter-terrain
  (u* t* 99)
  (s land 1)
  (w land 1)
  (w ice 1)
  (D t* 1)
  (( h C ) t* 1)
  (( x t y ) t* 1)
  ;; Floating cities can move around a planet,
  ;; but not into space.
  (@ t* 1)
  (@ vacuum 99)
  )

(table consumption-per-move
  (movers fuel 1)
  (s fuel 0)
  )

;;; Construction.

;            s  w  h  x  t  y  C  D  *  @
(add u* cp ( 4  6  6  5  5  8 15 40  1  1 ))

(table acp-to-create
  (cities movers 1)
  (@ @ 0)
  )

(table cp-on-creation
  (cities movers 1)
  (@ @ 0)
  )

(table acp-to-build
  (cities movers 1)
  (@ @ 0)
  )

(table cp-per-build
  (cities movers 1)
  (@ @ 0)
  )

;FIX hp-per-repair is U1 U2 -> .01HP that U1 restores to U2 per repair action
; ...and you have to add...
; acp-to-repair is U1 U2 -> ACP to do one repair action
(table acp-to-repair add (makers u* 1))
(table hp-per-repair add (makers u* 100))
(table acp-to-repair add (( C D ) ( C D ) 1))
(table hp-per-repair add (( C D ) ( C D ) 14))

;;; Combat.

;                s  w  h  x  t  y  C  D  *  @ 
(add u* hp-max ( 1  2  1  1  1  2 10 40 20 40 ))

(table acp-to-attack
  (u* u* 1)
  ;; Cities may not initiate attacks.
  (@ u* 0)
  )

(table hit-chance
  ;         s   w   h   x   t   y   C   D   *   @
  (s u* (  65  40  30  20  20  30  10   5   3   5 ))
  (w u* (  75  59  39  20  20  30  20  10   3   5 ))
  (h u* (  50  60  80  20  20  30   0   0   3   5 ))
  (x u* (  40  35  80  80  80  80  20   5  40  40 ))
  (t u* (  40  25  80  80  80  80  20   5  40  40 ))
  (y u* (  20  35  50  50  50  50  50  25  75  75 ))
  (C u* (  20  25  80  80  80  80  80  50  90  90 ))
  (D u* (  20  25  80  80  80  80  80  50  90  90 ))
  (* u* (  30  20  60  80  90  50  50  50   0   0 ))
  (@ u* (  50  40  60  80  90  50  50  50   0   0 ))
  )

(table damage
  (u* u* 1)
  (( C D ) w 2)
  (( C D ) ( C D ) 3)
  (cities ( C D ) 4)
  (( y C ) C 3)
  (D D 10)
  (( x t y ) D 20)
  )

(table capture-chance
  (s cities ( 50 30 ))
  (w cities ( 40 15 ))
  )

(table protection
  (w cities 90)
  (s cities 50)
  )

;FIX ferry-on-entry is U1 U2 -> FTYPE how much terrain U2 crosses to board U1
(table ferry-on-entry add (s cities over-all))

(table consumption-per-attack add (u* fuel 1))
(table hit-by add (u* fuel 1))

(add D acp-to-detonate 1)

;; Death stars lose no hp in their detonation.

(table detonation-unit-range (D u* 8))

(table detonation-damage-at (D u* 0))

(table detonation-damage-adjacent (D u* 64))

(table detonation-terrain-range (D t* 8))

(table detonation-terrain-damage-chance
  (D t* 100)
  (D vacuum 0)
  )

(table terrain-damaged-type
  (t* vacuum 1)
  (vacuum vacuum 0)
  )

;FIX "defeats" ( s w ) destroy-message
;FIX "shoots down" ( x t y ) destroy-message
;FIX "blasts out of the sky" ( C ) destroy-message
;FIX "miraculously destroys" ( D ) destroy-message

;FIX "flattens" cities destroy-message

;;; Other actions.

(add u* acp-to-change-side 1)

;;; Backdrop economy.

(table base-production (cities fuel 20))

(table productivity (cities t* 100))

;;; Scoring.

(add makers point-value ( 10 5 25 ))

;;; Setup.

(add t* alt-percentile-min (  95  98  98  98  99   0   0 ))
(add t* alt-percentile-max (  98  99  99  99 100   0  95 ))
(add t* wet-percentile-min (   0  20  80   0   0   0   0 ))
(add t* wet-percentile-max ( 100  80 100  20 100   0 100 ))

;(set alt-smoothing 5)

(set edge-terrain vacuum)

(set country-radius-min 2)

(set country-separation-min 16)
(set country-separation-max 100)

;;; Of the 19 cells implied by radius of 2, require at least 12
;;; to be NON-vacuum.

(add vacuum country-terrain-max 7)

(add cities start-with (3 1))

(table independent-density (* land 100))

(table favored-terrain
  (u* t* 0)
  ;; Towns must be on land, preferably plains.
  (* land 40)
  (* plains 100)
  ;; Floating city can go anywhere except space.
  (@ t* 100)
  (@ vacuum 0)
  )

(set side-library '(
  ((name "Rebellion") (noun "Rebel"))
  ((name "Empire") (noun "Empire"))
  ))

(add cities namer "random-starwars-town-names")

(namer random-starwars-town-names (random
  "Darth Valley Junction"
  "Yodaville"
  "Neda"
  "Ya-ya"
  "Jabba"
  "Ewoktown"
  "Foo"
  "Bar"
  "Pits"
  "Endor"
  "Dry"
  "Tatooine"
  "Mjb"
  "Egm"
  "Star City"
  "Cape Canaveral"
  "Cold"
  "Rebel Yell"
  "Imperial"
  ))

;;; Documentation.

(game-module (notes (
"Originally designed as a test, and so we could fly Death Stars around.
Additional plausibility supplied by Alan Wexelblat <wex%sw.MCC.COM@MCC.COM>.

  Tweaked a little bit 1/27/92 by peterm@cory.berkeley.edu. 
   I increased fuel contained
  in towns and cities from 100.  I decreased accuracy of death beam from 100
  to 90.  I think that 100% hits doesn't agree well with the software.

  Also added a machine strategy to get the computer players to actually
  produce some of the tougher units, like C and D."
  ""
  "Tweaked a lot by Stan Shebs to make it work for 7.0."
  )))

(game-module (instructions (
  )))

(game-module (design-notes (
  )))
