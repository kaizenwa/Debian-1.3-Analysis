(game-module "postmodern"
  (blurb "An elaborated version of the standard game")
  (title "of surviving city-states struggling for control of the apocalypse's ruins...")
  (variants
   (world-seen false)
   (see-all false)
   )
  )

; possibly radar should get no moves - you have to embark it explicitly ?
; but then you can't disembark it.

;
(unit-type i (name "infantry") (char "i")
  (help "marches around and captures things"))
(unit-type a (name "armor") (char "a")
  (help "faster than infantry, limited to open terrain"))
(unit-type S (name "Special Forces") (char "S")
  (help "special infiltration units"))
(unit-type r (name "radar") (char "r")
  (help "small device that sees far"))
(unit-type A (name "airlifter") (char "A")
  (help "big cargo plane, for moving supplies and troops"))
(unit-type f (name "fighter") (char "f")
  (help "interceptor to get those nasty Aircraft"))
(unit-type b (name "bomber") (char "b")
  (help "long range aircraft, carries infantry and bombs"))
(unit-type d (name "destroyer") (char "d")
  (help "fast, cheap, and sinks subs"))
(unit-type s (name "submarine") (char "s")
  (help "sneaks around and sinks ships"))
(unit-type m (name "sea mines") (char "m")
  (help "sea mine, floats around, sinks shps"))
(unit-type t (name "transport ship") (char "t")
  (help "carries units and supplies across the water"))
(unit-type j (name "jeep") (char "j")
  (help "trucks and jeeps for moving infantry and supplies"))
(unit-type X (name "mech infantry") (char "X")
  (help "infantry in powered armor and AFVs"))
(unit-type C (name "carrier") (char "C")
  (help "carries aircraft around"))
(unit-type z (name "spy plane") (char "z")
  (help "fast, hard to see spy craft"))
(unit-type B (name "battleship") (char "B")
  (help "the most powerful ship"))
(unit-type G (name "missile") (char "G")
  (help "powerful explosive, Very accurate"))
(unit-type N (name "atomic") (char "N")
  (help "nuclear fuel unit, or nuclear bomb"))
(unit-type e (name "engineers") (char "e")
  (help "produces things, fights sieges"))
(unit-type O (name "bolo") (char "O")
  (help "huge CyberTank - a veritable land battleship"))
(unit-type L (name "land fortifications") (char "L")
  (help "Hold those enemies at bay!"))
(unit-type & (name "bridge") (char "&")
  (help "serves as something units can walk over"))
(unit-type / (name "base") (char "/")
  (help "airstrip plus port"))
(unit-type V (name "village") (char "V")
  (help "small town"))
(unit-type * (name "town") (char "*")
  (help "smaller than a city"))
(unit-type @ (name "city") (char "@")
  (help "capital of a side"))

(material-type fuel (help "basic motive power"))
(material-type ammo (help "generic hitting capability"))
(material-type people (help "population, used to stock cities, operate units"))

(terrain-type sea (char ".") (color "sky blue"))
(terrain-type shallows (char ",") (color "cyan"))
(terrain-type swamp (char "=") (color "yellowgreen"))
(terrain-type desert (char "~") (color "yellow"))
(terrain-type plains (char "+") (color "green"))
(terrain-type forest (char "%") (color "forest green"))
(terrain-type mountains (char "^") (color "sienna"))
(terrain-type ice (char "_") (color "white"))
(terrain-type vacuum (char ":") (color "black"))

(add i image-name "man")
(add a image-name "tank")
(add S image-name "elite")
(add r image-name "radar")
(add A image-name "airlift")
(add f image-name "jets")
(add b image-name "bomber")
(add d image-name "small-ship")
(add s image-name "sub")
(add m image-name "fortress")	; hey, that icon makes a good mine!
(add t image-name "cargo-ship")
(add j image-name "jeep")
(add X image-name "mech")
(add C image-name "carrier")
(add z image-name "spysat")
(add B image-name "battleship")
;"engineer" e icon-name
(add e image-name "tractor")
(add G image-name "missile")
(add N image-name "bomb")
(add O image-name "ogre")
(add L image-name "walltown")
(add & image-name "bridge")
(add / image-name "airbase")
(add V image-name "village")
(add * image-name "town20")
(add @ image-name "city20")

(define bases ( L & / V * @ ))
(define cities ( V * @ ))
(define makers ( V * @ ))
(define ground ( i a e S j X O ))
(define aircraft ( A f b G z ))
(define ships ( d s m t C B ))
(define movers ( i a S r A f b d s m t j X C z B G N e O ))
(define water ( sea shallows ))
(define land ( plains forest desert mountains ))

;;; Static relationships.

;; Unit-unit.

(table unit-capacity-x
  (u* r 1)	; all units can have radar
  (( S z ) r 0)	; radar gives away their position - they use passive
  (f G 4) ; fighters carry missiles
  (d ( m G ) ( 8 2 )) ; destroyers - sea mines and missiles
  (X ( G N ) ( 20 1 )) ; mech infantry - missiles and a nuke
  (G N 1) ; a missile can be a nuke carrier
  (A ( i a S j X m O e ) ( 2 1 2 3 1 10 1 1 )) ; airlifter - alot
  (a (i S) 1) ; armor - a single troop
  (b ( i S m j N r G )
     ( 1 1 6 1 2 1 4 )) ; bombers - troops, sea mines, missiles, nukes, jeeps, radar
  (s ( S N G )
     ( 1 2 4 ))
  (t ( i a S j X m  O G e )
     ( 6 3 8 8 3 12 1 6 2 ))
  (j ( i S X G e )
     ( 3 3 2 3 1 ))
  (C ( f  b m z G  N )
     ( 10 3 6 2 10 2 ))
  (B ( i S m G N ) 8)
  (G N 3)
  (O ( G N ) ( 6 3 ))
  (bases u* 40)
  (bases bases 0)
  )

(add u* capacity 0)
(add (i a S A b s t j C B O) capacity
     (1 1 1 4 2 2 6 3 8 1 1))
;                     L  &  /  V  *  @ "bases"
(add bases capacity ( 3  2  8 12 24 48 ))

(table unit-size-as-occupant
  (u* u* 99)
  ( (N G) u* (1 1) ) ; everything carries nukes and missiles
  (ground bases 1)
  ;              d s m t C B "ships"
  (ships bases ( 4 4 1 5 6 6 ))	; ships are BIG!
  ;                 A f b G z "aircraft"
  (aircraft bases ( 2 1 2 1 1 ))
  ( (i S N G) a (1 1 1 1) ) ; armor
  (( i a S j X m O e ) A (1 1 1 1 1 1 1 1)) ; airlifter
  )

(table occupant-max add (u* N 1) )
(table occupant-max add (u* G 2) )
(table occupant-max add (a ( i S ) 1))	; small groups of troops can ride on armor!
(table occupant-max add (S ( N G ) ( 1 2 )))
(table occupant-max add (A ( i a S j X m O e ) ( 2 1 2 2 1 10 1 1 ))) ; heavy airlifter indeed
(table occupant-max add (b ( i S m j N r G ) ( 1 1 6 1 2 1 4 )))
(table occupant-max add (s ( S N G ) ( 1 2 4 )))
(table occupant-max add (t ( i a S j X m O G e ) ( 6 3 8 8 3 12 1 6 2 )))
(table occupant-max add (j ( i S X G e ) ( 3 3 2 3 1 )))
(table occupant-max add (C ( f b m z G N ) ( 10 3 6 2 10 2 )))
(table occupant-max add (B ( i S m G N ) 8))
(table occupant-max add (G N 3))
(table occupant-max add (O ( G N ) ( 6 3 )))
(table occupant-max add (bases u* 40))
(table occupant-max add (bases bases 0))

;; Unit-terrain.

(add t* capacity 16)

(table unit-size-in-terrain
  (u* t* 1)
  (( * @ ) t* 12)
  )

;; Unit-material.

(table unit-storage-x
;                   L  &   /   V   *    @ "bases"
  (bases fuel   ( 100 50 100 300 500  500 ))
  (bases ammo   (  70 10  50  80 100  200 ))
  (bases people (   2 1   20 200 400  999 ))
;                  i  a  S r  A  f  b  d  s m  t  j  X   C  z   B  G  N  e   O "movers"
  (movers fuel   ( 6 10 20 2 40 18 36 99 99 5 99 50 30 400 48 200 20 10 30 180 ))
  (movers ammo   ( 6 10 20 2 40 18 36 99 99 2 99 50 30 400 48 200 20 10 30  80 ))
  (movers people ( 3  1  1 0  2  0  5  4  1 0 20 10  1   8  0   9  0  0 30   0 ))
  )

;;; Vision.

;FIXME visibility is U T -> N, U's % visibility in T
(table visibility add (( S m s z ) t* 0)) ; can't see these unless you step on them
(table visibility add (( G N ) t* 10)) ; missiles and nukes tough to see
; small forces hidden by rough terrain
(table visibility add ( (i X j) forest 40))
(table visibility add ( (i X j) swamp 30))
(table visibility add ( (i X j) mountains 50))

(add ( V * @ ) see-always 1)

;FIXME vision-at-max-range is N, coverage afforded by unit at max range
(add u* vision-range 1)
(add aircraft vision-range 2)
(add (@ * r z m C B) vision-range
     (3 2 6 4 0 3 3))
(add ( s z @ * C B ) vision-range 6)

;;; Actions.

;                          i a S r A f b d s m t j X C  z B  G N e O "movers"
(add movers acp-per-turn ( 1 2 2 1 5 9 6 3 3 1 3 3 2 4 12 4 10 2 1 3 ))

(add cities acp-per-turn 1)

;;; Movement.

;(add ( r m ) speed 0)

(add cities speed 0)

(table mp-to-enter-terrain
  (u* t* 99)
  (ships water 1)	; sea is pretty straightforward...
  (( s C B ) shallows (2 3 3))	; big ships don' like shallows
  (ground water 99)
  (( X O ) shallows 3)
  (aircraft t* 1)	; Aircraft ignore most terrain
  (u* vacuum 99)	; nothing here can stand vacuum
  (G vacuum 1)	;  well, missiles can...
  (bases t* 1)	; bases can go anywhere
  ; movement on terrain...
  (ships land 99)	; most ships can't go on land too!
  (( d t m ) swamp 1)	; but swamps are shallow enough for small ones
  ; ground units and land terrain:
  ;Ice... is special.. It's often water covered with ice.
  (ships ice 99)
  (s ice 1)	; subs can go under ice
  (m water 1); sea mines in the sea
  ; ground units ant how they handle terrain:
  ;               i  a e S  j X O  "ground"
  (ground swamp  (1 99 1 1 3 2 3))	; most ground units don't like swamp
  (ground desert (1 1 1 1 1 1 1))
  (ground plains (1 1 1 1 1 1 1))	; plains= big highway
  (ground forest (1 2 1 1 2 1 1))	;Vehicles must navigate/crush way
  (ground mountains (1 99 1 2 99 2 3))	; most ground units don't like ice
  (ground ice (99 99 1 2 3 2 2))	; most ground units don't like ice
  ;Other special moves:
  ;-1 t* r moves ; radar can only operate as a passenger
  (r t* 1)	;  radar techs can take radar anywhere
  (r water 1)	; except on water...
  (e t* 1)	;engineers can go anywhere... but vacuum
  (cities land 1)	; looks strange, but needed to define allowable places
  )

(table consumption-per-move add (movers fuel 1))	; all units take 1 fuel to move
(table consumption-per-move add (( C B O ) fuel 2))	; big units take more fuel

;100 u* A alter-mobility
;(table mp-per-occupant add (G N 90))

(table mp-to-enter-unit add (aircraft u* 20))	; aircraft can't sortie again until next turn
(table mp-to-enter-unit add (bases ships 1))
(table mp-to-leave-unit add (bases ships 1))

;FIXME ferry-on-entry is U1 U2 -> FTYPE how much terrain U2 crosses to board U1
(table ferry-on-entry add (u* u* over-all))	; lets everything attack everywhere
(table ferry-on-entry add (m u* over-all))	; mines can't attack across terrain
(table ferry-on-entry add (bases i over-all))	; infantry can capture cities even on water.

;;; Construction.

; big cities produce a tad faster, especially high tech stuff

;                i  a  S  r  A  f  b  d  s  m  t  j  X  C  z  B  G  N  e  O "movers"
(add movers cp ( 5  9 12  9 13 12 15 13 14  4 11  7 16 45 27 60  7 30 14 45 ))

(add bases cp ( 4 1 10 15 30 45 ))

(table acp-to-create
  (i L 1)
  (ground / 1)
  (O / 0)
  (e bases 1)
  (makers movers 1)
  (V O 0)
  )

(table cp-on-creation
  (i L 1)
  ;           i a e S j X O "ground"
  (ground / ( 7 5 8 1 7 8 0))
  (e bases 1)
  (makers movers 1)
  ;           i  a  S  r  A  f  b  d  s  m  t  j  X  C  z  B  G  N  e  O "movers"
  (* movers ( 2  3  3  4  4  4  6  3  3  2  2  3  5 16 10 21  2  1  3  1 ))
  (@ movers ( 2  3  4  4  4  5  6  4  4  2  3  3  7 19 16 25  3  3  5  8 )) 
  )

(table acp-to-build
  (i L 1)
  (ground / 1)
  (e bases 1)
  (makers movers 1)
  (V O 0)
  )

(table cp-per-build
  ;          L & / V * @ "bases"
  (e bases ( 2 1 3 1 1 1 ))
  ((* @) N 2)
  )

#|  ; leave this out for the moment
(table material-to-build
  (movers people (  4  2  1  0  0  0  0  1  1  0  1  1  2  3  0  3  3  0  3  0 ))
  ; people needed to operate units...
  (L ( ammo fuel people ) ( 3 3 1 ))	; fortification needs stockpiles!
  (bases people ( 0 0 0 200 400 800 ))	; bases need to be populated
  )
|#


(table acp-to-toolup
  (u* u* 1)
  )

(table tp-to-build
  (u* u* 0)
  ;; on the average, add about 20% build time for toolup
  ;           i  a  S  r  A  f  b  d  s  m  t  j  X  C  z  B  G  N  e  O "movers"
  (V movers ( 1  2  2  2  2  2  3  2  2  1  2  1  3  9  5 12  1  6  3  9 ))
  (* movers ( 1  2  2  2  2  2  3  2  2  1  2  1  3  9  5 12  1  6  3  9 ))
  (@ movers ( 1  2  2  2  2  2  3  2  2  1  2  1  3  9  5 12  1  6  3  9 ))
  )

(table tp-max
  (u* u* 0)
  ;; on the average, add about 20% build time for toolup
  ;           i  a  S  r  A  f  b  d  s  m  t  j  X  C  z  B  G  N  e  O "movers"
  (V movers ( 1  2  2  2  2  2  3  2  2  1  2  1  3  9  5 12  1  6  3  9 ))
  (* movers ( 1  2  2  2  2  2  3  2  2  1  2  1  3  9  5 12  1  6  3  9 ))
  (@ movers ( 1  2  2  2  2  2  3  2  2  1  2  1  3  9  5 12  1  6  3  9 ))
  )

; does all prod time calcs using integer arith.
;  i   a   S   r   A   f   b   d  s  m  t  j  X  C   z  B   G   N   e   O
;FIXME this is handled by tech level and research now...
;FIXME ( 50 100 160 400 100 120 100  60 88 50 58 50 80 12 100  5 360 375 192 60 ) movers research
;  template for research-contrib...
;  it is % of research on first unit(vector) that counts toward unit.
;() () r research-contrib
;FIXME ( 234 434 284 ) A ( f b z ) research-contrib
;FIXME ( 388 634 ) ( b r ) G research-contrib
;FIXME ( 300 50 100 80 ) ( a X B N ) O research-contrib
;FIXME ( 100 80 ) ( i X ) S research-contrib
;FIXME 60 z r research-contrib
;FIXME ( 60 120 50 50 ) ( f b z G ) A research-contrib
;FIXME ( 50 90 90 100 ) ( A b z G ) f research-contrib
;FIXME ( 80 90 60 30  ) ( A f z G ) b research-contrib
;FIXME ( 20 90 70 90  ) ( A f b G ) z research-contrib
;FIXME ( 40 40 60 50 50 ) ( s m t C B ) d research-contrib
;FIXME ( 40 40 10 20 20 ) ( s d t C B ) m research-contrib
;FIXME ( 20 50 20 60 60 ) ( s d m C B ) t research-contrib
;FIXME ( 50 10 40 60 10 80 40 ) ( f s d t m B N ) C research-contrib
;FIXME ( 60 10 80 50 60 10 40 ) ( O s d t C m N ) B research-contrib
;FIXME ( 50 80 ) ( i a ) j research-contrib
;FIXME ( 100 80 90 ) ( i a S ) X research-contrib
;FIXME ( 60 80 60 20 40 40 ) ( b m G O C B ) N research-contrib
;FIXME ( 80 10 ) ( i X ) e research-contrib

; no special materials to make

(table supply-on-completion (bases m* 100))	; we don't start with any materials

;;; Repair.

;FIXME hp-per-repair is U1 U2 -> .01HP that U1 restores to U2 per repair action
; ...and you have to add...
; acp-to-repair is U1 U2 -> ACP to do one repair action
(table acp-to-repair add (( / V * @ ) u* 1))
(table acp-to-repair add (( L & e ) u* 1))
(table acp-to-repair add (( C B O ) ( C B O ) 1))
(table acp-to-repair add (i bases 1))
(table acp-to-repair add (e u* 1))
(table acp-to-repair add (i ( L & / V ) 1))

(table hp-per-repair add (( / V * @ ) u* 100))
(table hp-per-repair add (( L & e ) u* 50))
(table hp-per-repair add (( C B O ) ( C B O ) 12))
(table hp-per-repair add (i bases 20))
(table hp-per-repair add (e u* 100))
(table hp-per-repair add (i ( L & / V ) ( 100 50 100 25 )))

;;; Production.

;;; Combat.

;                i a S r A f b d s m t j X C z B G N e  O L &  /  V  *  @
(add u* hp-max ( 1 2 2 1 2 1 3 3 2 1 3 1 3 4 1 8 1 1 1 12 3 2 10 12 20 40 ))

;FIXME in addition you can set hp-to-repair and other things to cripple a unit
;(add u* hp-at-min-speed ( 0 1 0 0 1 0 1 1 1 0 1 0 1 1 0 3 0 0 0  4  1  2  6  8 10 20 ))
; crippled is < test, not <=

; split the matrix in half to reduce the number of entries per line
(define u1 ( i  a  S  r  A  f  b  d  s  m  t  j  X ))
(define u2 ( C  z  B  G  N  e  O  L  &  /  V  *  @ ))

(table hit-chance
  ;         i   a   S   r   A   f   b  d  s  m  t  j  X "u1"
  (i u1 (  50  40  35  90  30  20  25 20 20 30 30 80 30 ))
  (a u1 (  60  50  50  85  35  25  25 25 25 25 35 85 45 ))
  (S u1 (  65  60  50  90  10   7   9 10 12 15 20 80 35 ))
  (r u* 0)	; radar don't hit anything
  (A u* 0)	; Transport plane isn't armed!
  (f u1 (  25  20  15  80  80  60  70 45 50 35 45 75 35 ))
  (b u1 (  20  30  15  80  30  10   9 55 50 35 60 70 40 ))
  (d u1 (   7   9   6  80  20  15  17 50 60 80 75 40 25 ))
  (s u1 (   9  10   7  80  30  10  10 50 50 60 75 20  6 ))
  (m u* 0)
  (m ships ( 40 50 80 70 75 75 ))
  (t u1 (   4   3   3  80  10  10  10 40 30 50 45 20  2 ))
  (j u1 (   1   0   5  80  00  00   0  0  0  0  0 50  0 ))
  (X u1 (  65  70  50  80  30  35  32 40 40 50 50 90 45 ))
  (C u1 (  30  20  15  80  60  50  55 40 40 30 55 30 15 ))
  ; z
  (B u1 (  50  50  45  80  55  40  45 65 60 40 70 80 40 ))
  (G u* 100)	; missiles hit everything, modifiers ineffective
  (N u* 100)	; nukes hit everything, modifiers ineffective
  (e u1 (   5   4   3  70  10   9  10 10 12 15  9 40  3 ))
  (O u1 (  80  70  65  90  60  40  35 60 80 40 60 90 60 ))
  (L u1 (  40  50  25  90  60  30  30 40 40 30 40 90 35 ))
  (& u* 0)	; bridges aren't armed!
  (/ u1 (  10  10  15  80  60  20  20 20 20 20 20 50  7 ))
  (V u1 (  25  15  30  80  70  45  35 25 20 20 30 80 12 ))
  (* u1 (  30  20  35  80  85  50  40 40 20 20 45 80 20 ))
  (@ u1 (  50  40  55  80  95  70  60 50 20 20 55 80 30 ))
  ; second half of matrix
  ;        C   z   B   G   N   e   O   L  &  /   V   *   @ "u2"
  (X u2 ( 65  70  50  80  30  35  32  40 40  50 50  90  45 ))
  (O u2 ( 80  20  65  90  60  60  45  80 85 80  70  65  60 ))
  (i u2 ( 20   1   9  40  40  50  10  60 40 80  70  60  40 ))
  (a u2 ( 20   1  20  50  50  60  30  40 50 90  80  70  50 ))
  (S u2 (  1   3   4   4  30  30  20  40 70 55  40  30  20 ))
  (A u2 (  5   3   0   0   0   0   3   1 10  5  10  25  30 ))
  (f u2 ( 50  50  40  80  80  65  30  48 60 70  80  80  80 ))
  (b u2 ( 70  10  60  50  50  20  40  70 60 90  90  90  94 ))
  (d u2 ( 40   1  20   0   0   5  10  15 50 99  90  90  80 ))
  (s u2 ( 40   1  50   0   0   0  10   0  6  0   0   0   0 ))
  ; m
  (t u2 ( 30   1   9   0   0  20   8   4 30  0   0   0   0 ))
  (j u2 (  1   0   5  80  00  00   0   0  0  0   0  50   0 ))
  ; X
  (C u2 ( 20  10  20   8   8  30   7   0  0  0   0   0   0 ))
  (B u2 ( 50  10  90   0   0  50  40  70 89 99  99  99  99 ))
  ; G
  ; N
  (e u2 (  0   1   3  20  21  30   8  80 70 80  70  60  40 ))
  (L u2 ( 20   5   9  40  40  50  60   0  0  0   0   0   0 ))
  (/ u2 ( 20  20  20   0   0  10  30   0  0  0   0   0   0 ))
  (V u2 ( 20  20  20   0   0  25  30   0  0  0   0   0   0 ))
  (* u2 ( 20  30  20   0   0  30  35   0  0  0   0   0   0 ))
  (@ u2 ( 20  40  50   0   0  50  40   0  0  0   0   0   0 ))
  ;; Missiles and nukes never survive their attack.
  (u* ( G N ) 100)
  )

(table defend-terrain-effect 
  (i ( forest swamp mountains ) 90)
  )

(table damage
  (u* u* 1)
  (a cities 2)
  (a ( a X O ) 2)
  (S bases 2)
  (b ships 2)
  (b s 1)
  (b ( / V * ) 2)
  (b @ 3)
  (b ground 2)
  (d s 2)
  (( s m ) ships 3)
  (B u* 2)
  (O u* 3)
  (O bases 4)
  (B ( V * @ ) ( 3 3 4 ))
  (G u* 4)
  (G ( V * @ ) ( 5 5 6 ))
  (N u* 60)
  (X u* 2)
  )

;FIXME true capturemoves

;FIXME true counterattack

(table acp-to-defend add (( G N m ) u* false))

(table capture-chance
  (S ( A b s t j C B N ) ( 20 15 15 26 22 4 4 18 ))	; elite special missions
  (i ( b t j C B ) ( 00 10 15 3 3 ))	; board and capture!
  ;           L  &  /  V  *  @ "bases"
  (S bases ( 10 70 60 30 20 10 ))
  (i bases (  5 30 70 55 50 30 ))
  (a bases (  6 40 90 75 70 50 ))
  (X bases (  8 90 90 75 70 60 ))
  )

(add ( G N m ) acp-to-detonate 1)

(table protection
  (cities movers 50)	; cities offer some protection to occupants
  (a cities 90)	; armor protect the cities housing them.
  ; can't make this too large or city can be
  ; invulnerable.
  (i cities 95)	; same for infantry.
  (a / 75)
  (i / 85)
  (L ( i a ) (75 90))
  )
; ???
(table protection add (bases ground 99))
(table protection add (L ( e i A X O S ) 98))

;FIXME U1 U2 -> % that U2 withdraws before U1 attacks
(table withdraw-chance-per-attack add (u* ground 10))
(table withdraw-chance-per-attack add (u* O 0))	; ogres don't retreat!
(table withdraw-chance-per-attack add (u* ships 5))
(table withdraw-chance-per-attack add (u* aircraft 25))

(table consumption-per-attack add (u* ammo 1))

(table hit-by (u* ammo 1))

;FIXME "destroys" ground destroy-message
;FIXME "sinks" ships destroy-message
;FIXME "shoots down" aircraft destroy-message
;FIXME "devastates" cities destroy-message

;100 u* max-quality ;  what's the range of this?
;100 u* veteran ;  what's the range of this?
;100 u* max-morale ;  what's the range of this?
;FIXME true u* neutral	;everything can become neutral

;;; Other actions.

(add u* acp-to-change-side 1)	; equipment is indifferent to its fate
(add ( i a e X O S ) acp-to-change-side 0)	; but armies have some loyalty
; Should N, e change sides? (gives away research)

;FIXME true u* neutral
;FIXME false i neutral

(add movers acp-to-disband 1)
(add ( L & / V ) acp-to-disband 1)

(add movers hp-per-disband 99)
(add ( L & / V ) hp-per-disband 99)

;100 u* efficiency ; so you can reclaim materials used to make disbanded units
; (should calc from to-build etc)

;;; Backdrop economy.

(table base-production
  (( i S m ) fuel ( 2 2 1 ))
  (N fuel 20)	; nuclear plant powers things
  (e ( fuel ammo ) ( 2 1 ))
  ;                L  &  /  V  *  @ "bases"
  (bases fuel   (  4  1 10 15 20 50 ))
  (bases ammo   (  0  1  5  8 10 20 ))
  (bases people (  0  0  0  2  4  8 ))
  )

; terrain effects on material production
(table productivity
  (u* t* 100)	;the default anyway!
  (i ( sea desert mountains ice ) 0)
  (i shallows 50)
  (a plains 100)	; in this case "plains" = "gas stations"
  ; plains/forest/desert/mountains
  (L land ( 100 90 70 50 ))	; one arg must be a scalar...
  (/ land ( 100 90 70 50 ))
  (V land ( 100 90 70 50 ))
  (* land ( 100 90 70 50 ))
  (@ land ( 100 90 70 50 ))
  (bases water 100)	; Cities in water have improved transport!
  (m water 100)
  (N t* 100)	; Nuke power is independant!
  (cities t* 100)
  )

(table base-consumption 
  (movers fuel 1)	; all consume 1 fuel, except...
  (L ammo 2)
  (( r m ) fuel 0)  ; these just sit quietly
  (i fuel 1)
  (aircraft fuel 2)	; aircraft need lotsa fuel to stay aloft
  )

; consume-as-occupant is strange.  1 means they don't consume!
(table consumption-as-occupant add (u* m* 1))
(table consumption-as-occupant add (( r X S C B O ) m* 0))
(table consumption-as-occupant add (aircraft m* 1))
(table consumption-as-occupant add (( a m j O N ) m* 1))

; inlength is how far away a unit can receive fuel from
; out-length is how far away a unit can deliver fuel.  Should be kept
; small, as it can waste much CPU time

(table out-length
  (u* m* 0)
  (bases m* 4)
  (bases people 2)	;  they will look for nearby places to expand
  ; bases form good supply lines, so do transport units and big ships
  (( A t j C B L & / V * @ ) fuel ( 1 1 1 1 1 2 1 2 2 3 4 ))
  (( A t j C B L & / V * @ ) ammo ( 1 1 1 1 1 2 1 2 2 3 4 ))
  )

(table in-length
  (u* fuel 1)	;  all units can be resupplied by adjacent units
  (u* ammo 1)
  (u* people -1)	; People don't really want to migrate in war
  (bases people 4)	; except to cities, of course.
  (e m* 2)	;  They follow the engineers for jobs!
  (aircraft fuel 0)	; no mid-air refueling yet.  can't make it selective
  (( L & / V * @ ) fuel ( 2 2 3 4 4 4 ))
  (( L & / V * @ ) ammo ( 2 2 3 4 4 4 ))
  )

;;; Random events.

; accidents
;FIXME surrender is U1 U2 -> .01N chance that U1 surrenders to U2
; and surrender-range is distance at which it can occur
;(table surrender-chance add (( V * @ ) u* ( 6 3 1 )))
;( 60 50 30 ) ( V * @ ) siege
;FIXME 190 bases siege	; They have too much supply needs to withstand siege for long


;FIXME attrition is U T -> .01HP (rate of loss, hp/turn)
(table attrition add (s ( sea shallows ice ) ( 1 5 8 )))	; chance of attrition for the unit
(table attrition add (t ( sea shallows ice ) ( 1 5 8 )))	; chance of attrition for the unit
;FIXME "disabled at sea" ( s t ) attrition-message


(table attrition add (( C B ) shallows 10))
;FIXME "runs aground" ( C B ) attrition-message
;FIXME accident is U T -> .01N chance that an accident happens to U
; and you can set accident-damage
(table accident-hit-chance add (( A f b ) mountains ( 9 4 5 )))	; flyers running into mountains
;FIXME "Crashes in the mountains" ( A f b ) accident-message
(table accident-hit-chance add (( A f b ) mountains ( 9 4 5 )))	; flyers running into mountains
;FIXME "Crashes in the mountains" ( A f b ) accident-message
(table accident-hit-chance add (m water 2))	; mines disappearing at sea
;FIXME "Drifts away" m accident-message
(table attrition add (O shallows 10))
;FIXME "Gets stuck in mud" O attrition-message

;FIXME spy-chance is now on a per unit basis...
; and spy-range specifies how far a unit can spy
(add u* spy-chance false)	; we don't spontaneously see all the enemies
;FIXME spy-quality is U1 U2 -> % that U1 returns info about U2
(table spy-quality (u* u* false))	; and we don't see any of them if we do!

;FIXME I guess nuked is damaged-terrain now, T1 T2 -> N
;FIXME t* t* nuked	; most terrain won't actually change
;FIXME desert ( plains forest ) nuked
;FIXME mountains ice nuked
;FIXME swamp shallows nuked

;;; Scoring.

(scorekeeper (do last-side-wins))

(add ( & / V * @ ) point-value ( 100 200 800 2000 5000 ))	;  territory values for victory pts.

;;; Random setup.

(set synthesis-methods
  '(make-fractal-percentile-terrain make-countries make-independent-units
    make-initial-materials name-units-randomly))

; storm with alt:0-33 wet:50-100 seemed a bit too much
(add t* alt-percentile-min (   0  68  69  70  70  70  93  97  0 ))
(add t* alt-percentile-max (  68  69  71  93  93  93  97 100  0 ))
(add t* wet-percentile-min (   0   0  50   0  20  80   0   0  0 ))
(add t* wet-percentile-max ( 100 100 100  20  80 100 100 100  0 ))
;(set alt-blob-density 5000)
;(set alt-blob-size 100)
;(set wet-blob-density 500)
;(set wet-blob-size 100)

(set edge-terrain ice)

(set country-radius-min 8)	;  the radius of a 'country'
(set country-separation-min 10)	;  the closest a country may be
(set country-separation-max 30)	;  the furthest apart a country may be.

(add ( V * @ ) start-with ( 1 2 1 ))	; units player starts with
(add ( V * @ ) independent-near-start (1 1 1))

(table independent-density (( @ * V ) land ( 50 100 60 )))

(table favored-terrain
  (u* t* 0)
  (( V * @ ) land 20)
  (( V * @ ) plains 40)
  (e t* 10)	; engineers can go anywhere!
  (@ plains 100)
  )

(add u* initial-seen-radius 7)

(include "nat-names")

(add cities namer "random-town-names")

(include "town-names")

; should add naming for C and B

;;; Documentation.

(game-module (instructions (
  )))

(game-module (notes (
  )))

(game-module (design-notes (
  )))

; Hand this to a Lisp system, use for calculations...
; (defun res (res prod)
;	(setq res (- res (floor (* prod .2))))
;	(ceiling (* (/ res (floor (* prod 1.2))) 100)))

; (defun restime (prod res) (floor (/ (* (floor (* prod 1.2)) (+ res 100)) 100)))
