(game-module "wizard"
  (blurb "Wizard dominated game")
)

;(game-module (title "when the gods were young"))

(unit-type w (name "wagon") (char "w") (image-name "wiz-wagon")
  (help "moves supplies"))
(unit-type s (name "ship") (char "s") (image-name "wiz-ship")
  (help "transports units over water"))
(unit-type g (name "grog") (char "g") (image-name "wiz-grog")
  (help "marches around and captures things"))
(unit-type k (name "knight") (char "k") (image-name "wiz-knight")
  (help "rides around and captures things better"))
(unit-type d (name "demon") (char "d") (image-name "wiz-demon")
  (help "kills and captures even better"))
(unit-type f (name "fire") (char "f") (image-name "wiz-fire")
  (help "burns things up"))
(unit-type e (name "earth") (char "e") (image-name "wiz-earth")
  (help "very strong, hard to kill"))
(unit-type a (name "air") (char "a") (image-name "wiz-air")
  (help "carries things through the sky"))
(unit-type c (name "crystal") (char "c") (image-name "wiz-crystal")
  (help "sees very far"))
(unit-type m (name "manicon") (char "m") (image-name "wiz-manicon")
  (help "concentrated mana ball"))
(unit-type W (name "wizard") (char "W") (image-name "wiz-wizard")
  (help "makes magic"))
(unit-type * (name "town") (char "*") (image-name "wiz-town")
  (help "produces food and grogs only"))
(unit-type @ (name "city") (char "@") (image-name "wiz-city")
  (help "produces all things except mana"))
(unit-type ! (name "guild") (char "!") (image-name "wiz-guild")
  (help "produces mana and wizards"))
(unit-type $ (name "outpost") (char "$") (image-name "wiz-outpost")
  (help "supplies mundanes"))

(material-type food (help "what people eat"))
(material-type mana (help "magic energy"))
(material-type drink (help "what people drink"))

(terrain-type sea (char ".") (color "sky blue"))
(terrain-type shallows (char ",") (color "cyan"))
(terrain-type swamp (char "=") (color "yellowgreen"))
(terrain-type desert (char "~") (color "yellow"))
(terrain-type plains (char "+") (color "green"))
(terrain-type forest (char "%") (color "forest green"))
(terrain-type mountains (char "^") (color "sienna"))
(terrain-type ice (char "_") (color "white"))
(terrain-type vacuum (char ":") (color "black"))

;FIXME I guess nuked is damaged-terrain now, T1 T2 -> N
;FIXME t* t* nuked	; most terrain won't actually change
;FIXME desert ( plains forest ) nuked
;FIXME mountains ice nuked

(define water ( sea shallows ))
(define land ( plains forest desert mountains ))
(define marsh swamp)

(define cities ( ! * @ ))
(define mundanes ( w s g k ))
(define wizards ( W ))
(define ships s)
(define flyers ( a m ))
(define ground ( w g k d f e c W ))
(define movers ( w s g k d f e a c m W ))
(define spells ( d f e a c m ))
(define stupid ( w g m * ))
(define smart ( k s d f e a c W @ ! ))
(define slow ( w g c ))
(define swift ( k d f e a W ))

;FIXME spy-quality is U1 U2 -> % that U1 returns info about U2
(table spy-quality (u* u* 10))
;    w  s  g  k  d  f  e  a  c  m  W  *  @  !  $
;  ( 10 10 10 5  10 1  1  1  1  1  5  50 40 15 15 ) u* revolt
;  ( 30 20 20 10 5  1  1  1  1  1  15 99 70 20 30 ) u* surrender
;  ( 30 20 20 10 1  1  1  1  1  1  20 99 99 15 20 ) u* siege
; 300 sea ships disaster
; "is lost in a storm" s disaster-message
; 100 t* a disaster
; "is lost in a storm" a disaster-message

; no disasters

;;; Static relationships.

;; Unit-unit.

;FIXME unit-size-as-occupant is U1 U2 -> N U1's size as occupant of U2
(table unit-size-as-occupant add (u* u* 1))
(table unit-size-as-occupant add (e u* 3))
;(table mp-per-occupant add (ships e 50))
;FIXME occupant-max is U1 U2 -> N upper limit on occupants by type
(table occupant-max add (e movers 1))
;FIXME capacity is N, upper limit on total occupants by size
(add movers capacity 1)
(add ( * @ ! $ ) capacity 30)
(add s capacity 3)
(table occupant-max add (s movers 3))
(table occupant-max add (a spells 1))
(table occupant-max add (a e 0))
(table occupant-max add (a wizard 1))
(table occupant-max add (spells m 1))
(table occupant-max add (wizards spells 1))
;(table mp-per-occupant add (wizards ( d f e ) 0))
(table occupant-max add (u* c 1))
(table occupant-max add ($ u* 5))
(table occupant-max add (( * ! ) u* 10))
(table occupant-max add (@ u* 20))
(table occupant-max add (w ( g k ) 1))
(table occupant-max add (( ! $ @ * ) ( ! $ * @ ) 0))

;; Unit-terrain.

;; Unit-material.

(table unit-storage-x
  (wizards mana 25)
  (! mana 150)
  (spells mana 10)
  (m mana 25)
  (@ food 150)
  (( * $ ) food 40)
  (mundanes food 10)
  (w food 20)
  (mundanes drink 3)
  (w drink 5)
  (ships drink 4)
  (cities drink 10)
  ($ drink 10)
  (! food 10)
  )

(table productivity add (@ land ( 100 50 20 20 )))	; one arg must be a scalar...
(table productivity add (* land ( 100 50 20 20 )))
(table productivity add (( $ ! ) land 100))
(table productivity add (ships ( shallows marsh ) 100))
(table productivity add (mundanes land 100))
(table productivity add (mundanes desert 20))

(table base-production add (mundanes drink 2))
(table base-production add (( $ ! * @ ) drink 10))
(table base-production add (( @ * ) food 10))
(table base-production add ($ food 4))
(table base-production add (! mana 10))

(table base-consumption add (mundanes food 1))
(table base-consumption add (spells mana 1))
(table base-consumption add (mundanes drink 1))
(table base-consumption add (wizards mana 1))

(table out-length add (u* m* 1))
(table in-length add (u* m* 1))
(table out-length add (( ! @ $ * ) m* 2))

;;; Actions.

;FIXME don't forget to add acp-per-turn for non-movers!
;                          w s g k d f e  a c m W
(add movers acp-per-turn ( 1 6 1 2 3 4 1 10 3 8 3 ))

;;; Movement.

;FIXME moves is mp-to-{enter|leave}-terrain U T -> MP
(table mp-to-enter-terrain add (slow land 1))
(table mp-to-enter-terrain add (g marsh 1))
(table mp-to-enter-terrain add (e marsh 2))
(table mp-to-enter-terrain add (swift ( forest mountains marsh ) 2))
(table mp-to-enter-terrain add (swift ( plains desert ) 1))
(table mp-to-enter-terrain add (ships land 99))
(table mp-to-enter-terrain add (ships water 1))
(table mp-to-enter-terrain add (ships shallows 3))
(table mp-to-enter-terrain add (ships marsh 5))
(table mp-to-enter-terrain add ($ land 1))
(table mp-to-enter-terrain add (flyers t* 1))

;;; Construction.

;FIXME make is replaced by acp-to-create, cp-per-build and friends
;FIXME ( 4 12 4 9 ) mundanes @ make
;FIXME ( 7 20 4 15 ) mundanes * make
;FIXME 12 wizards ! make
;FIXME 4 spells wizards make
;FIXME occupant-base-production is U M -> N of M that U produces as occupant
(table occupant-base-production (u* m* 1))
;FIXME 4 $ w make
(table material-to-build add (spells mana 4))

;FIXME tp-to-build is U1 U2 -> TP that U1 needs before building U2
; you also need UnitProperty acp-to-toolup and Table tp-per-toolup
(table tp-to-build add (u* u* 20))

;FIXME this is handled by tech level and research now...
;FIXME 50 mundanes research
;FIXME 200 wizards research
;FIXME 100 spells research
;FIXME 100 m research
;FIXME 50 $ research

;FIXME hp-per-repair is U1 U2 -> .01HP that U1 restores to U2 per repair action
; ...and you have to add...
; acp-to-repair is U1 U2 -> ACP to do one repair action
(table acp-to-repair add (u* u* 1))
(table hp-per-repair add (u* u* 14))

;;; Combat.

(add u* hp-max ( 1 2 1 1 3 1 5 1 1 1 2 5 10 15 8 ))
;FIXME in addition you can set hp-to-repair and other things to cripple a unit
;(add u* hp-at-min-speed ( 0 1 0 0 1 0 2 0 0 0 1 2 3  5 4 ))

; w  s  g  k  d  f  e  a  c  m  W  *  @  !  $
(table hit-chance add (w u* ( 50 20 10 10 5  5  5  5  20 70 30 30 10 5  10 )))
(table hit-chance add (s u* ( 40 50 30 20 15 20 5  20 30 70 30 35 25 10 25 )))
(table hit-chance add (g u* ( 70 60 50 30 20 30 10 30 40 80 40 50 40 25 40 )))
(table hit-chance add (k u* ( 80 70 70 50 35 50 20 50 50 85 50 65 50 35 50 )))
(table hit-chance add (d u* ( 85 80 75 65 50 60 30 65 70 90 25 70 60 50 60 )))
(table hit-chance add (f u* ( 90 90 85 70 60 70 40 70 85 80 35 75 70 50 70 )))
(table hit-chance add (e u* ( 80 70 70 50 35 40 20 50 85 80 15 70 70 70 70 )))
(table hit-chance add (a u* ( 50 80 50 40 35 40 20 50 45 75 20 40 35 30 35 )))
(table hit-chance add (c u* ( 20 15 10 5  5  5  5  5  20 50 15 20 10 5  10 )))
(table hit-chance add (m u* ( 90 90 80 70 65 70 60 75 80 70 65 90 80 60 80 )))
(table hit-chance add (W u* ( 90 90 75 65 60 65 40 65 80 80 50 70 60 50 60 )))
(table hit-chance add (* u* ( 50 20 10 10 5  5  5  5  20 70 30 30 10 5  10 )))
(table hit-chance add (@ u* ( 70 60 50 30 20 30 10 30 40 80 40 50 40 25 40 )))
(table hit-chance add (! u* ( 90 90 75 65 60 65 40 65 80 80 50 70 60 50 60 )))
(table hit-chance add ($ u* ( 70 60 50 30 20 30 10 30 40 80 40 50 40 24 40 )))

(table damage add (u* u* 1))
(table damage add (d u* 2))
(table damage add (f u* 4))
(table damage add (f ( e ! $ ) 1))
(table damage add (e ( @ ! $ * ) 3))
(table damage add (m u* 8))
(table damage add (a ships 3))
(table consumption-per-attack add (m mana 20))

;(add m acp-to-detonate 0)

(add m acp-to-detonate 1)

(table capture-chance add (g ( * @ ! $ ) ( 50 30 10 30 )))
(table capture-chance add (k ( * @ ! $ ) ( 70 50 15 50 )))
(table capture-chance add (d ( * @ ! $ ) ( 70 50 25 50 )))
(table capture-chance add (wizards spells 20))
(table capture-chance add (W ( * @ ! $ ) ( 70 50 50 50 )))

;FIXME ferry-on-entry is U1 U2 -> FTYPE how much terrain U2 crosses to board U1
(table ferry-on-entry add (( * @ ! $ ) ( g k d W ) over-all))	; infantry can capture cities even on water.

(table protection add (* movers 80))
(table protection add (( @ ! ) movers 50))
(table protection add (! movers 20))	; cities offer some protection to occupants
(table protection add (k ( * @ ! $ ) 90))	; armor protect the cities housing them.
(table protection add (W ! 90))	; can't make this too large or city can be
(table protection add (( d f e ) ! 93))	; invulnerable.
(table protection add (g ( * @ ! $ ) 95))	; same for infantry.
(table protection add (e movers 50))

(add u* acp-to-change-side 0)	; but armies have some loyalty
(add ( c a m ) acp-to-change-side ( 80 65 40 ))
(add ( * @ ! $ ) acp-to-change-side 100)
;FIXME true u* neutral
;FIXME false spells neutral
;FIXME true ( c m ) neutral
;FIXME false capturemoves
(table consumption-as-occupant add (u* m* true))
;FIXME survival is now hp-per-starve U M -> HP
(table hp-per-starve add (u* m* 30))

; 10 u* max-quality
; 2 u* veteran
; ( 90 95 90 95 90 90 90 90 90 90 95 85 90 95 90 ) u* control

;FIXME "vanquishes" movers destroy-message
;FIXME "sacks" cities destroy-message

;FIXME hp-per-disband is U1 U2 -> HP lost in a disband action performed by U2
; you might add U1 acp-to-disband U2 as well, U1 U2 -> ACP
; (table hp-per-disband add (movers u* 100))

;;; Vision.

;FIXME vision-at is N, coverage afforded by unit in its own hex
;(add u* vision-at-max-range ( 40 60 50 70 80 70 50 70 90 30 85 40 70 85 70 ))
;(add u* vision-at ( 1 2 1 1 1 1 1 2 10 1 2 1 2 3 ))
;FIXME vision-at-max-range is N, coverage afforded by unit at max range
(add u* vision-range ( 30 40 40 55 70 60 40 60 75 20 75 30 60 75 70 ))
;FIXME visibility is U T -> N, U's % visibility in T
;(table visibility add (wizards t* 40))
;FIXME conceal is now visibility, you should subtr these numbers from that table
;FIXME 15 ( forest mountains ) u* conceal

;;; Setup.

(add t* alt-percentile-min (   0  68  69  70  70  70  93  99  0 ))
(add t* alt-percentile-max (  68  69  71  93  93  93  99 100  0 ))
(add t* wet-percentile-min (   0   0  50   0  20  80   0   0  0 ))
(add t* wet-percentile-max ( 100 100 100  20  80 100 100 100  0 ))

(set edge-terrain ice)

(add @ start-with 1)
(add ! start-with 1)
(add * start-with 5)
(add W start-with 1)

(table favored-terrain
  (u* t* 0)
  (* land 20)
  (* plains 40)
  (@ plains 100)
  (! land 20)
  (! mountains 40)
  )

(set country-separation-min 10)
(set country-separation-max 30)

(table independent-density
  (* land 50)
  (* plains 150)
  (@ land 20)
  (@ plains 50)
  (! land 20)
  (! plains 50)
  )

;;; Scoring.

(add ( * @ ! $ ) point-value ( 1 5 10 1 ))

(scorekeeper (do last-side-wins))

;;; Documentation.

(game-module (instructions (
  )))

(game-module (notes (
  )))

(game-module (design-notes (
  )))
