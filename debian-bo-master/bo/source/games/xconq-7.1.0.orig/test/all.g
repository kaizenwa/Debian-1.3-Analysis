;;; This is a test game that mentions *everything*.
;;;
;;; Comments and symbols or strings beginning with xxx
;;; will be filtered out when comparing with the set of
;;; defined symbols.

(game-module "all"
  (base-game "")
  (base-module "")
  (blurb "xxxyyy")
  (default-base-module "")
  (design-notes "")
  (instructions "")
  (notes "")
  (picture-name "xxxzzz")
  (program-version "")
  (title "xxx")
  (variants
   (real-time)
   (see-all)
   (sequential)
   (world-seen)
   (world-size)
   )
  (version "")
  )

(unit-type xxxomnibus
  (start-with 1)
  (acp-damage-effect nil)
  (acp-max 0)
  (acp-min 0)
  (acp-per-turn 0)
  (acp-per-turn-max 0)
  (acp-per-turn-min 0)
  (acp-season-effect nil)
  (acp-temperature-effect nil)
  (acp-to-change-side 0)
  (acp-to-detonate 0)
  (acp-to-disband 0)
  (acp-to-fire 0)
  (acp-to-move 0)
  (acp-to-transfer-part 0)
  (action-priority 0)
  (already-seen 0)
  (already-seen-independent 0)
  (assign-number 0)
  (available 1)
  (can-be-self 0)
  (capacity 0)
  (char "")
  (color "")
  (consumption-temperature-effect nil)
  (country-units-max 0)
  (cp 1)
  (cp-per-self-build 0)
  (cp-to-self-build 0)
  (cxp-max 0)
  (cxp-on-capture-effect 0)
  (description-format nil)
  (detonate-on-death 0)
  (direct-control 1)
  (elevation-at-max-range 0)
  (extensions nil)
  (free-acp 0)
  (free-mp 0)
  (generic-name "")
  (has-opinions 0)
  (help "")
  (hit-falloff-range 0)
  (hp-max 1)
  (hp-per-disband 0)
  (hp-per-detonation 0)
  (hp-recovery 0)
  (image-name "")
  (independent-growth-chance 0)
  (independent-near-start 0)
  (independent-takeover-chance 0)
  (initial-seen-radius 0)
  (long-name "")
  (move-range 0)
  (mp-to-leave-world 0)
  (namer "")
  (name "")
  (name-internal "")
  (notes nil)
  (occupant-total-max 0)
  (point-value 0)
  (parts-max 1)
  (possible-sides nil)
  (range 0)
  (range-min 0)
  (revolt-chance 0)
  (see-always 0)
  (see-occupants 0)
  (self-changeable 0)
  (self-resurrects 0)
  (short-name "")
  (speed 0)
  (speed-max 0)
  (speed-damage-effect nil)
  (speed-min 0)
  (speed-wind-effect nil)
  (spot-action 1)
  (spur-chance 0)
  (spur-range 0)
  (spy-chance 0)
  (spy-range 0)
  (stack-order 0)
  (start-with 0)
  (tech-from-ownership 0)
  (tech-leakage 0)
  (tech-max 0)
  (tech-per-turn-max 0)
  (tech-to-build 0)
  (tech-to-own 0)
  (tech-to-see 0)
  (tech-to-use 0)
  (temperature-attrition nil)
  (type-in-game-max 0)
  (type-per-side-max 0)
  (unit-growth-chance 0)
  (unit-takeover-chance 0)
  (vision-range 0)
  (vision-bend 0)
  (wrecked-type 0)
  (zz-b 0)
  (zz-bb 0)
  (zz-transport 0)
  (zz-c 0)
  (zz-cm 0)
  (zz-cc 0)
  (zz-bw 0)
  )
(unit-type xxxship)
(unit-type xxxcity
  (start-with 3))
(unit-type xxxF-15)
(unit-type xxxsatellite)

(terrain-type xxxroad
  (alt-percentile-max 0)
  (alt-percentile-min 0)
  (available 1)
  (capacity 0)
  (char "")
  (clouds-max 0)
  (clouds-min 0)
  (color "")
  (country-growth-chance 0)
  (country-people-chance 0)
  (country-takeover-chance 0)
  (country-terrain-max 0)
  (country-terrain-min 0)
  (description-format nil)
  (elevation-max 0)
  (elevation-min 0)
  (extensions nil)
  (help "")
  (image-name "")
  (independent-people-chance 0)
  (liquid 0)
  (maze-passage-occurrence 0)
  (maze-room-occurrence 0)
  (name "")
  (notes nil)
  (occurrence 0)
  (people-max 0)
  (river-chance 0)
  (subtype 0)
  (subtype-x 0)
  (temperature-average 0)
  (temperature-max 0)
  (temperature-min 0)
  (temperature-variability 0)
  (thickness 0)
  (wet-percentile-max 0)
  (wet-percentile-min 0)
  (wind-force-average 0)
  (wind-force-max 0)
  (wind-force-min 0)
  (wind-force-variability 0)
  (wind-variability 0)
  (zz-fr 0)
  (color "xxxgray")
  )
(terrain-type |xxxdeep xxxsea|
  (color "xxxnavy xxxblue"))
(terrain-type xxxshelf
  (color "xxxlight xxxblue"))
(terrain-type xxxshallows
  (color "xxxcyan"))
(terrain-type xxxriver
  (color "xxxblue"))

(material-type xxxmaterial
  (available 1)
  (color "")
  (description-format nil)
  (extensions nil)
  (help "")
  (image-name "")
  (name "")
  (notes nil)
  (people 0)
  )

;(print false)
;(print true)

;;; Variables.

(set action-messages nil)
(set advantage-default 1)
(set advantage-max 3)
(set advantage-min 1)
(set alt-blob-density 0)
(set alt-blob-height 0)
(set alt-blob-size 0)
(set alt-smoothing 0)
(set calendar nil)
(set country-radius-max 0)
(set country-radius-min 0)
(set country-separation-max 0)
(set country-separation-min 0)
(set edge-terrain 0)
(set elapsed-real-time 0)
(set event-messages nil)
(set extra-turn-chance 0)
(set feature-namers nil)
(set feature-types nil)
(set grid-color "")
(set growth-stop-chance 0)
(set initial-date "")
(set initial-day-part 0)
(set initial-year-part 0)
(set last-turn 100)
(set maze-passage-density 0)
(set maze-room-density 0)
;(set player-mix-default nil)
;(set player-mix-required nil)
(set player-sides-locked 0)
(set random-events nil)
(set random-state 0)
(set real-time-for-game 0)
(set real-time-per-side 0)
(set real-time-per-turn 0)
(set river-sink-terrain 0)
(set scorefile-name "")
(set season-names nil)
(set see-all 0)
(set see-terrain-always 0)
(set see-weather-always 0)
(set self-required 0)
(set side-library nil)
(set sides-max 9)
(set sides-min 1)
(set synthesis-methods nil)
(set temperature-floor 0)
(set temperature-floor-elevation 0)
(set temperature-moderation-range 0)
(set temperature-year-cycle 0)
(set terrain-seen 0)
(set turn 0)
(set units-in-game-max 100)
(set units-per-side-max 100)
(set unseen-char "")
(set unseen-color "")
(set unseen-image-name "")
(set use-side-priority 0)
(set wet-blob-density 0)
(set wet-blob-height 0)
(set wet-blob-size 0)
(set wet-smoothing 0)
(set wind-mix-range 0)

;;; Tables.

(table accident-damage)
(table accident-hit-chance)
(table accident-vanish-chance)
(table acp-night-effect)
(table acp-for-retreat)
(table acp-occupant-effect)
(table acp-to-add-terrain)
(table acp-to-attack)
(table acp-to-be-fired-on)
(table acp-to-build)
(table acp-to-capture)
(table acp-to-change-type)
(table acp-to-create)
(table acp-to-defend)
(table acp-to-enter-unit)
(table acp-to-load)
(table acp-to-remove-terrain)
(table acp-to-produce)
(table acp-to-repair)
(table acp-to-research)
(table acp-to-toolup)
(table acp-to-unload)
(table adjacent-terrain-effect)
(table alter-terrain-range)
(table altitude-max)
(table altitude-min)
(table attack-range)
(table attack-range-min)
(table attack-terrain-effect)
(table attrition)
(table base-consumption)
(table base-production)
(table build-range)
(table can-enter-independent)
(table capture-chance)
(table change-on-exhaustion-chance)
(table coating-depth-max)
(table coating-depth-min)
(table consumption-as-occupant)
(table consumption-on-creation)
(table consumption-per-attack)
(table consumption-per-build)
(table consumption-per-move)
(table consumption-per-repair)
(table control-chance)
(table control-chance-at)
(table control-chance-adjacent)
(table control-range)
(table cp-on-creation)
(table cp-per-build)
(table create-range)
(table cxp-per-capture)
(table cxp-per-combat)
(table damage)
(table damage-cxp-effect)
(table defend-terrain-effect)
(table detonate-on-approach-range)
(table detonate-on-capture)
(table detonate-on-hit)
(table detonation-accident-chance)
(table detonation-damage-at)
(table detonation-damage-adjacent)
(table detonation-terrain-damage-chance)
(table detonation-terrain-range)
(table detonation-unit-range)
(table eye-height)
(table favored-terrain)
(table ferry-on-entry)
(table ferry-on-departure)
(table fire-damage)
(table fire-hit-chance)
(table hp-min)
(table hp-to-garrison)
(table hit-at-max-range-effect)
(table hit-by)
(table hit-chance)
(table hit-cxp-effect)
(table hp-per-repair)
(table hp-to-repair)
(table hp-per-starve)
(table in-length)
(table independent-density)
(table independent-capture-chance)
(table load-max)
(table material-per-production)
(table material-to-act)
(table material-to-build)
(table material-to-change-type)
(table material-to-create)
(table material-to-fight)
(table material-to-move)
(table material-to-produce)
(table material-to-repair)
(table mp-to-enter-terrain)
(table mp-to-enter-unit)
(table mp-to-enter-zoc)
(table mp-to-leave-terrain)
(table mp-to-leave-unit)
(table mp-to-leave-zoc)
(table mp-to-traverse)
(table mp-to-traverse-zoc)
(table occupant-can-have-occupants)
(table occupant-combat)
(table occupant-can-construct)
(table occupant-escape-chance)
(table occupant-base-production)
(table occupant-vision)
(table occupant-max)
(table out-length)
(table people-consumption)
(table people-production)
(table people-see-chance)
(table people-surrender-chance)
(table people-surrender-effect)
(table productivity)
(table productivity-max)
(table productivity-min)
(table protection)
(table recycleable-material)
(table retreat-chance)
(table road-chance)
(table road-into-chance)
(table scuttle-chance)
(table see-chance)
(table see-chance-adjacent)
(table see-chance-at)
(table speed-occupant-effect)
(table spy-quality)
(table stack-protection)
(table supply-on-creation)
(table supply-on-completion)
(table supply-per-disband)
(table surrender-chance)
(table surrender-chance-per-attack)
(table surrender-range)
(table tech-crossover)
(table tech-per-research)
(table temperature-protection)
(table terrain-consumption)
(table terrain-damaged-type)
(table terrain-production)
(table terrain-capacity-x)
(table terrain-exhaustion-type)
(table terrain-initial-supply)
(table terrain-storage-x)
(table tp-max)
(table tp-per-toolup)
(table tp-to-build)
(table tp-attrition)
(table tp-crossover)
(table unit-initial-supply)
(table unit-capacity-x)
(table unit-size-as-occupant)
(table unit-size-in-terrain)
(table unit-storage-x)
(table unload-max)
(table vanishes-on)
(table visibility)
(table vision-night-effect)
(table withdraw-chance-per-attack)
(table wrecks-on)
(table zoc-into-terrain)
(table zoc-from-terrain-effect)
(table zoc-range)
(table zz-basic-hit-worth)
(table zz-basic-capture-worth)
(table zz-basic-transport-worth)

;;; The world.

(world 100
  (axial-tilt 30)
  (circumference 1000)
  (day-length 10)
  (year-length 400)
  )

(area 20 20
  (cell-width 10)
  (height 25)
  (latitude 40)
  (longitude 180)
  (width 30)
  )

(area (aux-terrain 1 "fnsifunosiv"))

(area (cloud-bottoms "xxxababababababa"))

(area (cloud-heights "xxxababababababa"))

(area (clouds "xxxababababababa"))

(area (elevations "xxxdvandkjanldfadreo"))

(area (features (("xxxfoo") ("xxxbar")) "xxxabababaaba"))

(area (material 0 "xxxdadkjnadvadfadf"))

(area (people-sides "xxxababababababa"))

(area (temperatures "xxxuhgfuhpoafnpuoafpjh"))

(area (terrain "xxxabcdebbcacdbee"))

(area (winds "xxxababababababa"))


(doctrine xxxdefault-doctrine
  )

(doctrine xxxbe-aggressive
  )

;;; Sides.

(side-defaults)

(side 1
  (name "xxxOneia")
  (long-name "Nation of xxxOneia")
  (short-name "xxxON")
  (noun "xxxOneian")
  (plural-noun "xxxOneiaa")
  (adjective "xxxOneian")
  (color "xxxgreen,blue")
  (emblem-name "xxxstar")
  (feature-namers)
  (names-locked true)
  (class "xxxtest")
  (active true)
  (status draw)
  (advantage 1)
  (advantage-min 1)
  (advantage-max 3)
  (controlled-by 0)
  (trusts)
  (trades)
  (next-numbers (xxxomnibus 46))
  (unit-namers)
  (feature-namers)
  (tech)
  (init-tech)
  (terrain-view)
  (unit-view)
  (unit-view-dates)
  (turn-time-used 3)
  (total-time-used 15)
  (timeouts 3)
  (timeouts-used 0)
  (finished-turn false)
  (willing-to-draw false)
  (doctrines (u* xxxdefault-doctrine) (xxxomnibus xxxbe-aggressive))
  (doctrines-locked false)
  (self-unit 0)
  (priority 1)
  (scores (xxxtest2 15))
  (ui-data)
  (ai-data)
  (player 1)
  )

(side 2
  (active false)
  (status win)
  )
  
(side 3
  (status lose)
  )

(side 4
  (controlled-by 1)
  )

(independent-units
  (color "xxxblack")
  )

;;; Players.

(player 1
  (name "xxxJoe")
  (config-name "xxx")
  (display-name "xxxscreen")
  (ai-type-name "xxxstupid")
  (password "xxxIUL8h786m")
  (initial-advantage 1)
  )

;;; Units.

(unit-defaults)

(xxxcity 0 3 3 (plan none))

(unit #|45|# xxxF-15
  (@ 5 2 10000)
  (# 3)
  (cp 1)
  (cxp 0)
  (hp 1)
  (in 0)
  (m 0)
  (mo 0)
  (n "xxxCPT xxxSocks")
  (nb 34)
  (opinions 0 0)
  (s 1)
  (tp 0 0 0)
  (z 15000)
  (x
   (xxxhi xxxthere)
   (appear 2)
   (disappear 4)
   )
  (act
   (aa 3)
   (acp 1)
   (acp0 10)
   (am 3)
   (a move 4 2 15000)
   )
  (plan passive
   (ai-control 1)
   (asleep 1)
   (delayed 1)
   (final-turn 1)
   (formation)
   (initial-turn 1)
   (reserve 1)
   )
  )
  
;;; Agreements.

(agreement 1
  (type-name "xxxtest")
  (title "xxxTreaty xxxof xxxTesting")
  (terms
	"xxxBe xxxexcellent xxxto xxxone xxxanother!"
	)
  (drafters 1)
  (proposers 1)
  (signers 1)
  (willing-to-sign 1)
  (known-to side*)
  (enforcement 0)
  (state 0)
  )

;;; Scorekeepers.

(scorekeeper xxxtest1
  (applies-to 0)
  (do xxxdo)
  (initial 0)
  (known-to 0)
  (messages)
  (title "xxxFirst xxxTest xxxSK")
  (trigger)
  (triggered 0)
  (when)
  )

;;; History.

(exu -2 xxxomnibus 1 1 0)

(evt 0 log-started all)
(evt 0 game-started all)
(evt 1 side-joined all)
(evt 1 side-lost all)
(evt 1 side-withdrew all)
(evt 1 side-won all)
(evt 1 unit-started-with all)
(evt 1 unit-created all)
(evt 1 unit-completed all)
(evt 1 unit-acquired all)
(evt 1 unit-moved all)
(evt 1 unit-assaulted all)
(evt 1 unit-damaged all)
(evt 1 unit-captured all)
(evt 1 unit-killed all)
(evt 1 unit-wrecked all)
(evt 1 unit-vanished all)
(evt 1 unit-garrisoned all)
(evt 1 unit-disbanded all)
(evt 1 unit-starved all)
(evt 1 unit-left-world all)
(evt 1 game-ended all)
(evt 1 log-ended all)
(evt 1 action-ok all)
(evt 1 action-error all)
(evt 1 cannot-do all)
(evt 1 insufficient-acp all)
(evt 1 insufficient-material all)
(evt 1 not-implemented-yet all)
(evt 1 action-done all)
(evt 1 move-error all)
(evt 1 insufficient-mp all)
(evt 1 cannot-leave-world all)
(evt 1 destination-full all)
(evt 1 overrun-failed all)
(evt 1 overrun-succeeded all)
(evt 1 capture-failed all)
(evt 1 capture-succeeded all)
(evt 1 fire-into-outside-world all)
(evt 1 too-far all)
(evt 1 too-near all)

;;; Imagery

(imf "xxx"
  ((16 16 tile) (actual 16 16)
   (color (pixel-size 1) "xxxyyyzzzaaa")
   (mono "xxxyyyzzzaaa")
   (mask "xxxyyyzzzaaa")))

(palette)

;;; Misc

(battle)

(namer xxxnamer
  )

;;; other keywords

(define xxxrandom-list
  (quote (
    undefine
    independent-units
    include
    namer
    text
    print
    restrict
    appear
    disappear
    feelings
    quote
    list
    append
    remove
    if
    else
    end-if
    u*
    m*
    t*
    non-unit
    non-material
    non-terrain
    constant
    subarea
    xform
    by-bits
    by-char
    by-name
    random
    grammar
    junky
    tasks
    reset
    stop
    win
    lose
    draw
    end
    cell
    border
    connection
    coating
    river-x
    valley-x
    road-x
    over-nothing
    over-own
    over-border
    over-all
    usual
    reject
    any
    capitalize
    cond
    and
    or
    not
    =
    /=
    <
    <=
    >
    >=
    sum
    before-turn
    after-turn
    after-action
    after-event
    last-side-wins
    make-fractal-percentile-terrain
    make-random-terrain
    make-earthlike-terrain
    make-maze-terrain
    make-rivers
    make-countries
    make-independent-units
    make-initial-materials
    name-units-randomly
    name-geographical-features
    make-roads
    make-random-date
    make-weather
    attrition-in-terrain
    accidents-in-terrain
    units-revolt
    units-surrender
    )
))
