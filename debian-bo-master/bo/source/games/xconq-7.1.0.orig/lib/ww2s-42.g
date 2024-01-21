(game-module "ww2s-42"
  (title "WWII (Std), Jan 42")
  (blurb "The world in January 1942 (based on types from standard game)")
  (base-module "ww2-stdunit")
  (variants (see-all true))
  )

(add u* start-with 0)
(add u* independent-near-start 0)

(set synthesis-methods '(name-units-randomly))

(include "earth-1deg")

(scorekeeper (do last-side-wins)) ; for now

;;; Sides.
;;; This is a simplified game, so have only two sides.

(side 1 (name "Allies"))

(side 2 (name "Axis"))

(set sides-min 2)
(set sides-max 2)

;;; Units.

(define capital city)

;; This maps countries to Allies, Axis, or neutral.

(define uk 1)
(define fr 2)
(define us 1)
(define de 2)
(define it 2)
(define jp 2)
(define su 1)
(define zh 1)
(define es 0)

(include "u-e1-1938")

;; Changes for beginning of 1942.
;; (should be in a generic 1942 status file)

;; Iceland was occupied (sort of) by Britain.

(unit "Reykjavik" (s 1))

;; Douala is Free French.

(unit "Douala" (s 1))

;; Indonesia is Dutch.

(unit "Banjarmasin" (s 1))
(unit "Jakarta" (s 1))
(unit "Medan" (s 1))
(unit "Palembang" (s 1))
(unit "Surabaya" (s 1))
(unit "Ujung Pendang" (s 1))

;; German/Italian conquests and allies.

(unit "Athens" (s 2))
(unit "Belgrade" (s 2))
(unit "Brussels" (s 2))
(unit "Bucharest" (s 2))
(unit "Budapest" (s 2))
(unit "Copenhagen" (s 2))
(unit "Danzig" (s 2))
(unit "Kiev" (s 2))
(unit "Minsk" (s 2))
(unit "Odessa" (s 2))
(unit "Prague" (s 2))
(unit "Riga" (s 2))
(unit "Rotterdam" (s 2))
(unit "Sofia" (s 2))
(unit "Thessaloniki" (s 2))
(unit "Vienna" (s 2))
(unit "Vilnius" (s 2))
(unit "Warsaw" (s 2))
(unit "Zagreb" (s 2))

;; Japanese conquests in China.

(unit "Baoding" (s 2))
(unit "Beijing" (s 2))
(unit "Guangzhou" (s 2))
(unit "Kaohsiun" (s 2))
(unit "Nanjing" (s 2))
(unit "Nanchang" (s 2))
(unit "Shanghai" (s 2))
(unit "Shantou" (s 2))
(unit "Taipei" (s 2))
(unit "Tianjin" (s 2))
(unit "Wuhan" (s 2))
(unit "Xiamen" (s 2))
(unit "Xuzhou" (s 2))
