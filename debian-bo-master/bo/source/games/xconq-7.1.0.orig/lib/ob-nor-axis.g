;;; The German Order-of-Battle for Normandy.

(game-module "ob-nor-german"
  (default-base-module "ww2-bn")
  )

; 5 German inf
; 6[01] pz
; 62 SS
; 63 LWF
; 64 para
; 65 flak

(unit-defaults (s 2))

;; 7th army

(hq-inf-div (nb 507700000))
(inf (nb 507710491))
(inf (nb 507710492))
(inf (nb 507710493))
(inf (nb 507710501))
(inf (nb 507710502))
(inf (nb 507710503))

(hq-inf-div (nb 509100000) (n "91"))
(inf (nb 509110571) (n "I/1057"))
(inf (nb 509110572) (n "II/1057"))
(inf (nb 509110581))
(inf (nb 509110582))
(inf (nb 509110591))
(inf (nb 509110592))

(hq-inf-div (nb 524300000))

(hq-inf-div (nb 526500000))

(hq-inf-div (nb 526600000))

(hq-inf-div (nb 527500000))
(inf (nb 527509831))
(inf (nb 527509832))
(inf (nb 527509841))
(inf (nb 527509842))
(inf (nb 527509851))
(inf (nb 527509852))

(hq-inf-div (nb 534300000))

(hq-inf-div (nb 535200000))

(hq-inf-div (nb 535300000))

(hq-inf-div (nb 570900000))

(hq-inf-div (nb 571600000))

;(hq-para-div (nb 640200000))

;(hq-para-div (nb 640300000))

;(hq-para-div (nb 640500000))

;; 15th army

(hq-inf-div (nb 504800000))

(hq-inf-div (nb 508400000))

(hq-inf-div (nb 508500000))

(hq-inf-div (nb 532600000))

(hq-inf-div (nb 533100000))

(hq-inf-div (nb 534400000))

(hq-inf-div (nb 534600000))

(hq-inf-div (nb 571100000))

(hq-inf-div (nb 631700000))

;; 19th army

(hq-inf-div (nb 527100000))

(hq-inf-div (nb 527200000))

(hq-inf-div (nb 527700000))

(hq-inf-div (nb 533800000))

;; 1st army

(hq-inf-div (nb 527600000))

(hq-inf-div (nb 570800000))

;; others

(hq-inf-div (nb 508900000) (n "Norwegian"))

(hq-inf-div (nb 536300000) (n "Danish"))

(hq-inf-div (nb 631600000) (n "LW Dutch"))

(hq-inf-div (nb 631800000))
(inf (nb 631800351))
(inf (nb 631800352))
(inf (nb 631800361))
(inf (nb 631800362))
(inf (nb 631800471))
(inf (nb 631800472))
(inf-pion (nb 631800000))
(arty-horse (nb 631800001))
(arty-horse (nb 631800002))
(arty-horse (nb 631800003))
(arty-nebel-bty (nb 631800000))

;(hq-para-div (nb 640600000))

;; armor

(hq-armor-div (nb 600200000))

(hq-armor-div (nb 600900000))

(hq-armor-div (nb 602100000))

(hq-armor-div (nb 611600000))

(hq-armor-div (nb 613000000) (n "Lehr"))

;; SS

(hq-armor-div (nb 620100000))

(hq-armor-div (nb 620200000))

(hq-armor-div (nb 620900000))

(hq-armor-div (nb 621000000))

(hq-armor-div (nb 621200000))

(hq-inf-div (nb 621700000))  ; actually pzgren
