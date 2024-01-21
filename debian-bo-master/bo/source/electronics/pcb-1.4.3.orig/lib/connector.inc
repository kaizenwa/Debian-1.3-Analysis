#
#                             COPYRIGHT
# 
#   PCB, interactive printed circuit board design
#   Copyright (C) 1994,1995,1996 Thomas Nau
# 
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
# 
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
# 
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
# 
#   Contact addresses for paper mail and Email:
#   Thomas Nau, Schlehenweg 15, 88471 Baustetten, Germany
#   Thomas.Nau@rz.uni-ulm.de
# 
#   RCS: $Id: connector.inc,v 143.1 1996/09/16 09:09:38 nau Exp $
#
# connector packages

# -------------------------------------------------------------------
# the definition of a single connector
# $1: canonical name
# $2: name on PCB
# $3: value
# $4: number of rows
# $5: number of columns
#
define(`PKG_CONNECTOR',
	`define(`MAXY', `eval(`$4' * 100)')
	define(`MAXX', `eval(`$5' * 100)')
Element(0x00 "$1" "$2" "$3" eval(MAXX + 60) 0 3 100 0x00)
(
	forloop(`row', 1, $4, `forloop(`col', 1, $5,
		`PIN(eval(col * 100 -50), eval(row * 100 -50), 60, 30, eval((row-1)*$5+col))
	') ')
	ElementLine(0 0 0 MAXY 20)
	ElementLine(0 MAXY MAXX MAXY 20)
	ElementLine(MAXX MAXY MAXX 0 20)
	ElementLine(MAXX 0 0 0 20)
	ElementLine(0 100 100 100 10)
	ElementLine(100 100 100 0 10)
	Mark(50 50)
)')

# ---------------------------------------------------------------
# base definition (housing) for connector DIN 41.612
# 11/95 Volker Bosch (bosch@iema.e-technik.uni-stuttgart.de)
# female package added 05/96 also by Volker Bosch
#
# $1: canonical name
# $2: name on PCB
# $3: value
# $4: requested rows a,b,c
#
define(`PKG_DIN41_612MALE',
	`define(`XPOS', `eval(300 + 100 * i)')
	define(`MINY', 300)
Element(0x00 "$1" "$2" "$3" 520 550 0 200 0x00)
(
	# Reihe a
	ifelse(index(`$4', `a'), `-1', ,
		forloop(`i', 1, 32, `PIN(eval(3600 - 100 *i), 300, 60, 30, i)
	'))

	# Reihe b
	ifelse(index(`$4', `b'), `-1', , `define(`MINY', 200)'
		forloop(`i', 1, 32, `PIN(eval(3600 - 100 *i), 200, 60, 30, eval(32 + i))
	'))

	# Reihe c
	ifelse(index(`$4', `c'), `-1', , `define(`MINY', 100)'
		forloop(`i', 1, 32, `PIN(eval(3600 - 100 *i), 100, 60, 30, eval(64 + i))
	'))
    # Rueckseite Kontaktstifte
	forloop(`i', 1, 32, `ElementLine(XPOS MINY XPOS 375 40)
	')

	# Befestigungsbohrung
	Pin( 200 400 120 80 "M1" 0x01)
	Pin(3700 400 120 80 "M2" 0x01)

	# Begrenzung M1
	ElementLine( 100  300  320  300 20)
	ElementLine( 320  300  320  395 20)
	ElementLine( 320  395  320  620 10)
	ElementLine( 320  620  200  620 10)
	ElementLine( 200  620  100  620 20)
	ElementLine( 100  620  100  300 20)

	# Begrenzung M2
	ElementLine(3580  300 3800  300 20)
	ElementLine(3800  300 3800  620 20)
	ElementLine(3800  620 3700  620 20)
	ElementLine(3700  620 3580  620 10)
	ElementLine(3580  620 3580  395 10)
	ElementLine(3580  395 3580  300 20)

	# Kante Pins
	ElementLine( 320  395 3580  395 20)

	# Kanten Stifte
	ElementLine( 200 620  200 800 20)
	ElementLine( 200 800 3700 800 20)
	ElementLine(3700 800 3700 620 20)

	Mark(3500 300)
)')

define(`PKG_DIN41_612FEMALE',
	`define(`MAXX', 420)
	define(`MAXX1', `eval(MAXX -170)')
	define(`MAXX2', `eval(MAXX -40)')
Element(0x00 "$1" "$2" "$3" 50 100 3 200 0x00)
(
	# Reihe a
	ifelse(index(`$4', `a'), `-1', ,
		forloop(`i', 1, 32, `PIN(200, eval(300 + 100 *i), 60, 30, i)
	'))

	# Reihe b
	ifelse(index(`$4', `b'), `-1', ,
		forloop(`i', 1, 32, `PIN(300, eval(300 + 100 *i), 60, 30, eval(32 + i))
	'))

	# Reihe c
	ifelse(index(`$4', `c'), `-1', , `define(`MAXX', 520)'
		forloop(`i', 1, 32, `PIN(400, eval(300 + 100 *i), 60, 30, eval(64 + i))
	'))

	# Befestigungsbohrung
	Pin(290  180 120 80 "M1" 0x01)
	Pin(290 3720 120 80 "M2" 0x01)

	# Aeussere Begrenzung
	ElementLine( 80  80 MAXX   80 20)
	ElementLine(MAXX  80 MAXX 3820 20)
	ElementLine(MAXX 3820 80 3820 20)
	ElementLine( 80 3820 80   80 20)

	# Innere Begrenzung
	ElementLine(120  320 MAXX1  320 10)
	ElementLine(MAXX1  320 MAXX1  360 10)
	ElementLine(MAXX1  360 MAXX2  360 10)
	ElementLine(MAXX2  360 MAXX2 3540 10)
	ElementLine(MAXX2 3540 MAXX1 3540 10)
	ElementLine(MAXX1 3540 MAXX1 3580 10)
	ElementLine(MAXX1 3580 120 3580 10)
	ElementLine(120 3580 120  320 10)

	# Markierung: Pin 1a
	Mark(200 400)
)')

# ---------------------------------------------------------------
# base definition (housing) for connector DIN 41.651 laying and standing
# 05/96 Volker Bosch (bosch@iema.e-technik.uni-stuttgart.de)
#
# $1: canonical name
# $2: name on PCB
# $3: value
# $4: number of pins
#
define(`PKG_DIN41_651LAY',
	`define(`MAXY', `eval($4/2 *100 +400)')
	define(`CENTERY', `eval($4/2 *50 +425)')
Element(0x00 "$1" "$2" "$3" 400 250 3 200 0x00)
(
	forloop(`i', 1, eval($4 / 2),
		`PIN(100,  eval(100*i +400), 60, 20, eval(2*i-1))
		PIN(200, eval(100*i +400), 60, 20, eval(2*i))
	')

	# Befestigungsbohrung
	Pin(180  270 100 80 "M1" 0x01)
	Pin(180 eval(MAXY+230) 100 80 "M2" 0x01)

	# aeusserer Rahmen
	ElementLine(80 70 335 70 20)
	ElementLine(335 70 770 200 20)
	ElementLine(770 200 770 300 20)
	ElementLine(770 300 610 390 20)
	ElementLine(610 390 610 eval(MAXY+150) 20)
	ElementLine(610 eval(MAXY+150) 770 eval(MAXY+200) 20)
	ElementLine(770 eval(MAXY+200) 770 eval(MAXY+300) 20)
	ElementLine(770 eval(MAXY+300) 335 eval(MAXY+430) 20)
	ElementLine(335 eval(MAXY+430) 80 eval(MAXY+430) 20)
	ElementLine( 80 eval(MAXY+430) 80 70 20)

	# Codieraussparung
	ElementLine(610 eval(CENTERY -50) 435 eval(CENTERY -50) 5)
	ElementLine(435 eval(CENTERY -50) 435 eval(CENTERY +100) 5)
	ElementLine(435 eval(CENTERY +100) 610 eval(CENTERY +100) 5)

	# Markierung Pin 1
	ElementLine(610 450 500 500 5)
	ElementLine(500 500 610 550 5)

	# Plazierungsmarkierung == Pin 1
	Mark(100 500)
)')

define(`PKG_DIN41_651STAND',
	`define(`MAXY', `eval($4/2 *100 +400)')
	define(`CENTERY', `eval($4/2 *50 +425)')
Element(0x00 "$1" "$2" "$3" 50 100 3 200 0x00)
(
	forloop(`i', 1, eval($4 / 2),
		`PIN(200,  eval(100*i +400), 60, 20, eval(2*i-1))
		PIN(300, eval(100*i +400), 60, 20, eval(2*i))
	')
	# aeusserer Rahmen
	ElementLine(90 70 410 70 20)
	ElementLine(410 70 410 eval(MAXY +430) 20)
	ElementLine(410 eval(MAXY +430) 90 eval(MAXY +430) 20)
	ElementLine(90 eval(MAXY +430) 90 70 20)

	# innerer Rahmen mit Codieraussparung
	ElementLine(110  350 390  350 5)
	ElementLine(390  350 390 eval(MAXY +150) 5)
	ElementLine(390 eval(MAXY +150) 110 eval(MAXY +150) 5)
	ElementLine(110 eval(MAXY +150) 110 eval(CENTERY +100) 5)
	ElementLine(110 eval(CENTERY +100)  90 eval(CENTERY +100) 5)
	ElementLine(90  eval(CENTERY -50) 110 eval(CENTERY -50) 5)
	ElementLine(110 eval(CENTERY -50) 110  350 5)

	# Markierung Pin 1
	ElementLine(110 390 150 350 5)

	# Auswurfhebel oben
	ElementLine(200 70 200 350 5)
	ElementLine(300 70 300 350 5)

	# Auswurfhebel unten
	ElementLine(200 eval(MAXY+150) 200 eval(MAXY+430) 5)
	ElementLine(300 eval(MAXY+150) 300 eval(MAXY+430) 5)

	# Plazierungsmarkierung == Pin 1
	Mark(200 500)
)')

# ---------------------------------------------------------------
# base definition (housing) for SUB-D connectors, laying
# 11/95 Volker Bosch (bosch@iema.e-technik.uni-stuttgart.de)
# female package added 05/96 also by Volker Bosch
#
# $1: canonical name
# $2: name on PCB
# $3: value
# $4: pins
#
# base is upper drilling hole
#
define(`PKG_SUBD_LAY_BASE',
	`define(`BASEX', 1000)
	define(`BASEY', 1000)
	define(`PY1', `eval(BASEY +270)')
	define(`PY2', `eval(PY1 +($4-1)/2*100)')
	define(`OFFSET', `eval(($4+1)/2)')
Element(0x00 "$1" "$2" "$3" eval(BASEX-630) eval(PY1-70) 3 100 0x00)
(
	# Befestigungsbohrung
	Pin(BASEX  BASEY 120 100 "C1" 0x01)
	Pin(BASEX eval(PY2+270) 120 100 "C1" 0x01)

	# Gehaeuse (schmaler Kasten incl. Bohrungen)
	define(`X1', `eval(BASEX-460)')
	define(`Y1', `eval(PY1-390)')
	define(`X2', `eval(BASEX-430)')
	define(`Y2', `eval(PY2+390)')
	ElementLine(X1 Y1 X2 Y1 10)
	ElementLine(X2 Y1 X2 Y2 10)
	ElementLine(X2 Y2 X1 Y2 10)
	ElementLine(X1 Y2 X1 Y1 10)
	ElementLine(X1 eval(Y1+60) X2 eval(Y1+60) 10)
	ElementLine(X1 eval(Y1+180) X2 eval(Y1+180) 10)
	ElementLine(X1 eval(Y2-60) X2 eval(Y2-60) 10)
	ElementLine(X1 eval(Y2-180) X2 eval(Y2-180) 10)

	# Gehaeuse (aeusserer Kasten)
	define(`X1', `eval(BASEX-730)')
	define(`Y1', `eval(PY1-100)')
	define(`X2', `eval(BASEX-460)')
	define(`Y2', `eval(PY2+100)')
	ElementLine(X1 Y1 X2 Y1 20)
	ElementLine(X2 Y1 X2 Y2 10)
	ElementLine(X2 Y2 X1 Y2 20)
	ElementLine(X1 Y2 X1 Y1 20)

	# Gehaeuse (innerer Kasten)
	define(`X1', `eval(BASEX-430)')
	define(`Y1', `eval(PY1-160)')
	define(`X2', `eval(BASEX-230)')
	define(`Y2', `eval(PY2+160)')
	ElementLine(X1 Y1 X2 Y1 20)
	ElementLine(X2 Y1 X2 Y2 20)
	ElementLine(X2 Y2 X1 Y2 20)
	ElementLine(X1 Y2 X1 Y1 10)

	# Pins
	SUBD_SUBFUNCTION
)')

define(`PKG_SUBD_MALE_LAY',
	`define(`SUBD_SUBFUNCTION',
		`forloop(`i', 1, eval($4/2),
			`define(`Y', eval(PY1 +(i-1)*100))
			PIN(eval(BASEX+50), Y, 60, 20, i)
			ElementLine(eval(BASEX+50) Y X2 Y 20)
			PIN(eval(BASEX-50), eval(Y+50), 60, 20, eval(i+OFFSET))
			ElementLine(eval(BASEX-50) eval(Y+50) X2 eval(Y+50) 20)
		')
		PIN(eval(BASEX+50), PY2, 60, 20, OFFSET)
		ElementLine(eval(BASEX+50) PY2 X2 PY2 20)

		# Plazierungsmarkierung == PIN 1
		Mark(eval(BASEX +50) PY1)
	')'
	`PKG_SUBD_LAY_BASE(`$1', `$2', `$3', `$4')'
)

define(`PKG_SUBD_FEMALE_LAY',
	`define(`SUBD_SUBFUNCTION',
		`forloop(`i', 1, eval($4/2),
			`define(`Y', eval(PY1 +(i-1)*100))
			PIN(eval(BASEX+50), Y, 60, 20, eval(OFFSET+1-i))
			ElementLine(eval(BASEX+50) Y X2 Y 20)
			PIN(eval(BASEX-50), eval(Y+50), 60, 20, eval($4+1-i))
			ElementLine(eval(BASEX-50) eval(Y+50) X2 eval(Y+50) 20)
		')
		PIN(eval(BASEX+50), PY2, 60, 20, 1)
		ElementLine(eval(BASEX+50) PY2 X2 PY2 20)

		# Plazierungsmarkierung == PIN 1
		Mark(eval(BASEX +50) PY1)
	')'
	`PKG_SUBD_LAY_BASE(`$1', `$2', `$3', `$4')'
)

