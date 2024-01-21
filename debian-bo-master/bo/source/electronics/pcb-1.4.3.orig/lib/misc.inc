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
#   RCS: $Id: misc.inc,v 143.1 1996/09/16 09:09:43 nau Exp $
#
# misc packages
#

# -------------------------------------------------------------------
# the definition of a SD (ZIP) package
# based on 'old style format' by Olaf Kaluza (olaf@criseis.ruhr.de)
# $1: canonical name
# $2: name on PCB
# $3: value
# $4: number of pins
#
define(`PKG_SD',
	`define(`MAXY', `eval(`$4' / 2 * 100 + 50)')
Element(0x00 "$1" "$2" "$3" 275 50 3 100 0x00)
(
	forloop(`i', 1, eval($4 / 2),
		`PIN(50,  eval(100*(i-1)+50), 50, 20, eval(2*i-1))
		PIN(150, eval(100*(i-1)+100), 50, 20, eval(2*i))
	')

	ElementLine(0 0 0 MAXY 20)
	ElementLine(0 MAXY 200 MAXY 20)
	ElementLine(200 MAXY 200 0 20)
	ElementLine(200 0 0 0 20)
	ElementLine(100 0 100 100 10)
	ElementLine(100 100 0 100 10)

	Mark(50 50)
)')

# -------------------------------------------------------------------
# the definition of a "MULTIWATT" package (I never actually saw one)
# based on 'old style format' by Olaf Kaluza (olaf@criseis.ruhr.de)
# $1: canonical name
# $2: name on PCB
# $3: value
define(`PKG_MULTIWATT15',
`Element(0x00 "$1" "$2" "$3" 150 100 3 100 0x00)
(
	forloop(`i', 1, 15,
		`PIN(ifelse(eval(i % 2 == 0), 1, 250, 50), eval(i*50), 60, 20, i)
	')
	ElementLine(0 0 400 0 20)
	ElementLine(400 0 400 800 20)
	ElementLine(400 800 0 800 20)
	ElementLine(0 800 0 0 20)
	Mark(50 50)
)')

# -------------------------------------------------------------------
# the definition of a resistor (0.25W) package
# $1: canonical name
# $2: name on PCB
# $3: value
define(`PKG_R025',
`Element(0x00 "$1" "$2" "$3" 120 30 0 100 0x00)
(
	PIN(0, 50, 50, 20, 1)
	PIN(400, 50, 50, 20, 2)
	ElementLine(100 0 300 0 20)
	ElementLine(300 0 300 100 20)
	ElementLine(300 100 100 100 20)
	ElementLine(100 100 100 0 20)
	ElementLine(0 50 100 50 20)
	ElementLine(300 50 400 50 20)
	Mark(0 50)
)')

# -------------------------------------------------------------------
# the definition of a SIL package without a common pin
# $1: canonical name
# $2: name on PCB
# $3: value
# $4: number of pins
define(`PKG_SIL',
	`define(`MAXY', `eval(`$4' * 100 -50)')
Element(0x00 "$1" "$2" "$3" 160 10 3 100 0x00)
(
	forloop(`i', 1, $4,
		`PIN(50, eval(i * 100 -50), 50, 20, i)
	')
	ElementLine(  0 50   0 MAXY 20)
	ElementLine(100 50 100 MAXY 20)
	ElementArc(50  50 50 50 180 180 20)
	ElementArc(50 MAXY 50 50   0 180 20)
	forloop(`i', 1, eval($4 /2 -1),
		`ElementLine(0 eval(i * 200) 100 eval(i * 200) 10)
	')
	Mark(50 50)
)')

# -------------------------------------------------------------------
# the definition of a SIL package with a common pin
# $1: canonical name
# $2: name on PCB
# $3: value
# $4: number of pins
define(`PKG_CSIL',
	`define(`MAXY', `eval(`$4' * 100 -50)')
Element(0x00 "$1" "$2" "$3" 160 10 3 100 0x00)
(
	forloop(`i', 1, $4,
		`PIN(50, eval(i * 100 -50), 50, 20, i)
	')
	ElementLine(  0 50   0 MAXY 20)
	ElementLine(100 50 100 MAXY 20)
	ElementLine(  0 100 100 100 10)
	ElementArc(50  50 50 50 180 180 20)
	ElementArc(50 MAXY 50 50   0 180 20)
	Mark(50 50)
)')

# -------------------------------------------------------------------
# a QFP-132 pin flat pack
# 
# $1: canonical name
# $2: name on PCB
# $3: value
#
# based on mail by Volker Bosch (bosch@iema.e-technik.uni-stuttgart.de)
define(`PKG_QFP132',
`Element(0x00 "$1" "$2" "$3" 250 200 0 150 0x00)
(
	forloop(`i', 1, 17,
		`define(`XPOS', eval(625 -i*25))'
		`PAD(XPOS, 40, XPOS, 90, 15, i)
	')
	forloop(`i', 1, 16,
		`define(`XPOS', eval(1025 -i*25))'
		`PAD(XPOS, 40, XPOS, 90, 15, eval(i+116))
	')
	forloop(`i', 1, 33,
		`define(`YPOS', eval(175 +i*25))'
		`PAD(30, YPOS, 80, YPOS, 15, i)
	')
	forloop(`i', 1, 33,
		`define(`XPOS', eval(175 +i*25))'
		`PAD(XPOS, 1160, XPOS, 1110, 15, eval(i+50))
	')
	forloop(`i', 1, 33,
		`define(`YPOS', eval(1025 -i*25))'
		`PAD(1120, YPOS, 1170, YPOS, 15, i)
	')

	# Markierung pin 1
	ElementArc(600 150 10 10 0 360 5)

	# Nase links oben (PIN 17/18)
	ElementLine( 125 175  75 150 20)
	ElementLine(  75 150  75 125 20)
	ElementLine(  75 125 125  75 20)
	ElementLine( 125  75 150  75 20)
	ElementLine( 150  75 175 125 20)

	# Verbindungsline zur Ecke rechts oben
	ElementLine( 175  125 1025 125 20)

	# Nase rechts oben (PIN 116/117)
	ElementLine(1025 125 1050  75 20)
	ElementLine(1050  75 1075  75 20)
	ElementLine(1075  75 1125 125 20) 
	ElementLine(1125 125 1125 150 20)
	ElementLine(1125 150 1075 175 20)

	# Verbindungsline zur Ecke rechts unten
	ElementLine(1075 175  1075 1025 20)

	# Nase rechts unten (PIN 83/84)
	ElementLine(1075 1025 1125 1050 20)
	ElementLine(1125 1050 1125 1075 20)
	ElementLine(1125 1075 1075 1125 20)
	ElementLine(1075 1125 1050 1125 20)
	ElementLine(1050 1125 1025 1075 20)

	# Verbindungsline zur Ecke links unten 
	ElementLine(1025 1075  175 1075 20)

	# Nase links unten (PIN 50/51)
	ElementLine( 175 1075 150 1125 20)
	ElementLine( 150 1125 125 1125 20)
	ElementLine( 125 1125  75 1075 20)
	ElementLine(  75 1075  75 1050 20)
	ElementLine(  75 1050 125 1025 20)

	# Verbindungsline zur Ecke links oben
	ElementLine( 125 1025 125 175 20)

	# Markierung so anordnen, dass Pinanchse im 25-MIL-Raster zu liegen kommt
	Mark(200 200)
)')

# -------------------------------------------------------------------
# LED
# Pin 1 is -, 2 is +
# 
# $1: canonical name
# $2: name on PCB
# $3: value
# $4: diameter
#
# based on mail by Volker Bosch (bosch@iema.e-technik.uni-stuttgart.de)
define(`PKG_LED',
	`define(`RADIUS1', `eval(`$4' /2)')
	define(`RADIUS2', `eval(`$4' /2 +20)')
Element(0x00 "$1" "$2" "$3" 100 70 0 100 0x00)
(
	PIN(150, 150, 60, 20, 1)
	PIN(150, 250, 60, 20, 2)
	ElementArc(150 200 RADIUS1 RADIUS1 0 360 15)
	ElementArc(150 200 RADIUS2 RADIUS2 0 360 10)
	Mark(150 150)
)')

# -------------------------------------------------------------------
# diodes
# Pin 1 is K, 2 is A
# 
# $1: canonical name
# $2: name on PCB
# $3: value
# $4: distance in mil
#
define(`PKG_DIODE_LAY',
	`define(`X1', `eval(`$4' /3)')
	define(`X2', `eval(`$4' -X1)')
	define(`Y', `eval(`$4' /2)')
	define(`DELTA', `eval(X1 /2)')
	define(`PINSIZE', `ifelse(eval($4 >= 500), 1, 80, 50)')
Element(0x00 "$1" "$2" "$3" eval(X2+20) eval(Y-DELTA) 0 100 0x00)
(
	PIN(0, Y, PINSIZE, 20, 1)
	PIN($4, Y, PINSIZE, 20, 2)

	ElementLine(0 Y X1 Y 10)
	ElementLine(X2 Y $4 Y 10)

	ElementLine(X1 Y X2 eval(Y-DELTA) 10)
	ElementLine(X2 eval(Y-DELTA) X2 eval(Y+DELTA) 10)
	ElementLine(X2 eval(Y+DELTA) X1 Y 10)
	ElementLine(X1 eval(Y-DELTA) X1 eval(Y+DELTA) 20)
	Mark(0 Y)
)')

# -------------------------------------------------------------------
# an crystal package
# Pin 1 is K, 2 is A
# 
# $1: canonical name
# $2: name on PCB
# $3: value
# $4: package width in MIL
#
define(`PKG_CRYSTAL',
	`define(`X1', `100')
	define(`X2', `$4')
	define(`Y', `100')
	define(`Y1', `eval(Y -50)')
	define(`Y2', `eval(Y +50)')
Element(0x00 "$1" "$2" "$3" X1 eval(Y2+20) 0 100 0x00)
(
	PIN(X1, Y, 60, 20, 1)
	PIN(X2, Y, 60, 20, 2)
	ElementLine (X1 Y1 X2 Y1 20)
	ElementLine (X1 Y2 X2 Y2 20)
	ElementArc(X1 Y 50 50 270 180 20)
	ElementArc(X2 Y 50 50 90 180 20)
	Mark (X1 Y)
)')

