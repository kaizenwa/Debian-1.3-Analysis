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
#   RCS: $Id: plcc.inc,v 143.1 1996/09/16 09:09:44 nau Exp $
#
# PLCC packages
#

# -------------------------------------------------------------------
# the definition of a plcc package
# works only for packages with (# of pins -4) dividable by 16
# $1: canonical name
# $2: name on PCB
# $3: value
# $4: number of pins
# $5: additional border
#
define(`PKG_PLCC',
	`define(`QUARTER', `eval($4 /4)')
	define(`OFFSET', eval((QUARTER +1) / 2))
	define(`BORDER', `$5')
	define(`WIDTH', `eval((OFFSET+1) *100 +2*BORDER)')
	define(`CENTER', `eval(WIDTH / 2)')
	define(`ILLEGALPIN', `$4')
Element(0x00 "$1" "$2" "$3" eval(BORDER+200) CENTER 0 100 0x00)
(
	# the four special corners
	PIN(eval(BORDER+100), BORDER, 50, 20, OFFSET)
	PIN(BORDER, eval(WIDTH-BORDER-100), 50, 20, eval(OFFSET+QUARTER))
	PIN(eval(WIDTH-BORDER-100), eval(WIDTH-BORDER), 50, 20, eval(OFFSET+2*QUARTER))
	PIN(eval(WIDTH-BORDER), eval(BORDER+100), 50, 20, eval(OFFSET+3*QUARTER))

	# left row without corner
define(`X', BORDER)
define(`Y', eval(BORDER+100))
define(`count', `eval(OFFSET+1)')
forloop(`i', 1, eval(OFFSET-1),
	`PIN(X, Y, 50, 20, count)' `define(`count', incr(count))'
	`PIN(eval(X+100), Y, 50, 20, count)' `define(`count', incr(count))'
	`define(`Y', eval(Y+100))'
)

	# bottom row without corner
define(`X', eval(X+100))
define(`Y', eval(Y+100))
define(`count', incr(count))
forloop(`i', 1, eval(OFFSET-1),
	`PIN(X, Y, 50, 20, count)' `define(`count', incr(count))'
	`PIN(X, eval(Y-100), 50, 20, count)' `define(`count', incr(count))'
	`define(`X', eval(X+100))'
)

	# right row without corner
define(`X', eval(X+100))
define(`Y', eval(Y-100))
define(`count', incr(count))
forloop(`i', 1, eval(OFFSET-1),
	`PIN(X, Y, 50, 20, count)' `define(`count', incr(count))'
	`PIN(eval(X-100), Y, 50, 20, count)' `define(`count', incr(count))'
	`define(`Y', eval(Y-100))'
)

	# top row without corner
define(`X', eval(X-100))
define(`Y', eval(Y-100))
define(`count', incr(count))
forloop(`i', 1, eval(OFFSET-1),
	`PIN(X, Y, 50, 20, count)' `define(`count', incr(count))'
	`ifelse(eval(count > ILLEGALPIN), 1, `define(`count', 1)')'
	`PIN(X, eval(Y+100), 50, 20, count)' `define(`count', incr(count))'
	`ifelse(eval(count > ILLEGALPIN), 1, `define(`count', 1)')'
	`define(`X', eval(X-100))'
)

	ElementLine(0 0 WIDTH 0 20)
	ElementLine(WIDTH 0 WIDTH WIDTH 20)
	ElementLine(WIDTH WIDTH 0 WIDTH 20)
	ElementLine(0 WIDTH 0 0 20)
	ElementLine(0 100 100 0 10)
	ElementLine(eval(CENTER-50) 0 CENTER 50 10)
	ElementLine(CENTER 50 eval(CENTER+50) 0 10)

	Mark(CENTER BORDER)
)')

