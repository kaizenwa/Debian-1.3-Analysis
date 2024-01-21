divert(-1)
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
#   RCS: $Id: common.m4,v 143.1 1996/09/16 09:09:38 nau Exp $
#
# common defines for packages
#
# -------------------------------------------------------------------
# create a single object
# $1: mask name
# $2: 'value' of the new object
# $3: package of the circuit
#
define(`CreateObject',
	`ifdef(`PinList_$1', `DefinePinList(PinList_$1)')'
	`PKG_$3(`Description_$1', ,``$2'', Param1_$1, Param2_$1)'
)

# -------------------------------------------------------------------
# define for-loops like the manual tells us
#
define(`forloop',
	`pushdef(`$1', `$2')_forloop(`$1', `$2', `$3', `$4')popdef(`$1')')
define(`_forloop',
	`$4`'ifelse($1, `$3', ,
	`define(`$1', incr($1))_forloop(`$1', `$2', `$3', `$4')')')

# -------------------------------------------------------------------
# the following definitions evaluate the list of pin-names
# missing names will be defined as 'P_#'
#
# the first to arguments are skipped
#
define(`PIN', `Pin($1 $2 $3 $4 ifdef(`P_$5', "P_$5", "$5") ifelse($5, 1, 0x201, 0x01))')
define(`PAD', `Pad($1 $2 $3 $4 $5 ifdef(`P_$6', "P_$6", "$6") ifelse($5, 1, 0x200, 0x00))')
define(`DEFPIN', `define(`count', incr(count))' `define(`P_'count, $1)')
define(`DefinePinList', `ifelse($#, 1, ,
	`pushdef(`count')'
	`define(`count', 0)'
	`_DEFPINLIST($@)'
	`popdef(`count')')')
define(`_DEFPINLIST', `ifelse($#, 0, , $#, 1, `DEFPIN(`$1')',
	`DEFPIN(`$1')'`
	_DEFPINLIST(shift($@))')')

include(connector.inc)
include(dil.inc)
include(misc.inc)
include(plcc.inc)
include(to.inc)

divert(0)dnl
