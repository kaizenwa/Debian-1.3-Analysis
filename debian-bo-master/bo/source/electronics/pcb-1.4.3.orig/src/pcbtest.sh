#! /bin/sh
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
#   RCS: $Id: pcbtest.sh,v 143.1 1996/09/16 09:08:50 nau Exp $
#
#
#
# starts a test installation of pcb

# setup several paths and create the application resource default
# file in this directory
#
make Pcb.ad
sed -e 's/^\(pcb.elementPath\).*$/\1: .:packages:circuits/' \
	-e 's/^\(pcb.filePath\).*$/\1: ./' \
	-e 's/^\(pcb.fontPath\).*$/\1: ./' \
	-e 's/^\(pcb.libraryCommand\).*$/\1: ..\/lib\/QueryLibrary.sh "%p" "%f" %a/' \
	-e 's/^\(pcb.libraryContentsCommand\).*$/\1: ..\/lib\/ListLibraryContents.sh "%p" "%f"/' \
	-e 's/^\(pcb.libraryPath\).*$/\1: .:..\/lib/' \
	Pcb.ad > Pcb

# execute pcb
#
XAPPLRESDIR=.
export XAPPLRESDIR
exec ./pcb $@
