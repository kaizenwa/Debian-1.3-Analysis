#!/bin/sh
#
#  Showinfo - Display an info file to stdout
#  Copyright (C) 1996 Malc Arnold.
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 1, or (at your option)
#  any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */
#
#
##############################################################################
# RCS Info
#
# $Id: Afinfo.sh,v 1.1 1996/03/17 01:10:47 malc Exp $
#
##############################################################################
# The syntax for showinfo is pretty simple
#
#	Showinfo topic
#
##############################################################################
# The sed expressions we use
STRIPTOP="1,/^File:.* Node: Top/d"
STRIPEND="/^File:.* Node: Key Index/,999999d"
STRIPMENU="/^* Menu:/,/^File:/d"
STRIPNODE="/^File:/d"

# And the control characters to strip
CTRLS='[\037]'

# Check the arguments
if test $# != 1; then
	echo Usage: $0 topic
	exit 1
fi

# Now set up the files to read and how
SUFFIX="info*"
UNCOMPRESS=cat

# Handle compressed or gzipped info files
test -s "$1.info.Z" && SUFFIX="info-*.Z" && UNCOMPRESS="uncompress -c"
test -s "$1.info.gz" && SUFFIX="info-*.gz" && UNCOMPRESS="gunzip -c"

# Now we can do the work
$UNCOMPRESS $1.$SUFFIX | sed -e "$STRIPTOP" -e "$STRIPEND" \
-e "$STRIPMENU" -e $STRIPNODE | tr -d $CTRLS


