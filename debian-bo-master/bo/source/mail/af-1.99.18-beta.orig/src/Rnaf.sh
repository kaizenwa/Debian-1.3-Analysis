#!/bin/sh
#
#  Rnaf - Use af to send a mail reply to a news article.
#  Copyright (C) 1992, 1996 Malc Arnold.
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
# $Id: Rnaf.sh,v 1.2 1996/03/17 01:10:47 malc Exp $
#
##############################################################################
# Oddly enough, the syntax for rnaf is the same as for rnmail:
#
#	Rnaf [ -h headerfile ] [ destination ... ]
#
##############################################################################
# Don't you hate scripts with this high a comment-to-code ratio?

# Handle a header file being given
if [ $# -ge 2 -a "$1" = "-h" ]; then
	HEADERFILE=$2
	shift ; shift
	af -EH $* < $HEADERFILE
	exit $?
fi

# Invoke af in sending mode
af -EH $*
exit $?
