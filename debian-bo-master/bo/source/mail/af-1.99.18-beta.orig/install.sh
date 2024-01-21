#! /bin/sh
#
#  Install.sh - Simple script to install af.
#  Copyright (C) 1991, 1992, 1996 Malc Arnold.
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2, or (at your option)
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
# $Id: install.sh,v 1.3 1996/03/17 00:56:48 malc Exp $
#
##############################################################################
# This simple script is enough to install af if you don't have a BSD
# compatible install program.  It isn't general, but it will do for af.
#
# Usage:
#
# install.sh [ -c ] [ -m mode ] [ -o owner ] [ -g group ] [ -s ] src ... dest
# install.sh -d [ -m mode ] [ -o owner ] [ -g group ] dir
#
#******************************************************************************
# The constants
USAGE="usage: `basename $0` [ -m mode ] [ -o owner ] [ -g group ] [ -s ] [ -c ] src dest
       `basename $0` -d [ -m mode ] [ -o owner ] [ -g group ] dir"

# Set the default options
MODE=""
OWNER=""
GROUP=""
STRIP=""
DIR=""

# Set the list of files to process
SRC=""
DEST=""

# Handle the arguments
while test -n "$1"; do
	case "$1" in
	-m)	MODE=$2
		shift;;
	-o)	OWNER=$2
		shift;;
	-g)	GROUP=$2
		shift;;
	-s)	STRIP=yes;;
	-c)	;;
	-d)	DIR=yes;;
	*)	SRC="$DEST $SRC"
		DEST=$1;;
	esac
	shift
done

# Make source equal to destination for directories
test -n "$DIR" && SRC="$DEST"

# More argument checking
if test -z "$DEST" || test -z "$SRC"; then
	echo $USAGE
	exit 1
fi

# Now we actually do the task
if test -n "$DIR"; then
	test -d $DEST || mkdir $DEST || exit $?
else
	cp $SRC $DEST || exit $?
fi

# Handle setting modes and ownership and stripping the file
for file in "$SRC"
do
	# Decide which file we just created
	if test -d "$DEST" && test -z "$DIR"; then
		target="$DEST/$file"
	else
		target="$DEST"
	fi

	test -n "$STRIP" && strip $target
	test -n "$MODE" && chmod $MODE $target
	test -n "$GROUP" && chgrp $GROUP $target
	test -n "$OWNER" && chown $OWNER $target
done

# All ok
exit 0
