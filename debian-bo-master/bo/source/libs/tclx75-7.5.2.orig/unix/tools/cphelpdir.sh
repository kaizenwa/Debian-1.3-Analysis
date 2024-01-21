#
# cphelpdir.sh --
#
# Script for copying a help tree from the source directory to the build 
# directory.  If the source and build directories are the same, a warning
# is issued.
#
# Arguments:
#   $1 - source help directory.
#   $2 - build help directory.
#------------------------------------------------------------------------------
# Copyright 1993-1996 Karl Lehenbauer and Mark Diekhans.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice appear in all copies.  Karl Lehenbauer and
# Mark Diekhans make no representations about the suitability of this
# software for any purpose.  It is provided "as is" without express or
# implied warranty.
#------------------------------------------------------------------------------
# $Id: cphelpdir.sh,v 7.0 1996/06/16 05:34:34 markd Exp $
#------------------------------------------------------------------------------
#

SRC=$1
BUILD=$2
FOUND=notok
if [ -d $SRC ]
then
    SRCBRF=`(cd $SRC; echo *.brf)`
    if [  "$SRCBRF" != "*.brf" ]
    then
        FOUND=ok
    fi
fi

if [ "$FOUND" = "notok" ]
then
    echo "***"
    echo "*** help files not found. Run \"make buildhelp\" to generate"
    echo "***"
    exit 1
else
    ../runtcl ../tools/instcopy -dirname $SRC $BUILD
    exit $?
fi

