#! /bin/sh
:
# @(#) checkdefs.sh,v 1.5 1995/07/06 17:01:18 nm4 Exp
#
# Update the defs.h, defs.sh and defs.sed files if they don't exist
# or will change as a result of differences in the EDITME, os and arch
# files.
#
#    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992  Ronald S. Karr
# 
# See the file COPYING, distributed with smail, for restriction
# and warranty information.

# Note:  this shell script uses case rather than test to prevent
#	 a fork/exec on systems where test is not a builtin.
#
# Note:	 We assume that ROOT and SHELL are passed in the environment.

MKDEFS=$ROOT/conf/lib/mkdefs.sh

if [ ! -f defs.sh ] || [ ! -f defs.sed ] || [ ! -f defs.h ]; then
	rm -f defs.sh defs.sed defs.h
	echo $SHELL $MKDEFS
	$SHELL $MKDEFS
else
	echo Save copy of defs.sh, defs.h and defs.sed ...
	if mv defs.sh .defs.sh &&
	   mv defs.h .defs.h &&
	   mv defs.sed .defs.sed
	then
		:
	else
		echo "Failed to save defs.sh defs.h and defs.sed!" 1>&2
		exit 1
	fi
	echo $SHELL $MKDEFS
	$SHELL $MKDEFS
	if cmp -s defs.sh .defs.sh &&
	   cmp -s defs.h .defs.h &&
	   cmp -s defs.sed .defs.sed
	then
		mv .defs.sh defs.sh
		mv .defs.h defs.h
		mv .defs.sed defs.sed
		echo "saved files are identical, restored"
	fi
fi

exit 0
