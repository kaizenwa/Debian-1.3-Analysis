#! /bin/sh
# @(#) mkpath.sh,v 1.6 1992/07/11 11:40:14 tron Exp
#
# mkpath - make a pathalias output file
#
#    Copyright (C) 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992 Ronald S. Karr
#
# See the file COPYING, distributed with smail, for restriction
# and warranty information.

# Usage:	
#	mkpath [-v] [-V] [-x] [-e] [-n] [-t trace] [path_config]
#
#	-v		verbose mode for mkpath, execution block commands
#			are executed with the sh(1) -v flag
#	-V		pathalias(8) runs with '-v' verbose mode
#	-x 		execution block commands are executed with the
#			sh(1) '-x' option.
#	-e		tries to get mkpath to stop when it encounters a
#			syntax error, and the internal sh(1) to stop when
#			a top level command exits with a non-zero status.
#	-n		disables the execution of any sh(1) commands
#			useful with the '-v' and disables '-x', '-e', '-V', '-t'
#	-t trace	all input of pathalias(8) is to be copied into
#			the file 'trace'.
#	path_config 	pathalia's configuration file, '-' => stdin,
#			or $LIB_DIR/maps/mkpath.conf by default
#
# If '-V' and '-t' are both used, '-V' must be before '-t'.


# base directory for smail library/utility
#
# These directories should be owned by root, and only writeable by root
#
LIB_DIR="X_LIB_DIR_X"
UTIL_BIN_DIR="X_UTIL_BIN_DIR_X"
TMPDIR="X_TMP_DIR_X"
MAPDIR="$LIB_DIR/maps"

# standard locations
#
PATHALIAS=$UTIL_BIN_DIR/pathalias
DCASEHOST=$UTIL_BIN_DIR/dcasehost
AWKFILE=$UTIL_BIN_DIR/mkpath.awk
GLEEM=$UTIL_BIN_DIR/gleem
GETOPT=$UTIL_BIN_DIR/getopt
SORT=sort
PATH="X_UTIL_PATH_X:X_SECURE_PATH_X"; export PATH

# parse args - need a public domain getopt command - XXX
#
PROG=$0
USAGE="$PROG: usage: $PROG [-v] [-V] [-x] [-e] [-n] [-t trace] [path_config]"
set -- `$GETOPT -n $PROG -q vVxent: $*`
if [ "$?" -ne 0 ]; then
    echo "$USAGE" 1>&2
    exit 1
fi
SHELL_ARGS="-"
ERR=0
TEE_FILE=
PWD=`pwd`
for i in $*; do
    case $i in
    -v)	SHELL_ARGS="$SHELL_ARGS"v; shift;;
    -V)	PATHALIAS="$PATHALIAS -v"; shift;;
    -x)	SHELL_ARGS="$SHELL_ARGS"x; shift;;
    -e)	SHELL_ARGS="$SHELL_ARGS"e; ERR=1; shift;;
    -n)	SHELL_ARGS="$SHELL_ARGS"n; shift;;
    -t) if [ -z "$TEE_FILE" ]; then
	    TEE_FILE="$2"; PATHALIAS="tee -a $TEE_FILE | $PATHALIAS";
	fi; shift 2;;
    --)	shift; break;;
    esac
done
# determine the path.conf config file
if [ "$#" -gt 1 ]; then
    echo "$USAGE"
    exit 4
elif [ "$#" -eq 1 ]; then
    CONFIG="$1"
else
    CONFIG=
fi

# verify that we can read the config file
# determine the initial directory
#
if [ "$CONFIG" = "-" ]; then
    CD="$PWD"
    CONFIG="mkpath_stdin"
    CONF=""
elif [ -z "$CONFIG" ]; then
    CD="$MAPDIR"
    cd "$CD"
    CD=`pwd`
    CONFIG="$MAPDIR"/mkpath.conf
    CONF="$CONFIG"
elif [ -r "$CONFIG" ]; then
    if [ "`expr $CONFIG : '\\/'`" -eq 0 ]; then
	CD=`pwd`
    else
    	CD=`echo "$CONFIG" | sed 's,^\(.*\)/[^/][^/]*$,\1,'`
	cd "$CD"
	CD=`pwd`
    fi
    CONF="$CONFIG"
else
    echo "$PROG: can not read $CONFIG" 1>&2
    exit 5
fi

# special action commands
#
# args to run pathalias, switch fields and sort for the pathsort command
PATHSORT="$PATHALIAS -i -D|$DCASEHOST|$SORT -T $TMPDIR"

# export vars needed for the sub-shell
#

# process the config file
#
trap "rm -f $TMPDIR/stm*;exit 1" 1 2 3 15
if [ "$TEE_FILE" ]; then
	cat /dev/null > $TEE_FILE
fi
# strip input of #comments, run it thru awk and into the shell
(echo "#! /bin/sh $SHELL_ARGS"
 echo "PROG=\"$PROG\""
 echo "PWD=\"$PWD\""
 echo "CD=\"$CD\""
 echo "CWD=\"$CD\""
 echo "ERR=\"$ERR\""
 echo "PATH=\"$PATH\""
 echo "GLEEM=\"$GLEEM\""
 echo "PATHALIAS=\"$PATHALIAS\""
 echo "PATHSORT=\"$PATHSORT\""
 echo "export PROG PWD CD CWD ERR PATH GLEEM PATHALIAS PATHSORT"
 if [ -z "$CONF" ]; then
	sed -e 's/[	 ]*#.*$//' -e 's/^[	 ]*//'
 else	
	sed -e 's/[	 ]*#.*$//' -e 's/^[	 ]*//' < "$CONFIG"
 fi | awk -f "$AWKFILE" ERR="$ERR" PROG="$PROG" - ) | /bin/sh "$SHELL_ARGS"
exit $?
