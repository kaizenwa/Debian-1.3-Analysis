#! /bin/sh
# @(#) mkuuwho.sh,v 1.9 1992/07/11 11:40:22 tron Exp
#
#    Copyright (C) 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992 Ronald S. Karr
#
# See the file COPYING, distributed with smail, for restriction
# and warranty information.

# mkuuwho - make the uuwho database
#
# usage: mkuuwho [-v] [-x] [-e] [-n] [-t trace] [-u uuwho_data] [mkpath.conf]
#
#	-v		verbose mode for mkpath, execution block commands
#			are executed with the sh(1) -v flag
#	-x 		execution block commands are executed with the
#			sh(1) '-x' option.
#	-e		tries to get mkpath to stop when it encounters a
#			syntax error, and the internal sh(1) to stop when
#			a top level command exits with a non-zero status.
#	-n		disables the execution of any sh(1) commands
#			useful with the '-v', disables '-x', '-t' and '-e'
#	-t trace	all input of uuwho(1) is to be copied into
#			the file 'trace'.
#	-u uuwho_data	have uuwho build the uuwho DBM database 'uuwho_data'
#	mkpath.conf     use mkpath.conf rather then $LIB_DIR/maps/mkpath.conf
#
# files:
#    $UNSHAR_MAP_DIR			- maps are unpacked here
#    $UNSHAR_MAP_DIR/work/getmap.log	- log of getmap activity and errors
#    $UNSHAR_MAP_DIR/work/getmap.err	- like getmap.log + skipped lines
#					  removed if no major unshar errors
#    $LIB_DIR/uuwho.dir			- uuwho DBM database
#    $LIB_DIR/uuwho.pag			- uuwho DBM database
#    $LIB_DIR/uuwho			- database used when DBM not used

# locations and constants
#
PATH="X_UTIL_PATH_X:X_SECURE_PATH_X"; export PATH
UTIL_BIN="X_UTIL_BIN_DIR_X"
LIB_DIR="X_LIB_DIR_X"
UUWHO_DATA="X_UUWHO_FILE_X"
SMAIL_BIN_DIR="X_SMAIL_BIN_DIR_X"
SMAIL="X_SMAIL_NAME_X -ep -i -t"
AWKFILE=$UTIL_BIN/mkuuwho.awk
UUWHO=uuwho
GLEEM=$UTIL_BIN/gleem
GETOPT=$UTIL_BIN/getopt
HAVE_DBM="X_HAVE_DBM_X"
HAVE_NDBM="X_HAVE_NDBM_X"
UUWHO_USE_DBM="X_UUWHO_USE_DBM_X"
DBM=
case "$HAVE_DBM$HAVE_NDBM" in
?*)	case "$UUWHO_USE_DBM" in
	?*)	DBM=yes;;
	esac;;
esac

# set defaults value for location that can change by option
#
UNSHAR_MAP_DIR="X_UNSHAR_MAP_DIR_X"
MKPATH_CONF=$LIB_DIR/maps/mkpath.conf
case $UUWHO_DATA in
"")	UUWHO_DATA=$LIB_DIR/uuwho;;
/*)	:;;
*)	UUWHO_DATA=$LIB_DIR/$UUWHO_DATA;;
esac

# parse args
#
PROG=$0
USAGE="usage: $PROG [-v] [-x] [-e] [-n] [-t trace] [-u uuwho_file] [mkpath.conf]"
set -- `$GETOPT -n $PROG -q vxent:u: $*`
if [ $? != 0 ]; then
    echo $USAGE 1>&2
    exit 1
fi
SHELL_ARGS="-"
ERR=0
TEE_FILE=
for i in $*; do
    case $i in
    -v)	SHELL_ARGS="$SHELL_ARGS"v; shift;;
    -x)	SHELL_ARGS="$SHELL_ARGS"x; shift;;
    -e)	SHELL_ARGS="$SHELL_ARGS"e; ERR=1; shift;;
    -n)	SHELL_ARGS="$SHELL_ARGS"n; NO_UUWHO=1; shift;;
    -t) if [ -z "$TEE_FILE" ]; then
	    TEE_FILE="$2"; UUWHO="tee $TEE_FILE | $UUWHO";
	fi; shift 2;;
    -u) UUWHO_DATA=$2; shift 2;;
    --) shift; break;;
    esac
done
if [ "$#" -gt 1 ]; then
    echo $USAGE 1>&2
    exit 1
elif [ "$#" -eq 1 ]; then
    MKPATH_CONF=$1
fi
TUUWHO_DATA="`echo $UUWHO_DATA | sed -e 's,^\(.*\)/\([^/][^/]*\)$,\1,'`"/uuwho_tmp

# verify that we can read the config file
# determine the initial directory
#
PWD=`pwd`
if [ -r "$MKPATH_CONF" ]; then
    if [ "`expr $MKPATH_CONF : '\\/'`" -eq 0 ]; then
	CD=`pwd`
    else
    	CD=`echo "$MKPATH_CONF" | sed 's,^\(.*\)/[^/][^/]*$,\1,'`
	cd "$CD"
	CD=`pwd`
    fi
    CONF="$MKPATH_CONF"
else
    echo "$PROG: can not read $MKPATH_CONF" 1>&2
    exit 3
fi

# clear out the tempory uuwho database
#
case "$DBM" in
?*)	rm -f $TUUWHO_DATA.dir $TUUWHO_DATA.pag;;
*)	rm -f $TUUWHO_DATA;;
esac

# uuwho DBM building
#
if [ -z "$NO_MKPATH_CONF" ]; then

    # XXX - small race condition but who cares?
    (echo "#! /bin/sh $SHELL_ARGS"
     echo "PROG=\"$PROG\""
     echo "PWD=\"$PWD\""
     echo "CD=\"$CD\""
     echo "CWD=\"$CD\""
     echo "ERR=\"$ERR\""
     echo "PATH=\"$PATH\""
     echo "GLEEM=\"$GLEEM\""
     echo "export PROG PWD CD CWD ERR PATH GLEEM"
     sed -e 's/[	 ]*#.*$//' -e 's/^[	 ]*//' "$MKPATH_CONF" | \
     awk -f "$AWKFILE" ERR="$ERR" PROG="$PROG" - ) | \
    /bin/sh "$SHELL_ARGS" | eval "$UUWHO" -i -d "$TUUWHO_DATA" -
fi
case "$DBM" in
?*)	mv -f $TUUWHO_DATA.dir $UUWHO_DATA.dir	# begin race condition - XXX
	mv -f $TUUWHO_DATA.pag $UUWHO_DATA.pag	# end race condition - XXX
	;;
*)	mv -f $TUUWHO_DATA $UUWHO_DATA
	;;
esac

exit 0
