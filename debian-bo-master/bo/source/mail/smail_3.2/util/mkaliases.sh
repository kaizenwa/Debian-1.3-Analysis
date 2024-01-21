#! /bin/sh
:
#ident	"@(#)smail/src:RELEASE-3_2:mkaliases.sh,v 1.9 1996/05/27 18:29:41 woods Exp"
#
#    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992  Ronald S. Karr
# 
# See the file COPYING, distributed with smail, for restriction
# and warranty information.

# mkaliases - create an aliases database for a smail aliasfile director
#
# Usage: mkaliases [alias_file]
#
# Rebuilds the aliasfile X_PATH_ALIASES_FILE_X, or the argument, if given.
#
# Different types for the aliasfile are rebuilt in different ways.
# In the case of lsearch, verification is performed on the file,
# but no other changes are made.

PATH='X_UTIL_PATH_X:X_SECURE_PATH_X'; export PATH

# XXX FIXME!!!  this should be derived from current configuration....
#
ALIASES_TYPE='X_ALIASES_TYPE_X'

if [ $# -eq 0 ] ; then
	# XXX FIXME?  this should be derived from current configuration....
	ALIASES_FILE='X_PATH_ALIASES_FILE_X'
elif [ $# -eq 1 ] ; then
	if [ -w "$1" ] ; then
		ALIASES_FILE="$1"
	else
		echo "$0: '$1' does not appear to be a regular writable file." 1>&2
		exit 1
	fi
else
	echo "Usage: $0 [alias_file]" 1>&2
	exit 2
fi

# NOTE: list of types defined in lookup.c:protos[]
#
case "$ALIASES_TYPE" in
aliasyp | yp | nialias | nisplus )
	mkline "$ALIASES_FILE" | mkdbm -y -v -o "$ALIASES_FILE"
	;;
dbm )
	mkline "$ALIASES_FILE" | mkdbm -f -v -o "$ALIASES_FILE"
	;;
bsearch )
	ALIASES_DIR=`dirname "$ALIASES_FILE"`
	case "$ALIASES_FILE" in
	*.sort)
		TEXT_FILE="`basename "$ALIASES_FILE" .sort`"
		;;
	* )
		TEXT_FILE="`basename "$ALIASES_FILE"`".txt
		;;
	esac
	mkline "$ALIASES_DIR/$TEXT_FILE" | wc -lc | awk '{ print $1 " entries, " $2 " bytes total" }'
	rc=$?
	if [ $rc -ne 0 ] ; then
		echo "$0: not rebuilding '$ALISES_FILE'." 1>&2
		exit $rc
	fi
	mkline "$ALIASES_DIR/$TEXT_FILE" | mksort -f > "$ALIASES_DIR/.$TEXT_FILE"
	mv -f "$ALIASES_DIR/.$TEXT_FILE" "$ALIASES_FILE"
	;;
lsearch )
	mkline "$ALIASES_FILE" | wc -lc | awk '{ print $1 " entries, " $2 " bytes total" }'
	;;
* )
	echo "$0: unknown file type, $ALIASES_TYPE, for $ALIASES_FILE" 1>&2
	exit 1
	;;
esac

exit 0
