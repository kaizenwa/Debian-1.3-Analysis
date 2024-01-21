#! /bin/sh
:
#ident	"@(#)smail/conf/lib:RELEASE-3_2:mkdefs.sh,v 1.46 1996/05/29 18:47:17 woods Exp"

#    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992  Ronald S. Karr
# 
# See the file COPYING, distributed with smail, for restriction
# and warranty information.

# Setup shell variables for use in makefiles and some shell scripts used
# in building smail.  The file EDITME is used to gather information
# specified by the site administrator.  Files under conf/os and conf/arch
# are used to learn about a particular operating system and architecture.
# All of the information learned is summarized in a dump to the files
# conf/defs.sh (a .-able shellscript), conf/defs.h, (an #include-able
# C header file) and conf/defs.sed (a sed script).

# The variable ROOT should be defined as a path to the root of the
# smail source directory.  This should be put in the environment before
# calling this shell script.

# simple variables that can be set in the EDITME, OS or ARCH files.
# If these variables are set in the EDITME file, then the version
# from the EDITME file should override.

# NOTE: The complete VARS string is too long for A/UX.  Split it into
#	two parts which are concatenated.
VARS="\
 ALIGNED_TYPE\
 ALIASES_FILE\
 ALIASES_REMOVE_SENDER\
 ALIASES_TYPE\
 AUTH_DOMAINS\
 BIT_PER_CHAR\
 BYTES_PER_ALIGN\
 CC\
 CHOWN\
 COMPRESS\
 COMP_FLAG\
 DOMAINS\
 DOT_Z\
 DRIVER_CONFIGURATION\
 FLOCK_MAILBOX\
 GATEWAY_NAMES\
 HOSTNAMES\
 LIB_DIR\
 LISTS_REMOVE_SENDER\
 LMAIL\
 LOCAL_MAIL_ADDRS\
 LOCAL_MAIL_COMMAND\
 LOCAL_MAIL_FILE\
 LOCKING_PROTOCOL\
 LOCK_BY_NAME\
 LOG_DIR\
 MAILBOX_DIR\
 MAN1\
 MAN1_EXT\
 MAN5\
 MAN5_EXT\
 MAN8\
 MAN8_EXT\
 MORE_HOSTNAMES\
"
VARS="$VARS\
 NEWALIASES\
 NEWS_SPOOL_DIR\
 NOBODY\
 PATH_ALIASES_FILE\
 PATH_ALIASES_TYPE\
 POINTER_TYPE\
 POSTMASTER\
 RANLIB\
 RETRY_DURATION\
 RETRY_INTERVAL\
 SECOND_CONFIG_FILE\
 SECURE_PATH\
 SENDER_ENV_VARIABLE\
 SMAIL_BIN_DIR\
 SMAIL_NAME\
 SMALL_MEMORY\
 SMTP_RECEIVE_COMMAND_TIMEOUT\
 SMTP_RECEIVE_MESSAGE_TIMEOUT\
 SPOOL_DIRS\
 STRIP\
 TMP_DIR\
 UNCOMPRESS\
 UNSHAR_MAP_DIR\
 USE_ASCII\
 USE_SYMLINKS\
 UTIL_BIN_DIR\
 UUCP_NAME\
 UUCP_SYSTEM_FILE\
 UUCP_ZONE\
 UUWHO_FILE\
 UUWHO_USE_DBM\
 VISIBLE_NAME\
 ZCAT\
 SOCKET_INCLUDES\
"

# list OS/ARCH vars, for these variables, the EDITME versions are appended
LISTS="\
 CFLAGS\
 CPPFLAGS\
 HAVE\
 INCLUDES\
 LDFLAGS\
 LIBS\
 MISC_DEFINES\
 MISC_UNDEFINES\
 NO_HAVE\
 OSLIBS\
 OSNAMES\
 OTHER_SMAIL_NAMES\
"

# lists to append together with newlines
NLISTS="\
 MISC_C_DEFINES\
 MISC_DEFS_DEPEND\
 MISC_SH_DEFINES\
"

# HAVE values which are required to exist in either the HAVE or
# NO_HAVE list.  If not in HAVE, these will be added to NO_HAVE.
HAVE_REQUIRE="\
"

# list vars that are set below, and *not* in EDITME/OS/ARCH, etc.
# but are set by this script from derrived values....
NOEDVARS="\
 UTIL_PATH\
"

# separators for list variables
sep=":$IFS"
save_IFS="$IFS"

# clear all EDITME/OS/ARCH variables
for v in $VARS $NOEDVARS $LISTS $NLISTS; do
	eval "case \"$v\" in ?*) $v=;; esac"
done

TEST_BASE="<not defined>"	# clearing TEST_BASE in EDITME has meaning

# determine how to echo without a newline
case "`echo -n`" in
-n)
	nonls=""
	nonle='\c'
	;;
*)
	nonls='-n'
	nonle=""
	;;
esac

# Establish the root of the smail source directory
RELATIVE_ROOT="$ROOT"
ROOT=`(cd "$ROOT"; pwd)`
FROM_ROOT=`pwd | sed -e 's,^'$ROOT'/,,' -e 's,^'$ROOT'$,,'`

# Read in the EDITME file, creating it if necessary

# smail EDITME file specified in environment variable
case "$SMAIL_EDITME" in
/* )
	EDITME="$SMAIL_EDITME"
	RELATIVE_EDITME="$EDITME"
	;;
?* )
	EDITME="$ROOT/$SMAIL_EDITME"
	RELATIVE_EDITME="\${ROOT}/$SMAIL_EDITME"
	;;
"" )
	EDITME="$ROOT/conf/EDITME"
	RELATIVE_EDITME='${ROOT}/conf/EDITME'
	;;
esac

case "$EDITME" in
*/./* | */../* )
	# build a regular pathname from /
	dn="`echo "$EDITME" | sed 's|/[^/]*$||'`"
	EDITME="`cd $dn; pwd`/`basename "$EDITME"`"
	;;
esac

# If the EDITME file does not exist, create it from the distributed version
if [ ! -f "$EDITME" ]; then
	echo "Copy $ROOT/conf/EDITME-dist to $EDITME ..."
	if cat < "$ROOT/conf/EDITME-dist" > "$EDITME"; then
		:
	else
		exit 1
	fi
fi

echo $nonls "Read $RELATIVE_EDITME ... $nonle" 1>&2
if [ ! -r $EDITME ]; then
	echo "Cannot open $EDITME" 1>&2
	exit 1
fi
. $EDITME

case "$TEST_BASE" in

"<not defined>" )
	UTIL_PATH='$UTIL_BIN_DIR:$SMAIL_BIN_DIR'
	TEST_BASE=""
	DONT_INSTALL=""
	;;

"" )
	SMAIL_BIN_DIR="$ROOT/src"
	SMAIL_NAME="$ROOT/src/smail"
	OTHER_SMAIL_NAMES=""
	LIB_DIR="$ROOT/lib"
	SPOOL_DIRS="$ROOT/spool"
	LOG_DIR="$ROOT/log"
	SECOND_CONFIG_FILE=""
	UTIL_BIN_DIR="$ROOT/util"
	UTIL_PATH="$ROOT/pd/getopt:$ROOT/pd/pathalias:$ROOT/pd/uuwho"
	UTIL_PATH="$UTIL_PATH:$ROOT/util:$SMAIL_BIN_DIR"
	TMP_DIR="/tmp"
	TEST_BASE="$ROOT"
	NEWALIASES=""
	DONT_INSTALL="yes"
	echo "Testing: \$ROOT/$FROM_ROOT/Makefile install rule is now disabled"
	;;

/* )
	SMAIL_BIN_DIR="$TEST_BASE/bin"
	SMAIL_NAME=""
	OTHER_SMAIL_NAMES=""
	LIB_DIR="$TEST_BASE/lib"
	SPOOL_DIRS="$TEST_BASE/spool"
	LOG_DIR="$TEST_BASE/log"
	SECOND_CONFIG_FILE=""
	UTIL_BIN_DIR="$TEST_BASE/util"
	UTIL_PATH="$UTIL_BIN_DIR:$SMAIL_BIN_DIR"
	TMP_DIR="/tmp"
	NEWALIASES=""
	DONT_INSTALL=""
	echo "Testing: \$ROOT/$FROM_ROOT/Makefile install will use $TEST_BASE"
	;;

* )
	SMAIL_BIN_DIR="$ROOT/$TEST_BASE/bin"
	SMAIL_NAME=""
	OTHER_SMAIL_NAMES=""
	LIB_DIR="$ROOT/$TEST_BASE/lib"
	SPOOL_DIRS="$ROOT/$TEST_BASE/spool"
	LOG_DIR="$ROOT/$TEST_BASE/log"
	SECOND_CONFIG_FILE=""
	UTIL_BIN_DIR="$ROOT/$TEST_BASE/util"
	UTIL_PATH="$UTIL_BIN_DIR:$SMAIL_BIN_DIR"
	TMP_DIR="/tmp"
	TEST_BASE="$ROOT/$TEST_BASE"
	NEWALIASES=""
	DONT_INSTALL=""
	echo "Testing: \$ROOT/$FROM_ROOT/Makefile install will use $TEST_BASE"
	;;

esac

# save EDITME versions of variables
for v in $VARS $LISTS $NLISTS; do
	eval "
		case \"\$$v\" in
		?*)
			EDITME_$v=\"\$$v\";
			$v=''
			;;
		esac
	     "
done

# Fill in defaults.  OS/ARCH files can override these.  EDITME file
# can override the defaults or the OS/ARCH files.
#
CC=cc
TMP_DIR='$SPOOL_DIRS/tmp'
SECURE_PATH=/bin:/usr/bin
POSTMASTER=root
RANLIB=:
CHOWN=chown
NEWS_SPOOL_DIR=/usr/spool/news

if [ -z "$EDITME_COMPRESS" ]; then
	COMPRESS=:
	COMP_FLAG=""
	UNCOMPRESS=true
	ZCAT=cat
	DOT_Z=""
fi

if [ "X${OS_TYPE}X" = "XX" ]; then
	echo "You must define OS_TYPE in $ROOT/conf/EDITME" 1>&2
	exit 1
fi

# Read the OS-specific information
echo $nonls "conf/os/$OS_TYPE ... $nonle" 1>&2
if [ ! -r "$ROOT/conf/os/$OS_TYPE" ]; then
	echo "Cannot open $ROOT/conf/os/$OS_TYPE" 1>&2
	exit 1
fi

. "$ROOT/conf/os/$OS_TYPE"

# Read the architecture-specific information
echo $nonls "conf/arch/$ARCH_TYPE ... $nonle" 1>&2
if [ ! -r $ROOT/conf/arch/$ARCH_TYPE ]; then
	echo "Cannot open $ROOT/conf/arch/$ARCH_TYPE" 1>&2
	exit 1
fi

. "$ROOT/conf/arch/$ARCH_TYPE"

# for simple variables, make EDITME versions override os and arch versions
for v in $VARS; do
	eval "
		case \"\$EDITME_$v\" in
		?*)
			$v=\"\$EDITME_$v\"
			;;
		esac
	     "
done

# re-eval this, as the default depends on things that may not have been
# set in the EDITME file. It isn't allowed in EDITME.
UTIL_PATH=`eval echo $UTIL_PATH`

# Special case: some variables related to compression may be empty, so
# if COMPRESS was specified in the EDITME file, set them all from the
# EDITME file variables.
if [ ! -z "$EDITME_COMPRESS" ]; then
	COMPRESS="$EDITME_COMPRESS"
	COMP_FLAG="$EDITME_COMP_FLAG"
	UNCOMPRESS="$EDITME_UNCOMPRESS"
	ZCAT="$EDITME_ZCAT"
	DOT_Z="$EDITME_DOT_Z"
fi

# for list variables, append the EDITME version
#
# NOTE: some lists require space separation, some require : separation
#	others don't care.  Use space separation by default.
for v in $LISTS; do
	case $v in
	MISC_DEFINES|OSNAMES|HAVE|NO_HAVE)
		listsep=:
		;;
	*)
	      listsep=' '
		;;
	esac
	eval "
		case \"\${${v}}\" in
		''|'[ 	]')
			${v}=\"\${EDITME_${v}}\"
			;;
		?*)
			if [ ! -z \"\${EDITME_${v}}\" ] ; then
				${v}=\"\${${v}}${listsep}\${EDITME_${v}}\";
			fi
			;;
	   	esac
	     "
done
for v in $NLISTS; do
	eval "
		case \"\${${v}}\" in
		'')
			${v}=\"\${EDITME_${v}}\"
			;;
		?*)
			if [ ! -z \"\${EDITME_${v}}\" ] ; then
				${v}=\"\${${v}}\${EDITME_${v}}\";
			fi
			;;
	   	esac
	     "
done

CFLAGS="$CFLAGS $CPPFLAGS"

# check for required HAVE/NO_HAVE values, and add values to NO_HAVE
# as needed
for v in $HAVE_REQUIRE; do
	found=no
	IFS="$sep"
	for v2 in $HAVE; do
		case "$v" in
		"$v2")
			found=yes
			break
			;;
		esac
	done
	for v2 in $NO_HAVE; do
		case "$v" in
		"$v2")
			found=yes
			break
			;;
		esac
	done
	IFS="$save_IFS"
	case "$found" in
	no)
		case "$NO_HAVE" in
		"")
			NO_HAVE="$v"
			;;
		*)
			NO_HAVE="$NO_HAVE $v"
			;;
		esac
		;;
	esac
done

case "$SPOOL_DIRS" in
*:* )
	MAIN_SPOOL_DIR=`expr "\$SPOOL_DIRS" : '\([^:]*\):.*'`
	;;
*   )
	MAIN_SPOOL_DIR="$SPOOL_DIRS"
	;;
esac
case "$LOG_DIR" in
"")
	LOG_DIR=$MAIN_SPOOL_DIR/log
	;;
esac
LOGFILE="$LOG_DIR/logfile"
PANICLOG="$LOG_DIR/paniclog"
OLD_LOGDIR="$LOG_DIR/OLD"

# Append .sort to $ALIASES_FILE for sorted files.
case "$ALIASES_TYPE" in
bsearch )
	ALIASES_FILE="$ALIASES_FILE.sort"
	;;
esac

case "$ALIASES_TYPE" in

yp | aliasyp)
	PATH_ALIASES_FILE="$ALIASES_FILE"
	;;

* )
	case "$ALIASES_FILE" in
	/*  )
		PATH_ALIASES_FILE="$ALIASES_FILE"
		;;
	*   )
		PATH_ALIASES_FILE="$LIB_DIR/$ALIASES_FILE"
		;;
	esac
	;;
esac

case "$PATHS_TYPE" in

yp | aliasyp)
	PATH_PATHS_FILE="$PATHS_FILE"
	;;

*)
	case "$PATHS_FILE" in
	/*  )
		PATH_PATHS_FILE="$PATHS_FILE"
		;;
	*   )
		PATH_PATHS_FILE="$LIB_DIR/$PATHS_FILE"
		;;
	esac
	;;
esac

case "$FORCE_PATHS_TYPE" in

yp | aliasyp)
	PATH_FORCE_PATHS_FILE="$FORCE_PATHS_FILE"
	;;

*)
	case "$FORCE_PATHS_FILE" in
	/*  )
		PATH_FORCE_PATHS_FILE="$FORCE_PATHS_FILE"
		;;
	*   )
		PATH_FORCE_PATHS_FILE="$LIB_DIR/$FORCE_PATHS_FILE"
		;;
	esac
	;;
esac

case "$FORCE_SMTP_TYPE" in

yp | aliasyp)
	PATH_FORCE_SMTP_FILE="$FORCE_SMTP_FILE"
	;;

*)
	case "$FORCE_SMTP_FILE" in
	/*  )
		PATH_FORCE_SMTP_FILE="$FORCE_SMTP_FILE"
		;;
	*   )
		PATH_FORCE_SMTP_FILE="$LIB_DIR/$FORCE_SMTP_FILE"
		;;
	esac
	;;
esac

# If a SMAIL_NAME is specified, add it to the OTHER_SMAIL_NAMES.
# Otherwise, create one that points to the main smail binary.
case "$SMAIL_NAME" in
"")
	SMAIL_NAME="$SMAIL_BIN_DIR/smail"
	;;
*)
	OTHER_SMAIL_NAMES="$OTHER_SMAIL_NAMES $SMAIL_NAME"
	;;
esac

# replace : with ' ' in the OTHER_SMAIL_NAMES variable
list=$OTHER_SMAIL_NAMES
OTHER_SMAIL_NAMES=""
IFS="$sep"
for i in $list; do
	case "$OTHER_SMAIL_NAMES" in
	?*)
		OTHER_SMAIL_NAMES="$OTHER_SMAIL_NAMES $i"
		;;
	*)
		OTHER_SMAIL_NAMES="$i"
		;;
	esac
done
IFS="$save_IFS"

echo "done" 1>&2

# if DRIVER_CONFIGURATION does not begin with /, put it under the
# conf/driver directory.
case "$DRIVER_CONFIGURATION" in
/* )
	:
	;;
*  )
	DRIVER_CONFIGURATION=$RELATIVE_ROOT/conf/driver/$DRIVER_CONFIGURATION
	;;
esac

# append OSLIBS onto the end of LIBS, so that makefiles only need refer
# to LIBS.  The reason for having two is that OSLIBS is intended to
# be libraries that are linked after other libraries.  By specifically
# ordering them here, writers of OS and EDITME files don't need to
# be so careful.
LIBS="$LIBS $OSLIBS"

# determine how to get the dbm we need
case ":$HAVE:" in
*:DBM:*)
	:
	;;
*:NDBM:*)
	:
	;;
*)
	DBM_INCLUDES="-I$RELATIVE_ROOT/pd/sdbm"
	DBM_LIB="$RELATIVE_ROOT/pd/sdbm/sdbm.a"
	MISC_DEFINES="$MISC_DEFINES:SDBM"
	;;
esac

# If no LOCAL_MAIL_FILE is specified, derive it from MAILBOX_DIR
case "$LOCAL_MAIL_FILE" in
"")
	LOCAL_MAIL_FILE="$MAILBOX_DIR/\${lc:user}"
	;;
esac

# If the user specifies a LOCAL_MAIL_ADDRS, then they obviously want
# to use it, so do so
if [ -n "$LOCAL_MAIL_ADDRS" ] ; then
	DEFINE_LOCAL_MAIL_ADDRS="#define LOCAL_MAIL_ADDRS	\"$LOCAL_MAIL_ADDRS\""
fi

# If the user specifies a LOCAL_MAIL_COMMAND, then they obviously want
# to use it, so do so
if [ -n "$LOCAL_MAIL_COMMAND" ] ; then
	DEFINE_LOCAL_MAIL_COMMAND="#define LOCAL_MAIL_COMMAND	\"$LOCAL_MAIL_COMMAND\""
fi

# Build the shell include file
echo $nonls "Build \$ROOT/$FROM_ROOT/defs.sh ... $nonle" 1>&2

exec > "defs.sh"
case $? in
0)
	:
	;;
*)
	exit 1
	;;
esac

cat <<EOF
# DO NOT EDIT THIS FILE DIRECTLY, IT IS CREATED AUTOMATICALLY
# FROM THE SCRIPT $ROOT/conf/lib/mkdefs.sh.  IF YOU MAKE
# CHANGES TO THIS FILE THEY ARE LIKELY TO GO AWAY.  SEE THE FILE
# $EDITME FOR INFORMATION ON HOW TO CONTROL
# THE CONTENTS OF THIS FILE.

FROM_ROOT='$FROM_ROOT'
DEFS_DEPEND=\
'defs.sh defs.h defs.sed: $RELATIVE_EDITME
defs.sh defs.h defs.sed: \${ROOT}/conf/lib/arch.h
defs.sh defs.h defs.sed: \${ROOT}/conf/lib/mkdefs.sh
defs.sh defs.h defs.sed: \${ROOT}/conf/os/$OS_TYPE
defs.sh defs.h defs.sed: \${ROOT}/conf/arch/$ARCH_TYPE
version.sh version.h version.sed: \${ROOT}/conf/lib/mkversion.sh
version.sh version.h version.sed: \${ROOT}/level
$MISC_DEFS_DEPEND'
EOF

# Write out the simple variables
for v in $VARS $NOEDVARS $LISTS $NLISTS; do
	eval "
		echo \"\${v}='\${${v}}'\";
	     "
done


# write out the positive boolean list variables
(
	IFS="$sep"
	for i in $HAVE; do
		echo HAVE_$i=yes
	done
)
(
	IFS="$sep"
	for i in $OSNAMES; do
		echo $i=yes
	done
)

# clear the negative boolean list variables
(
	IFS="$sep"
	for i in $NO_HAVE; do
		echo HAVE_$i=
	done
)

# write the extensible set of variables that may have values
(
	IFS="$sep"
	for i in $MISC_DEFINES; do
		echo "$i"
	done | sed -n -e 's/^\(.[^=]*\)$/\1=yes/p' -e 's/^\(.[^=]*\)=\(.*\)/\1='\2'/p'
)

cat <<EOF
DBM_INCLUDES='$DBM_INCLUDES'
DBM_LIB='$DBM_LIB'
LOGFILE='$LOGFILE'
PANICLOG='$PANICLOG'
OLD_LOGDIR='$OLD_LOGDIR'
MAIN_SPOOL_DIR='$MAIN_SPOOL_DIR'
EDITME='$EDITME'
OS_TYPE='$OS_TYPE'
$MISC_SH_DEFINES
EOF

# Build the C include file
echo $nonls "defs.h ... $nonle" 1>&2

exec > "defs.h"
case $? in
0)
	:
	;;
*)
	exit 1
	;;
esac

# Write out the file header and first set of #define's
cat <<EOF
/*
 * DO NOT EDIT THIS FILE DIRECTLY, IT IS CREATED AUTOMATICALLY
 * FROM THE SCRIPT $ROOT/conf/mkdefs.sh.  IF YOU MAKE
 * CHANGES TO THIS FILE THEY ARE LIKELY TO GO AWAY.  SEE THE FILE
 * $EDITME FOR INFORMATION ON HOW TO CONTROL
 * THE CONTENTS OF THIS FILE.
 */

#define MAILBOX_DIR		"$MAILBOX_DIR"
#define CONSOLE			"$CONSOLE"
$DEFINE_LOCAL_MAIL_ADDRS
$DEFINE_LOCAL_MAIL_COMMAND
#define LOCAL_MAIL_FILE		"$LOCAL_MAIL_FILE"
$LOCKING_PROTOCOL
$DECLARE_STRINGS
EOF

# Handle string variables which should be turned off with a NO_ prefix
cat <<EOF | sed -n \
		-e 's/\([A-Z0-9_]*\)=\(..*\)/#define \1		"\2"/p' \
		-e 's/\([A-Z0-9_]*\)=$/#define NO_\1/p'
ALIASES_FILE=$ALIASES_FILE
PATH_ALIASES_FILE=$PATH_ALIASES_FILE
ALIASES_PROTO=$ALIASES_TYPE
PATHS_FILE=$PATHS_FILE
PATH_PATHS_FILE=$PATH_PATHS_FILE
PATHS_PROTO=$PATHS_TYPE
FORCE_PATHS_FILE=$FORCE_PATHS_FILE
PATH_FORCE_PATHS_FILE=$PATH_FORCE_PATHS_FILE
FORCE_PATHS_PROTO=$FORCE_PATHS_TYPE
FORCE_SMTP_FILE=$FORCE_SMTP_FILE
PATH_FORCE_SMTP_FILE=$PATH_FORCE_SMTP_FILE
FORCE_SMTP_PROTO=$FORCE_SMTP_TYPE
EOF

# Handle string variables which should revert to defaults if undefined
cat <<EOF | sed -n \
		-e 's/\([A-Z0-9_]*\)=\(..*\)/#define \1		"\2"/p' \
		-e 's/\([A-Z0-9_]*\)=$/#undef \1/p'
NOBODY=$NOBODY
SMAIL_LIB_DIR=$LIB_DIR
SMAIL_UTIL_DIR=$UTIL_BIN_DIR
UUCP_SYSTEM_FILE=$UUCP_SYSTEM_FILE
SPOOL_DIRS=$SPOOL_DIRS
UNSHAR_MAP_DIR=$UNSHAR_MAP_DIR
LOGFILE=$LOGFILE
OLD_LOGDIR=$OLD_LOGDIR
PANIC_LOG=$PANICLOG
SMAIL=$SMAIL_NAME
POSTMASTER_ADDRESS=$POSTMASTER
UUWHO_FILE=$UUWHO_FILE
NEWS_SPOOL_DIR=$NEWS_SPOOL_DIR
EOF

# Handle non-string variables which should revert to defaults if undefined
cat <<EOF | sed -n \
		-e 's/\([A-Z0-9_]*\)=\(..*\)/#define \1		(\2)/p' \
		-e 's/\([A-Z0-9_]*\)=$/#undef \1/p'
RETRY_INTERVAL=$RETRY_INTERVAL
RETRY_DURATION=$RETRY_DURATION
EOF

# Handle NULL or string variables
cat <<EOF | sed -n \
		-e 's/\([A-Z0-9_]*\)=\(..*\)/#define \1		"\2"/p' \
		-e 's/\([A-Z0-9_]*\)=$/#define \1		NULL/p'
EDITME=$EDITME
OS_TYPE=$OS_TYPE
RELATIVE_EDITME=$RELATIVE_EDITME
MAIN_SPOOL_DIR=$MAIN_SPOOL_DIR
HOSTNAMES=$HOSTNAMES
MORE_HOSTNAMES=$GATEWAY_NAMES
VISIBLE_DOMAINS=$DOMAINS
UUCP_NAME=$UUCP_NAME
VISIBLE_NAME=$VISIBLE_NAME
SECOND_CONFIG_FILE=$SECOND_CONFIG_FILE
SECURE_PATH=$SECURE_PATH
SENDER_ENV_VARIABLE=$SENDER_ENV_VARIABLE
SMAIL_TMP_DIR=$TMP_DIR
LMAIL=$LMAIL
EOF

# Handle empty or string variables
cat <<EOF | sed -n \
		-e 's/\([A-Z0-9_]*\)=\(.*\)/#define \1		"\2"/p'
FROM_ROOT=$FROM_ROOT
EOF

# Handle TRUE or FALSE variables
cat <<EOF | sed -n \
		-e 's/\([A-Z0-9_]*\)=..*/#define \1		TRUE/p' \
		-e 's/\([A-Z0-9_]*\)=$/#define \1		FALSE/p'
LOCK_BY_NAME=$LOCK_BY_NAME
FLOCK_MAILBOX=$FLOCK_MAILBOX
EOF

# Handle on or off variables
cat <<EOF | sed -n \
		-e 's/\([A-Z0-9_]*\)=..*/#define \1/p' \
		-e 's/\([A-Z0-9_]*\)=$/#undef \1/p'
SMALL_MEMORY=$SMALL_MEMORY
USE_ASCII=$USE_ASCII
UUWHO_USE_DBM=$UUWHO_USE_DBM
UUCP_ZONE=$UUCP_ZONE
ALIASES_REMOVE_SENDER=$ALIASES_REMOVE_SENDER
LISTS_REMOVE_SENDER=$LISTS_REMOVE_SENDER
EOF

# write out the positibe boolean list variables
(
	IFS="$sep"
	for i in $HAVE; do
		echo "#define HAVE_$i"
	done
)
(
	IFS="$sep"
	for i in $OSNAMES; do
		echo "#define $i"
	done
)

# clear the negative boolean list variables
(
	IFS="$sep"
	for i in $NO_HAVE; do
		echo "#undef HAVE_$i"
	done
)

# write the extensible set of variables that may have values
(
	IFS="$sep"
	for i in $MISC_DEFINES; do
		echo "$i"
	done | sed -n -e 's/^\(.[^=]*\)$/#define \1/p' \
		-e 's/^\(.[^=]*\)=\(.*\)/#define \1		\2/p'
)

if [ -z "$SOCKET_INCLUDES" ]; then
	SOCKET_INCLUDES=`cat "$ROOT/conf/lib/sockets.h"`
fi

cat <<EOF
#define POINTER_TYPE	$POINTER_TYPE
#define ALIGNED_TYPE	$ALIGNED_TYPE
#define BITS_PER_CHAR	$BITS_PER_CHAR
#define BYTES_PER_ALIGN	$BYTES_PER_ALIGN
$MISC_C_DEFINES
#if defined(__STDC__) && !defined(NO_ANSI_C)
# ifndef ANSI_C
#  define ANSI_C 1
# endif
#endif
#include "$RELATIVE_ROOT/conf/lib/arch.h"
#ifdef NEED_SOCKETS
#ifdef HAVE_BSD_NETWORKING
$SOCKET_INCLUDES
#endif	/* HAVE_BSD_NETWORKING */
#endif	/* NEED_SOCKETS */
EOF

# Build the sed change script
echo $nonls "defs.sed ... $nonle" 1>&2

exec > "defs.sed"
case $? in
0)
	:
	;;
*)
	exit 1
	;;
esac

case "$MISC_SED_DEFINES" in
?*)
	cat <<EOF
$MISC_SED_DEFINES
EOF
	;;
esac

cat <<EOF
s|X_LOGFILE_X|$LOGFILE|g
s|X_PATHS_FILE_X|$PATHS_FILE|g
s|X_FORCE_PATHS_FILE_X|$FORCE_PATHS_FILE|g
s|X_FORCE_SMTP_FILE_X|$FORCE_SMTP_FILE|g
s|X_PANICLOG_X|$PANICLOG|g
s|X_OLD_LOGDIR_X|$OLD_LOGDIR|g
s|X_MAIN_SPOOL_DIR_X|$MAIN_SPOOL_DIR|g
s|X_EDITME_X|$EDITME|g
s|X_OS_TYPE_X|$OS_TYPE|g
EOF

# write out the simple variables.  Ignore variables with newlines or | chars

if [ ! -z "$CASE_NO_NEWLINES" ]; then
	# HP-UX 7.0 cannot handle newlines within a case pattern.
	# Thus, we will use wc to find newlines, instead.

	for v in $VARS $NOEDVARS; do
		eval "
			if [ \`echo \"\$$v\" | wc -l\` -eq 1 ]; then
				case \"\$$v\" in
				*'|'*|*'\\'*)
					:
					;;
				*)
					echo \"s|X_${v}_X|\$$v|g\"
					;;
				esac
			fi
		     "
	done
else
	nl='
'
	for v in $VARS $NOEDVARS; do
		eval "
			case \"\$$v\" in
			*\$nl*|*'|'*|*'\\'*)
				:
				;;
			*)
				echo \"s|X_${v}_X|\$$v|g\"
				;;
			esac
		     "
	done
fi

# clear the negative boolean list variables
(
	IFS="$sep"
	for i in $NO_HAVE; do
		echo "s|X_HAVE_${i}_X||g"
	done
)

# write out the positive boolean list variables
(
	IFS="$sep"
	for i in $HAVE; do
		echo "s|X_HAVE_${i}_X|yes|g"
	done
)
(
	IFS="$sep"
	for i in $OSNAMES; do
		echo "s|X_${i}_X|yes|g"
	done
)

# If this OS cannot correctly use #!/bin/sh at the beginning of bourne
# shell scripts, then convert them not to do this.
(
	IFS="$sep"
	sed_hash_bang=true
	for i in $HAVE; do
		case "$i" in
		HASH_BANG)
			sed_hash_bang=false
			;;
		esac
	done
	if $sed_hash_bang; then
		echo '1s|^#! */bin/sh|: /bin/sh|'
	fi
)

echo "done." 1>&2

exit 0
