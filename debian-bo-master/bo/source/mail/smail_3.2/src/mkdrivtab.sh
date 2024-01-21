#! /bin/sh
:
#ident	"@(#)smail/src:RELEASE-3_2:mkdrivtab.sh,v 1.11 1996/02/26 18:08:25 woods Exp"

# Script to create drivertab.c and the driver makefile directories
# from a driver configuration file.  This script makes heavy use of
# the awk script mkdriv.awk to actually process the contents of the
# configuration file.  The awk script produces output tokens which
# tell this script how to operate.
#
# The format of a driver file is a collection of one line records
# describing the drivers which are to be linked to the smail binary.
# Additional source and header files can be specified for use in a
# particular library file using the special driver name "library".
#
# Drivers come in four types:  director, router, transport and lookup
# drivers.  A line describing a driver is of the following form:
#
#	type name [modifiers ...]
#
# where type is one of "director", "router", "transport" or "lookup",
# and where name is the symbolic name for the driver, for reference
# from run-time configuration files.
#
# There can be modifiers applied to an entry.  These modifiers can be
# one of the following:
#
#	source=name.c	This modifier declares that the source for the
#			driver is in the file "name.c" in the source
#			directory for the specific driver type.  The
#			default source file name is the symbolic name
#			of the driver with a suffix of ".c".
#
#	header=name	This modifier declares that the header file
#			associated with the driver is in the file
#			"name" in the source directory for the
#			specified driver type.  By default lookup
#			drivers do not have header files, while
#			director, router and transport drivers have
#			header files with the basename of the source
#			file and a suffix of ".h".
#
#	nocache		This modifier declares that the driver does
#			not have a "cache" entry point.  This does not
#			apply to lookup drivers.
#
#	nofinish	This modifier declares that the driver does
#			not have a "finish" entry point.  This does
#			not apply to lookup drivers.
#
#	nobuilder	This modifier declares that the driver does
#			not have a "builder" entry point.  This does
#			not apply to lookup drivers.
#
# Additional source and header files for a particular driver library
# can be specified using a line of the form:
#
#	type library [name.c | name.h] ...
#
# where type specifies the driver library, library is the keyword
# "library", and where name.c specifies a source file in the driver
# source directory, and where name.h specifies a header file in the
# driver source directory.
#
# Blanks lines are okay, and comments can be started with a '#'
# character and continue until the end of the line.
#
# The following is a simple example of a driver file:
#
#	# director drivers
#	director  aliasfile
#	director  forwardfile nocache nofinish source=fwdfile.c
#	director  user	      nocache nofinish
#	director  library     dtlib.c dtlib.h
#
#	# router drivers
#	router    pathalias
#	router	  uuname      # the command output may be cached
#	router	  smarthost   nocache nofinish
#	router	  library     rtlib.c rtlib.h
#
#	# transport drivers
#	transport pipe	      nocache nofinish
#	transport appendfile  nocache nofinish
#	transport library     tplib.c tplib.h
#	transport library     bsmtp.c bsmtp.h

if [ $# -gt 0 ]; then
	driver_file=$1
	exec < $driver_file
else
	driver_file="<standard-input>"
fi

sed -e 's/#.*//' -e '/^[ 	]*$/d' |
    if [ -f mkdriv.perl ]; then
	perl mkdriv.perl
    else
	awk -f mkdriv.awk
    fi |
    sed	-e '/%.*%NULL/d'			\
	-e 's/%CC%/extern void /'		\
	-e 's/%DD%/extern struct addr */'	\
	-e 's/%RD%/extern void /'		\
	-e 's/%PP%/extern void /'		\
	-e 's/%TD%/extern void /'		\
	-e 's/%VV%/extern void /'		\
	-e 's/%FF%/extern void /'		\
	-e 's/%BB%/extern char */'		\
	-e 's/%LO%/extern int /'		\
	-e 's/%LC%/extern void /'		\
	-e 's/%LL%/extern int /' |
    (
	if [ -f drivertab.c ]; then
		echo "Build drivertab.c, backup in .drivertab.c" 1>&2
		mv drivertab.c .drivertab.c
	else
		echo "Build drivertab.c ..." 1>&2
	fi
	outfile=drivertab.c
	cat <<EOF > $outfile
/*
 * drivertab.c:
 *	define the available director, router, transport and lookup
 *	drivers for use by smail.
 *
 * THIS FILE IS GENERATED AUTOMATICALLY BY THE SCRIPT $0 FROM
 * THE DRIVER CONFIGURATION FILE $driver_file
 * MAKE CHANGES TO THE DRIVER CONF FILE AND REBUILD RATHER THAN EDITING
 * THIS FILE DIRECTLY.
 */

#include <stdio.h>
#include "defs.h"
#include "smail.h"
#include "addr.h"
#include "direct.h"
#include "route.h"
#include "transport.h"

EOF

	while read a b; do
		case "$a" in
		%makefile_start)
			outfile=$b/Makefile
			if [ -f $outfile ]; then
			    echo "Build $outfile, backup in .Makefile" 1>&2
			    mv $outfile $b/.Makefile
			else
			    echo "Build $outfile ..." 1>&2
			fi
			cat <<EOF > $outfile
# Makefile for the $b driver library in smail

# THIS FILE IS GENERATED AUTOMATICALLY BY THE SCRIPT $0 FROM
# THE DRIVER CONFIGURATION FILE $driver_file
# MAKE CHANGES TO THE DRIVER CONF FILE AND REBUILD RATHER THAN EDITING

SHELL=/bin/sh
MAKE=make
GET=sccs get
AR=ar
LINT=lint
CC=cc
CLEAN=sccs clean
SRC_PREFIX=
ROOT=../..
MKDEPEND=\${ROOT}/conf/lib/mkdepend.sh
MKDEFS=\${ROOT}/conf/lib/mkdefs.sh
CHECKDEFS=\${ROOT}/conf/lib/checkdefs.sh
XEXEC=\${SHELL} \${ROOT}/conf/lib/xexec.sh
DEFS_SH=defs.sh
DEFS_H=defs.h
DEFS_SED=defs.sed

EOF
			;;
		%makefile_end)
			cat <<\EOF >> $outfile
SRC=${CSRC} ${HSRC}

.c.o:
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS $$INCLUDES -c $*.c

all:	${TARGET}

lint:	${CSRC} ${DEFS_SH}
	@. ./${DEFS_SH}; ${XEXEC} ${LINT} ${LINTFLAGS} ${CSRC}

${TARGET}: ${OBJ} Makefile ${DEFS_SH}
	rm -f ${TARGET}
	${AR} cr ${TARGET} ${OBJ}
	@. ./${DEFS_SH}; ${XEXEC} $$RANLIB ${TARGET}

csrc:;	@echo ${CSRC}
hsrc:;	@echo ${HSRC}

sources: ${SRC}

${SRC}:
	${GET} $@

mkdefs ${DEFS_H} ${DEFS_SH} ${DEFS_SED}:
	ROOT=${ROOT} ${SHELL} ${MKDEFS}

${OBJ}:	${DEFS_SH}

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

depend:	${SRC} check_defs
	@. ./${DEFS_SH}; ${XEXEC} ${SHELL} ${MKDEPEND} $$CPPFLAGS $$INCLUDES \
		Makefile ${CSRC}
	. ./${DEFS_SH}; echo "$$DEFS_DEPEND" >> Makefile; \
	chmod -w Makefile

check_defs:
	SHELL=${SHELL} ROOT=${ROOT} ${SHELL} ${CHECKDEFS}

clean:
	rm -f ${DEFS_SH} ${DEFS_H} ${DEFS_SED}
	rm -f a.out core ${OBJ}
	rm -f ${TARGET}

clobber: clean
	rm -f .${DEFS_SH} .${DEFS_H} .${DEFS_SED} .Makefile
	rm -f Makefile

nuke:   clobber
	-${CLEAN}

# DO NOT REMOVE THIS LINE OR "make depend" WILL NOT WORK
EOF
			;;
		%drop)	echo "    $b" >> $outfile;;
		*)	echo "$a $b" >> $outfile;;
		esac
	done
    )
