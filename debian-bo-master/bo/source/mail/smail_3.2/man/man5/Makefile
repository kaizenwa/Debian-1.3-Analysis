#! /bin/make -f
#
#ident	"@(#)smail/man/man5:RELEASE-3_2:Makefile,v 1.11 1996/02/27 20:07:32 woods Exp"
#
# Makefile for the smail section 5 man pages
#
#    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992  Ronald S. Karr
# 
# See the file COPYING, distributed with smail, for restriction
# and warranty information.

SHELL=/bin/sh
MAKE=make
AR=ar
LINT=lint
SRC_PREFIX=

MKDEPEND=${ROOT}/conf/lib/mkdepend.sh
MKDEFS=${ROOT}/conf/lib/mkdefs.sh
CHECKDEFS=${ROOT}/conf/lib/checkdefs.sh
MKDIRS=${ROOT}/conf/lib/mkdirs.sh
INSTM=${ROOT}/conf/lib/instm.sh
INST=${ROOT}/conf/lib/inst.sh
XEXEC=${SHELL} ${ROOT}/conf/lib/xexec.sh
DEFS_SH=defs.sh
DEFS_H=defs.h
DEFS_SED=defs.sed

ROOT=../..

MAN_SRC=smail.an smailconf.an smaildrct.an smailmeth.an smailqual.an \
	smailrtrs.an smailrtry.an smailtrns.an
MAN5=smail.5 smailconf.5 smaildrct.5 smailmeth.5 smailqual.5 \
	smailrtrs.5 smailrtry.5 smailtrns.5
MISC=Makefile
SRC=${MAN_SRC} ${MISC}
SED=sed
.SUFFIXES: .an .5

.an.5:
	${SED} -f ${DEFS_SED} $*.an > $*.5

all:	${MAN5}

${MAN5}: ${DEFS_SED}

mkdefs ${DEFS_SH} ${DEFS_H} ${DEFS_SED}: ${ROOT}/conf/EDITME
	ROOT=${ROOT} ${SHELL} ${MKDEFS}

${ROOT}/conf/EDITME: # cannot depend on anything!
	cd ${ROOT}/conf; ${MAKE} EDITME

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

install:

installman: all ${DEFS_SH}
	@. ./${DEFS_SH}; \
	   ${XEXEC} ${SHELL} ${MKDIRS} -m 0755 $$MAN5; \
	   case "$$MAN5_EXT" in \
	   5)	${XEXEC} ${SHELL} ${INSTM} -r -m 0444 $$MAN5 ${MAN5};; \
	   *)	for i in ${MAN5}; do \
			bn=`basename $$i .5`; \
			${XEXEC} ${SHELL} ${INST} -r -m 0444 $$i \
					 $$MAN5/$$bn.$$MAN5_EXT; \
		done;; \
	   esac

depend local_depend: check_defs
	${SHELL} ${MKDEPEND} Makefile
	. ./${DEFS_SH}; echo "$$DEFS_DEPEND" >> Makefile
	chmod -w Makefile

check_defs:
	SHELL=${SHELL} ROOT=${ROOT} ${SHELL} ${CHECKDEFS}

clean:
	rm -f ${DEFS_SH} ${DEFS_H} ${DEFS_SED}
	rm -f a.out core

clobber: clean
	rm -f ${MAN5}

# DO NOT REMOVE THIS LINE, OR "make depend" WILL NOT WORK
