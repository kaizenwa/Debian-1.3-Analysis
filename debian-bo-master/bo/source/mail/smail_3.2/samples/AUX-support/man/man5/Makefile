#!/bin/make -f
# @(#) Makefile,v 1.6 1992/07/11 11:58:41 tron Exp
# Makefile for the smail section 5 man pages
#
#    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992  Ronald S. Karr
# 
# See the file COPYING, distributed with smail, for restriction
# and warranty information.
#
# Edited:
#	Robert B. Denny
#	denny@alisa.com
#
#	Changes for A/UX V2, man page handling is quite different.
#	Added "ps" to create PostScript flavor files, "print" to
#	print the PostScript pages via atprint (e.g.) LaserWriter.
#
SHELL=/bin/sh
MAKE=make
GET=sccs get
AR=ar
LINT=lint
CLEAN=sccs clean
MK=${MAKE} -${MAKEFLAGS}
SRC_PREFIX=
MKDEPEND=${ROOT}/conf/lib/mkdepend.sh
MKDEFS=${ROOT}/conf/lib/mkdefs.sh
CHECKDEFS=${ROOT}/conf/lib/checkdefs.sh
MKVERSION=${ROOT}/conf/lib/mkversion.sh
MKDIRS=${ROOT}/conf/lib/mkdirs.sh
INSTM=${ROOT}/conf/lib/instm.sh
INST=${ROOT}/conf/lib/inst.sh
XEXEC=${SHELL} ${ROOT}/conf/lib/xexec.sh
DEFS_SH=defs.sh
DEFS_H=defs.h
DEFS_SED=defs.sed
VERSION_SH=version.sh
VERSION_H=version.h
VERSION_SED=version.sed

ROOT=../..
MAN_SRC=smail.an
MAN_LIST=smail.5.z
MAN5=smail.5
MAN5PS=smail.PS
MISC=Makefile
SRC=${MAN_SRC} ${MISC}
SED=sed
TROFF=troff
NROFF=nroff
PACK=pack
.SUFFIXES: .an .5 .PS

.an.5:
	${SED} -f ${DEFS_SED} $*.an > $*.5

.5.PS:
	${TROFF} -Tpsc -man $*.5 | psdit > $*.PS

all:	${MAN5}

${MAN5}: ${DEFS_SED}

${DEFS_SH} ${DEFS_H} ${DEFS_SED}:
	ROOT=${ROOT} ${SHELL} ${MKDEFS}

${VERSION_SH} ${VERSION_H} ${VERSION_SED}:
	ROOT=${ROOT} ${SHELL} ${MKVERSION}

ps:	${MAN5} ${MAN5PS}

print:	${MAN5} ${MAN5PS}
	@for i in ${MAN5PS}; do atprint < $$i; done

sources: ${SRC}

${SRC}:
	${GET} $@

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

install:

installman: all ${DEFS_SH}
	@. ./${DEFS_SH}; \
	   for i in ${MAN5}; do \
		echo "Installing $$i" ; \
		../auxman $$i $$MAN5/$$i.z ; \
	   done

uninstallman:
	@. ./${DEFS_SH} ; \
	   for i in ${MAN_LIST}; do \
		../rmvman $$MAN5/$$i ; \
	   done

depend local_depend: check_defs
	${SHELL} ${MKDEPEND} Makefile
	. ./${DEFS_SH}; echo "$$DEFS_DEPEND" >> Makefile
	chmod -w Makefile

check_defs:
	ROOT=${ROOT} ${SHELL} ${CHECKDEFS}

clean:
	rm -f ${DEFS_SH} ${DEFS_H} ${DEFS_SED}
	rm -f a.out core

clobber: clean
	rm -f ${MAN5}
	rm -f ${MAN5Z}
	rm -f ${MAN5PS}

nuke:	clobber
	-${CLEAN}
	-${GET} Makefile

# DO NOT REMOVE THIS LINE, OR "make depend" WILL NOT WORK
# Miscellaneous dependencies
defs.sh defs.h defs.sed: ${ROOT}/conf/EDITME
defs.sh defs.h defs.sed: ${ROOT}/conf/lib/arch.h
defs.sh defs.h defs.sed: ${ROOT}/conf/lib/mkdefs.sh
defs.sh defs.h defs.sed: ${ROOT}/conf/os/aux2.0
defs.sh defs.h defs.sed: ${ROOT}/conf/arch/32bit
version.sh version.h version.sed: ${ROOT}/conf/lib/mkversion.sh
version.sh version.h version.sed: ${ROOT}/level
version.sh version.h version.sed: ${ROOT}/patchnum

