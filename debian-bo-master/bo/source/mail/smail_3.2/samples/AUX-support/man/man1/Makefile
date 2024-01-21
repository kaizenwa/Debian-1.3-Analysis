#!/bin/make -f
# @(#) Makefile,v 1.6 1992/07/11 11:58:39 tron Exp
# Makefile for the smail section 1 man pages
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
MAN_SRC=pathto.an uuwho.an
MAN_LIST=pathto.1.z uuwho.1.z
PATHTO_LINKS=uupath.1.z optto.1.z
MAN1=pathto.1 uuwho.1 
MAN1PS=pathto.PS uuwho.PS
MISC=Makefile
SRC=${MAN_SRC} ${MISC}
SED=sed
TROFF=troff
NROFF=nroff
PACK=pack

.SUFFIXES: .an .1 .PS

.an.1:
	${SED} -f ${DEFS_SED} $*.an > $*.1

.1.PS:
	${TROFF} -Tpsc -man $*.1 | psdit > $*.PS


all:	${MAN1}

${MAN1}: ${DEFS_SED}

${DEFS_SH} ${DEFS_H} ${DEFS_SED}:
	ROOT=${ROOT} ${SHELL} ${MKDEFS}

${VERSION_SH} ${VERSION_H} ${VERSION_SED}:
	ROOT=${ROOT} ${SHELL} ${MKVERSION}

ps:	${MAN1} ${MAN1PS}

print:	${MAN1} ${MAN1PS}
	@for i in ${MAN1PS}; do atprint < $$i; done

sources: ${SRC}

${SRC}:
	${GET} $@

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

install:

installman: all ${DEFS_SH}
	@. ./${DEFS_SH}; \
	   for i in ${MAN1}; do \
		echo "Installing $$i" ; \
		../auxman $$i $$MAN1/$$i.z ; \
	   done ; \
	   for i in ${PATHTO_LINKS} ; do \
		echo "Linking pathto.1 with $$i" ; \
		../linkman $$MAN1/pathto.1.z $$MAN1/$$i ; \
	   done

uninstallman:
	@. ./${DEFS_SH} ; \
	   for i in ${MAN_LIST} ${PATHTO_LINKS} ; do \
		../rmvman $$MAN1/$$i ; \
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
	rm -f ${MAN1}
	rm -f ${MAN1PS}

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

