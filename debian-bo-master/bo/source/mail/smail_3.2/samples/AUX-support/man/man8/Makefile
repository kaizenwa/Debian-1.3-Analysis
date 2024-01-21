#!/bin/make -f
# @(#) Makefile,v 1.6 1992/07/11 11:58:44 tron Exp
# Makefile for the smail section 8 man pages
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
MAN_SRC=mkdbm.an mkline.an mksort.an pathalias.an smail.an mkpath.an \
	mkaliases.an pathmerge.an checkerr.an getmap.an mkhpath.an \
	savelog.an mkuuwho.an
SMAIL_LINKS=mailq.8.z sendmail.8.z runq.8.z rmail.8.z rsmtp.8.z smtpd.8.z
MAN8=mkdbm.8 mkline.8 mksort.8 pathalias.8 smail.8 mkpath.8 \
	mkaliases.8 pathmerge.8 checkerr.8 getmap.8 mkhpath.8 \
	savelog.8 mkuuwho.8
MAN8PS=mkdbm.PS mkline.PS mksort.PS pathalias.PS smail.PS mkpath.PS \
	mkaliases.PS pathmerge.PS checkerr.PS getmap.PS mkhpath.PS \
	savelog.PS mkuuwho.PS
MAN8Z=mkdbm.8.z mkline.8.z mksort.8.z pathalias.8.z smail.8.z mkpath.8.z \
	mkaliases.8.z pathmerge.8.z checkerr.8.z getmap.8.z mkhpath.8.z \
	savelog.8.z mkuuwho.8.z
MISC=Makefile
SRC=${MAN_SRC} ${MISC}
SED=sed
TROFF=troff
NROFF=nroff
PACK=pack
.SUFFIXES: .an .8 .PS

.an.8:
	${SED} -f ${DEFS_SED} $*.an > $*.8

.8.PS:
	${TROFF} -Tpsc -man $*.8 | psdit > $*.PS

all:	${MAN8}

${MAN8}: ${DEFS_SED}

${DEFS_SH} ${DEFS_H} ${DEFS_SED}:
	ROOT=${ROOT} ${SHELL} ${MKDEFS}

${VERSION_SH} ${VERSION_H} ${VERSION_SED}:
	ROOT=${ROOT} ${SHELL} ${MKVERSION}

ps:	${MAN8} ${MAN8PS}

print:	${MAN8} ${MAN8PS}
	@for i in ${MAN8PS}; do atprint < $$i; done

sources: ${SRC}

${SRC}:
	${GET} $@

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

install:

installman: all ${DEFS_SH}
	@. ./${DEFS_SH}; \
	   for i in ${MAN8}; do \
		echo "Installing $$i" ; \
		../auxman $$i $$MAN8/$$i.z ; \
	   done ; \
	   for i in ${SMAIL_LINKS} ; do \
		echo "Linking smail.8 with $$i" ; \
		../linkman $$MAN8/smail.8.z $$MAN8/$$i ; \
	   done

uninstallman:
	@. ./${DEFS_SH} ; \
	   for i in ${MAN8Z} ${SMAIL_LINKS} ; do \
		../rmvman $$MAN8/$$i ; \
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
	rm -f ${MAN8}
	rm -f ${MAN8Z}
	rm -f ${MAN8PS}

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

