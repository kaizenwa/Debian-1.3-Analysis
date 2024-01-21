#! /bin/make -f
#
#ident	"@(#)smail/man/man1:RELEASE-3_2:Makefile,v 1.9 1996/02/26 15:36:40 woods Exp"
#
# Makefile for the smail section 1 man pages
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
PATHTO_LINKS=uupath.1
MAN1=pathto.1 uuwho.1 ${PATHTO_LINKS}
MISC=Makefile
SRC=${MAN_SRC} ${MISC}
SED=sed
.SUFFIXES: .an .1

.an.1:
	${SED} -f ${DEFS_SED} $*.an > $*.1

all:	${MAN1}

${PATHTO_LINKS}:
	echo '.\"' > $@
	@. ./${DEFS_SH}; \
	   echo echo "\".so $$MAN1/pathto.$$MAN1_EXT\" >> $@"; \
	   echo ".so $$MAN1/pathto.$$MAN1_EXT" >> $@

${MAN1}: ${DEFS_SED}

mkdefs ${DEFS_SH} ${DEFS_H} ${DEFS_SED}: ${ROOT}/conf/EDITME
	ROOT=${ROOT} ${SHELL} ${MKDEFS}

${ROOT}/conf/EDITME: # cannot depend on anything!
	cd ${ROOT}/conf; ${MAKE} EDITME

${VERSION_SH} ${VERSION_H} ${VERSION_SED}:
	ROOT=${ROOT} ${SHELL} ${MKVERSION}

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

install:

installman: all ${DEFS_SH}
	@. ./${DEFS_SH}; \
	   ${XEXEC} ${SHELL} ${MKDIRS} -m 0755 $$MAN1; \
	   case "$$MAN1_EXT" in \
	   1)	${XEXEC} ${SHELL} ${INSTM} -r -m 0444 $$MAN1 ${MAN1};; \
	   *)	for i in ${MAN1}; do \
			bn=`basename $$i .1`; \
			${XEXEC} ${SHELL} ${INST} -r -m 0444 $$i \
					 $$MAN1/$$bn.$$MAN1_EXT; \
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
	rm -f ${MAN1}

# DO NOT REMOVE THIS LINE, OR "make depend" WILL NOT WORK
