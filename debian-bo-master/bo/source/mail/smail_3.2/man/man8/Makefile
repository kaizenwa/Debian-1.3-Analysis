#! /bin/make -f
#
#ident	"@(#)smail/man/man8:RELEASE-3_2:Makefile,v 1.9 1996/02/26 15:37:12 woods Exp"
#
# Makefile for the smail section 8 man pages
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
MAN_SRC=mkdbm.an mkline.an mksort.an pathalias.an smail.an mkpath.an \
	mkaliases.an pathmerge.an checkerr.an getmap.an mkhpath.an \
	savelog.an mkuuwho.an
SMAIL_LINKS=mailq.8 sendmail.8 runq.8 rmail.8 rsmtp.8 smtpd.8
MAN8=mkdbm.8 mkline.8 mksort.8 pathalias.8 smail.8 mkpath.8 mkaliases.8 \
     pathmerge.8 checkerr.8 getmap.8 mkhpath.8 savelog.8 mkuuwho.8 \
     ${SMAIL_LINKS}
MISC=Makefile
SRC=${MAN_SRC} ${MISC}
SED=sed
.SUFFIXES: .an .8

.an.8:
	${SED} -f ${DEFS_SED} $*.an > $*.8

all:	${MAN8}

${SMAIL_LINKS}:
	echo '.\"' > $@
	@. ./${DEFS_SH}; \
	   echo echo "\".so $$MAN8/smail.$$MAN8_EXT\" >> $@"; \
	   echo ".so $$MAN8/smail.$$MAN8_EXT" >> $@

${MAN8}: ${DEFS_SED}

mkdefs ${DEFS_SH} ${DEFS_H} ${DEFS_SED}: ${ROOT}/conf/EDITME
	ROOT=${ROOT} ${SHELL} ${MKDEFS}

${ROOT}/conf/EDITME: # cannot depend on anything!
	cd ${ROOT}/conf; ${MAKE} EDITME

${VERSION_SH} ${VERSION_H} ${VERSION_SED}:
	ROOT=${ROOT} ${SHELL} ${MKVERSION}

all:

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

install:

installman: all ${DEFS_SH}
	@. ./${DEFS_SH}; \
	   ${XEXEC} ${SHELL} ${MKDIRS} -m 0755 $$MAN8; \
	   case "$$MAN8_EXT" in \
	   8)	${XEXEC} ${SHELL} ${INSTM} -r -m 0444 $$MAN8 ${MAN8};; \
	   *)	for i in ${MAN8}; do \
			bn=`basename $$i .8`; \
			${XEXEC} ${SHELL} ${INST} -r -m 0444 $$i \
					 $$MAN8/$$bn.$$MAN8_EXT; \
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
	rm -f ${MAN8}

# DO NOT REMOVE THIS LINE, OR "make depend" WILL NOT WORK
