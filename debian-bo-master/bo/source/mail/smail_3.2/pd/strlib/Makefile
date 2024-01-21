#! /bin/make -f
#
#ident	"@(#)smail/pd/strlib:RELEASE-3_2:Makefile,v 1.8 1996/02/26 15:38:53 woods Exp"
#
# Makefile for the public domain string library for smail

SHELL=/bin/sh
MAKE=make
AR=ar
LINT=lint
SRC_PREFIX=

MKDEPEND=${ROOT}/conf/lib/mkdepend.sh
MKDEFS=${ROOT}/conf/lib/mkdefs.sh
CHECKDEFS=${ROOT}/conf/lib/checkdefs.sh
MKDIRS=${ROOT}/conf/lib/mkdirs.sh
INST=${ROOT}/conf/lib/inst.sh
INSTM=${ROOT}/conf/lib/instm.sh
XEXEC=${SHELL} ${ROOT}/conf/lib/xexec.sh
DEFS_SH=defs.sh
DEFS_H=defs.h
DEFS_SED=defs.sed

ROOT=../..
OBJ=_str2set.o strpbrk.o strspn.o strcspn.o bcopy.o bzero.o bcmp.o
CSRC=_str2set.c strpbrk.c strspn.c strcspn.c bcopy.c bzero.c bcmp.c
HSRC=_str2set.h strings.h
MISCSRC=Makefile README
SRC=${CSRC} ${HSRC} ${MISCSRC}

.c.o:
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS $$INCLUDES -c $*.c

all:	libstr.a

libstr.a: ${OBJ} Makefile
	${AR} cr libstr.a ${OBJ}
	@. ./${DEFS_SH}; \
	   ${XEXEC} $$RANLIB libstr.a

mkdefs ${DEFS_H} ${DEFS_SH} ${DEFS_SED}: ${ROOT}/conf/EDITME
	ROOT=${ROOT} ${SHELL} ${MKDEFS}

${ROOT}/conf/EDITME: # cannot depend on anything!
	cd ${ROOT}/conf; ${MAKE} EDITME

${OBJ}:	${DEFS_SH}

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

lint:	${CSRC}
	@. ./${DEFS_SH}; ${XEXEC} ${LINT} ${LINTFLAGS} ${CSRC}

depend local_depend: check_defs
	@. ./${DEFS_SH}; \
		${XEXEC} ${SHELL} ${MKDEPEND} $$CPPFLAGS $$INCLUDES \
		Makefile ${CSRC}
	. ./${DEFS_SH}; echo "$$DEFS_DEPEND" >> Makefile
	chmod -w Makefile

check_defs:
	SHELL=${SHELL} ROOT=${ROOT} ${SHELL} ${CHECKDEFS}

install:

clean:
	rm -f ${DEFS_SH} ${DEFS_H} ${DEFS_SED}
	rm -f .${DEFS_SH} .${DEFS_H} .${DEFS_SED} .Makefile
	rm -f a.out core ${OBJ}

clobber: clean
	rm -f libstr.a

# DO NOT REMOVE THIS LINE OR "make depend" WILL NOT WORK
