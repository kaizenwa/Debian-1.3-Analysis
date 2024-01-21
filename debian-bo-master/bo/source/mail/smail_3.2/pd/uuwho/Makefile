#! /bin/make -f
#
#ident	"@(#)smail/pd/uuwho:RELEASE-3_2:Makefile,v 1.13 1996/02/26 15:39:04 woods Exp"
#
# Makefile for uuwho

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
INCLUDES=$$DBM_INCLUDES -I${ROOT}/util $$INCLUDES
SMAILSRC=${ROOT}/src
TARGETS=uuwho
CSRC=uuwho.c
MISCSRC=Makefile
SRC=${CSRC} ${MISCSRC}
OBJ=uuwho.o
COMPAT_LIB_DIR=${ROOT}/compat
COMPAT_LIB=${COMPAT_LIB_DIR}/compat.a

all:	${TARGETS}

uuwho: uuwho.o ${DEFS_SH} ${COMPAT_LIB}
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS $$LDFLAGS uuwho.o -o uuwho \
		$$LIBS $$DBM_LIB ${COMPAT_LIB}

uuwho.o: uuwho.c ${DEFS_SH}
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS ${INCLUDES} -c uuwho.c

${COMPAT_LIB}:
	cd ${COMPAT_LIB_DIR}; ${MAKE}

mkdefs ${DEFS_H} ${DEFS_SH} ${DEFS_SED}: ${ROOT}/conf/EDITME
	ROOT=${ROOT} ${SHELL} ${MKDEFS}

${ROOT}/conf/EDITME: # cannot depend on anything!
	cd ${ROOT}/conf; ${MAKE} EDITME

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

depend local_depend: ${SRC} check_defs
	. ./${DEFS_SH}; ${SHELL} ${MKDEPEND} $$CPPFLAGS ${INCLUDES} \
		Makefile ${CSRC}
	. ./${DEFS_SH}; echo "$$DEFS_DEPEND" >> Makefile
	chmod -w Makefile

check_defs:
	SHELL=${SHELL} ROOT=${ROOT} ${SHELL} ${CHECKDEFS}

install: ${TARGETS} ${DEFS_SH}
	@. ./${DEFS_SH}; \
	   case "$$DONT_INSTALL" in \
	   ?*)	echo Testing ... install ignored; exit 0;;\
	   esac; \
	   ${XEXEC} ${SHELL} ${MKDIRS} -m 0755 $$SMAIL_BIN_DIR $$LIB_DIR; \
	   s_flag=; \
	   case "$$STRIP" in \
	   ?*)	s_flag=-s;;\
	   esac; \
	   ${XEXEC} ${SHELL} ${INSTM} $$s_flag -m 0555 $$SMAIL_BIN_DIR \
		${TARGETS}

clean:
	rm -f ${DEFS_SH} ${DEFS_H} ${DEFS_SED}
	rm -f .${DEFS_SH} .${DEFS_H} .${DEFS_SED} .Makefile
	rm -f ${OBJ} core a.out

clobber: clean
	rm -f ${TARGETS}

# DO NOT REMOVE THIS LINE, OR "make depend" WILL NOT WORK
