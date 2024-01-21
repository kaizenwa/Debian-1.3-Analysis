#! /bin/make -f
#
#ident	"@(#)smail/pd/getopt:RELEASE-3_2:Makefile,v 1.12 1996/03/19 14:48:46 woods Exp"
#
# Makefile for getopt

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
SMAILSRC=${ROOT}/src
UTIL_TARGETS=getopt
OBJ_TARGETS=get_opt.o
TARGETS=${UTIL_TARGETS} ${OBJ_TARGETS}
CSRC=getopt.c get_opt.c
MISCSRC=Makefile
SRC=${CSRC} ${MISCSRC}
OBJ=get_opt.o getopt.o
COMPAT_LIB_DIR=${ROOT}/compat
COMPAT_LIB=${COMPAT_LIB_DIR}/compat.a

all:	${TARGETS}

getopt: getopt.o ${COMPAT_LIB} ${DEFS_SH}
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$LDFLAGS getopt.o -o getopt \
		$$LIBS ${COMPAT_LIB}

getopt.o: getopt.c ${DEFS_SH}
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS $$INCLUDES -c getopt.c

get_opt.o: get_opt.c ${DEFS_SH}
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS $$INCLUDES -c get_opt.c

mkdefs ${DEFS_H} ${DEFS_SH} ${DEFS_SED}:
	ROOT=${ROOT} ${SHELL} ${MKDEFS}

${ROOT}/conf/EDITME: # cannot depend on anything!
	cd ${ROOT}/conf; ${MAKE} EDITME

${COMPAT_LIB}:
	cd ${COMPAT_LIB_DIR}; ${MAKE}

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

depend local_depend: ${SRC} check_defs
	@. ./${DEFS_SH}; \
		${XEXEC} ${SHELL} ${MKDEPEND} $$CPPFLAGS $$INCLUDES \
		Makefile ${CSRC}
	. ./${DEFS_SH}; echo "$$DEFS_DEPEND" >> Makefile
	chmod -w Makefile

check_defs:
	SHELL=${SHELL} ROOT=${ROOT} ${SHELL} ${CHECKDEFS}

install: ${UTIL_TARGETS} ${DEFS_SH}
	@. ./${DEFS_SH}; \
	   case "$$DONT_INSTALL" in \
	   ?*)	echo Testing ... install ignored; exit 0;; \
	   esac; \
	   ${XEXEC} ${SHELL} ${MKDIRS} -m 0755 $$UTIL_BIN_DIR; \
	   s_flag=; \
	   case "$$STRIP" in \
	   ?*)	s_flag=-s;;\
	   esac; \
	   ${XEXEC} ${SHELL} ${INSTM} $$s_flag -m 0555 $$UTIL_BIN_DIR \
		${UTIL_TARGETS}

clean:
	rm -f ${DEFS_SH} ${DEFS_H} ${DEFS_SED}
	rm -f .${DEFS_SH} .${DEFS_H} .${DEFS_SED} .Makefile
	rm -f ${OBJ} core a.out

clobber: clean
	rm -f ${TARGETS}

# DO NOT REMOVE THIS LINE, OR "make depend" WILL NOT WORK
