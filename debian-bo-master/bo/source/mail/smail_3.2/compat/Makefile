#! /bin/make -f
#
#ident	"@(#)smail/compat:RELEASE-3_2:Makefile,v 1.10 1996/02/26 15:32:40 woods Exp"
#
# Makefile for the smail compat library.
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
XEXEC=${SHELL} ${ROOT}/conf/lib/xexec.sh
DEFS_SH=defs.sh
DEFS_H=defs.h
DEFS_SED=defs.sed

ROOT=..
MISCSRC=Makefile
DUMMY_OBJ=dummy.o
DUMMY_CSRC=dummy.c
SRC=${MISCSRC} ${DUMMY_CSRC}
SYS5_STRLIB=_str2set.o strpbrk.o strspn.o strcspn.o
STRLIB_DIR=${ROOT}/pd/strlib
GETOPT=get_opt.o
GETOPT_DIR=${ROOT}/pd/getopt
BSTRING=bcopy.o bzero.o bcmp.o
BSTRING_DIR=${ROOT}/pd/strlib

.c.o:
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS $$INCLUDES -c $*.c

all:	compat.a

compat.a: ${DEFS_SH} ${DUMMY_OBJ}
	@. ./${DEFS_SH}; \
	   case "$$HAVE_SYS5_STRLIB" in \
	   '')	echo "cd ${STRLIB_DIR}; ${MAKE} ${SYS5_STRLIB}"; \
		(cd ${STRLIB_DIR}; ${MAKE} ${SYS5_STRLIB}); \
		for i in ${SYS5_STRLIB}; do \
			${XEXEC} ${AR} rc compat.a ${STRLIB_DIR}/$$i; \
		done;; \
	   esac; \
	   case "$$HAVE_GETOPT" in \
	   '')	echo "cd ${GETOPT_DIR}; ${MAKE} ${GETOPT}"; \
		(cd ${GETOPT_DIR}; ${MAKE} ${GETOPT}); \
		for i in ${GETOPT}; do \
			${XEXEC} ${AR} rc compat.a ${GETOPT_DIR}/$$i; \
		done;; \
	   esac; \
	   case "$$HAVE_BSTRING" in \
	   '')	echo "cd ${BSTRING_DIR}; ${MAKE} ${BSTRING}"; \
		(cd ${BSTRING_DIR}; ${MAKE} ${BSTRING}); \
		for i in ${BSTRING}; do \
			${XEXEC} ${AR} rc compat.a ${BSTRING_DIR}/$$i; \
		done;; \
	   esac; \
	   ${XEXEC} ${AR} rc compat.a ${DUMMY_OBJ}; \
	   ${XEXEC} $$RANLIB compat.a

mkdefs ${DEFS_SH} ${DEFS_H} ${DEFS_SED}: ${ROOT}/conf/EDITME
	ROOT=${ROOT} ${SHELL} ${MKDEFS}

${ROOT}/conf/EDITME: # cannot depend on anything!
	cd ${ROOT}/conf; ${MAKE} EDITME

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

depend local_depend: check_defs
	${SHELL} ${MKDEPEND} Makefile
	@. ./${DEFS_SH}; \
	   echo "echo \"defs.sh: \$${ROOT}/conf/os/$$OS_TYPE\" >> Makefile"; \
	   echo "defs.sh: \$${ROOT}/conf/os/$$OS_TYPE" >> Makefile; \
	chmod -w Makefile

check_defs:
	SHELL=${SHELL} ROOT=${ROOT} ${SHELL} ${CHECKDEFS}

install:

clean:
	rm -f ${DEFS_SH} ${DEFS_H} ${DEFS_SED}
	rm -f .${DEFS_SH} .${DEFS_H} .${DEFS_SED} .Makefile
	rm -f ${DUMMY_OBJ} ${SYS5_STRLIB} ${GETOPT} ${BSTRING}
	rm -f a.out core
	rm -f compat.a

clobber: clean

# DO NOT REMOVE THIS LINE, OR "make depend" WILL NOT WORK
