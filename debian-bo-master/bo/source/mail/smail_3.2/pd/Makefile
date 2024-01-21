#! /bin/make -f
#
#ident	"@(#)smail/pd:RELEASE-3_2:Makefile,v 1.11 1996/02/26 15:37:25 woods Exp"
#
# Makefile for items that are in the public domain
#
# This file is in the public domain

SHELL=/bin/sh
MAKE=make
AR=ar
LINT=lint
SRC_PREFIX=

MKDEPEND=${ROOT}/conf/lib/mkdepend.sh
MKDEFS=${ROOT}/conf/lib/mkdefs.sh
CHECKDEFS=${ROOT}/conf/lib/checkdefs.sh
DEFS_SH=defs.sh
DEFS_H=defs.h
DEFS_SED=defs.sed

MAKE_SUB_DIRS=getopt pathalias uuwho binmail
MISC_SUB_DIRS=sdbm strlib
SUB_DIRS=${MAKE_SUB_DIRS} ${MISC_SUB_DIRS}
ROOT=..
MISCSRC=Makefile README
SRC=${MISCSRC}

all:	build_util

build_util: ${DEFS_SH}
	@if [ -f ${DEFS_SH} ]; then \
	   . ./${DEFS_SH}; \
	   case "$$DBM_LIB" in \
	   */pd/sdbm/*) \
		echo "Build default targets under ${SRC_PREFIX}sdbm ..."; \
		(cd sdbm; ${MAKE} SRC_PREFIX=${SRC_PREFIX}sdbm/); \
	   esac; fi
	@for i in ${MAKE_SUB_DIRS}; do \
		echo "Build default targets under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/); \
	 done

${DEFS_SH} ${DEFS_H} ${DEFS_SED}: ${ROOT}/conf/EDITME
	ROOT=${ROOT} ${SHELL} ${MKDEFS}

${ROOT}/conf/EDITME: # cannot depend on anything!
	cd ${ROOT}/conf; ${MAKE} EDITME

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done
	@for i in ${SUB_DIRS}; do \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ names); \
	 done

install:
	@for i in ${MAKE_SUB_DIRS}; do \
		echo "Install targets under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ install); \
	 done

depend: local_depend subdir_depend

local_depend: check_defs
	${SHELL} ${MKDEPEND} Makefile
	. ./${DEFS_SH}; echo "$$DEFS_DEPEND" >> Makefile
	chmod -w Makefile

check_defs:
	SHELL=${SHELL} ROOT=${ROOT} ${SHELL} ${CHECKDEFS}

mkdefs: local_mkdefs subdir_mkdefs

local_mkdefs: ${DEFS_SH} ${DEFS_H} ${DEFS_SED}

subdir_mkdefs:
	@for i in ${SUB_DIRS} ${MORE_SUB_DIRS}; do \
		echo "Checking defs files under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ mkdefs); \
	 done

subdir_depend:
	@for i in ${MAKE_SUB_DIRS} strlib; do \
		echo "Make dependencies under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ depend); \
	 done
	@if [ -f ${DEFS_SH} ]; then \
	   . ./${DEFS_SH}; \
	   case "$$DBM_LIB" in \
	   */pd/sdbm/*) \
		echo "Make dependencies under ${SRC_PREFIX}sdbm ..."; \
		(cd sdbm; ${MAKE} SRC_PREFIX=${SRC_PREFIX}sdbm/ depend); \
	   esac; fi

clean: local_clean
	@for i in ${SUB_DIRS}; do \
		echo "Clean under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ clean); \
	 done

clobber: local_clean
	@for i in ${SUB_DIRS}; do \
		echo "Clobber under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ clobber); \
	 done

local_clean:
	rm -f ${DEFS_SH} ${DEFS_H} ${DEFS_SED}
	rm -f .${DEFS_SH} .${DEFS_H} .${DEFS_SED} .Makefile

nuke:
	-${CLEAN}
	-${GET} Makefile
	@for i in ${SUB_DIRS}; do \
		echo "Nuke under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ nuke); \
	 done

# DO NOT REMOVE THIS LINE, OR "make depend" WILL NOT WORK
