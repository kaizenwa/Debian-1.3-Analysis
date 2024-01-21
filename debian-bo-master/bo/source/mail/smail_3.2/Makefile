#! /bin/make -f
#
#ident	"@(#)smail:RELEASE-3_2:Makefile,v 1.14 1996/02/28 06:57:01 woods Exp"
#
# Top-level makefile for the smail source tree
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
MKDIRS=${ROOT}/conf/lib/mkdirs.sh
MKDEFS=${ROOT}/conf/lib/mkdefs.sh
CHECKDEFS=${ROOT}/conf/lib/checkdefs.sh
INST=${ROOT}/conf/lib/inst.sh
INSTM=${ROOT}/conf/lib/instm.sh
XEXEC=${SHELL} ${ROOT}/conf/lib/xexec.sh

DEFS_SH=defs.sh
DEFS_H=defs.h
DEFS_SED=defs.sed

ROOT=.
SUB_DIRS=conf compat pd util src man
MORE_SUB_DIRS=contrib guide NOTES

CSRC=
HSRC=
SHSRC=
CONTRIBSRC=
MISCSRC=Makefile level patchnum README CHANGES COPYING
SRC=${CSRC} ${HSRC} ${SHSRC} ${CONTRIBSRC} ${MISCSRC}

CODE_SUB_DIRS=pd util src contrib

all:	build_smail

build_smail: ${DEFS_H} ${DEFS_SH} ${DEFS_SED}
	@for i in ${SUB_DIRS}; do \
		echo "Build default targets under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/); \
	 done

everything: build_smail
	@for i in ${MORE_SUB_DIRS}; do \
		echo "Build default targets under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/); \
	 done

${DEFS_H} ${DEFS_SH} ${DEFS_SED}: ${ROOT}/conf/EDITME
	ROOT=${ROOT} ${SHELL} ${MKDEFS}

${ROOT}/conf/EDITME: # cannot depend on anything!
	cd ${ROOT}/conf; ${MAKE} EDITME

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done
	@for i in ${SUB_DIRS} ${MORE_SUB_DIRS}; do \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ $@); \
	 done

depend:	local_depend subdir_depend

local_depend: check_defs
	${SHELL} ${MKDEPEND} Makefile
	. ./${DEFS_SH}; echo "$$DEFS_DEPEND" >> Makefile
	chmod -w Makefile

all_subdir_depend: subdir_depend more_subdir_depend

subdir_depend:
	@for i in ${SUB_DIRS}; do \
		echo "Make dependencies under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ depend); \
	 done

more_subdir_depend:
	@for i in ${MORE_SUB_DIRS}; do \
		echo "Make dependencies under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ depend); \
	 done

check_defs:
	SHELL=${SHELL} ROOT=${ROOT} ${SHELL} ${CHECKDEFS}

mkdefs: local_mkdefs subdir_mkdefs

local_mkdefs: ${DEFS_H} ${DEFS_SH} ${DEFS_SED}

subdir_mkdefs:
	@for i in ${SUB_DIRS} ${MORE_SUB_DIRS}; do \
		echo "Running mkdefs under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ mkdefs); \
	 done

install: ${DEFS_SH}
	@. ./${DEFS_SH}; \
	   case "$$DONT_INSTALL" in \
	   ?*)	echo Testing ... $@ ignored; exit 0;; \
	   esac
	   for i in ${SUB_DIRS}; do \
		echo "Install targets under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ $@); \
	   done

installman:
	@echo "Install man pages under ${SRC_PREFIX}man ..."
	cd man; ${MAKE} SRC_PREFIX=${SRC_PREFIX}man/ $@

tags TAGS lint:
	@for i in ${CODE_SUB_DIRS}; do \
		echo "Running $@ under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ $@); \
	 done

clean:
	rm -f ${DEFS_SH} ${DEFS_H} ${DEFS_SED}
	@for i in ${SUB_DIRS} ${MORE_SUB_DIRS}; do \
		echo "Making $@ under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ $@); \
	 done

clobber:
	rm -f ${DEFS_SH} ${DEFS_H} ${DEFS_SED}
	rm -f .${DEFS_SH} .${DEFS_H} .${DEFS_SED} .Makefile
	@for i in ${SUB_DIRS} ${MORE_SUB_DIRS}; do \
		echo "Making $@ under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ $@); \
	 done

# DO NOT REMOVE THIS LINE, OR "make depend" WILL NOT WORK
