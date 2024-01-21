#! /bin/make -f
#
#ident	"@(#)smail/conf:RELEASE-3_2:Makefile,v 1.9 1996/02/27 20:08:25 woods Exp"
#
# Makefile for the conf directory in the smail source tree
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

ROOT=..
SUB_DIRS=arch driver os lib
DIST_SRC=EDITME-dist EDITME-always EDITME-most EDITME-sometimes EDITME-spinne \
	EDITME-weirdo
MISCSRC=Makefile
SRC=${MISCSRC} ${DIST_SRC}

all:
	@for i in ${SUB_DIRS}; do \
		echo "Build default targets under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/); \
	 done

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done
	@for i in ${SUB_DIRS}; do \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ $@); \
	 done

depend clobber clean:
	@for i in ${SUB_DIRS}; do \
		echo "Making $@ under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ $@); \
	 done

# WARNING:  don't depend on EDITME-dist, else local changes may be clobbered
# be paranoid and do not even attempt to make it if it exists....
#
EDITME:
	if [ ! -f EDITME ] ; then		\
		cat EDITME-dist > EDITME ; 	\
	fi

mkdefs install:
