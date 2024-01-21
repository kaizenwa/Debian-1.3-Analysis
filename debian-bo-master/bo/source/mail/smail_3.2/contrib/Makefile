#! /bin/make -f
#
#ident	"@(#)smail/contrib:RELEASE-3_2:Makefile,v 1.3 1996/02/26 15:33:50 woods Exp"
#
# Makefile for the contrib directory in the smail source tree
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
SUB_DIRS=akcs execm micnet nukeq smaillog striphdrs
CONTRIBSRC=
MISCSRC=Makefile README
SRC=${MISCSRC} ${CONTRIBSRC}

all:
	@for i in ${SUB_DIRS}; do \
		echo "Build targets under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/); \
	done

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done
	@for i in ${SUB_DIRS}; do \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ names); \
	done

depend:
	@for i in ${SUB_DIRS}; do \
		echo "Make dependencies under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ depend); \
	done

mkdefs install:

clean:
	rm -f core a.out
	@for i in ${SUB_DIRS}; do \
		echo "Clean under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ clean); \
	done

clobber:
	rm -f core a.out
	@for i in ${SUB_DIRS}; do \
		echo "Clobber under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ clobber); \
	done
