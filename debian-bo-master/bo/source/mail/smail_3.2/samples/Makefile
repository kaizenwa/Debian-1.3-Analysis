#!/bin/make -f
# @(#) Makefile,v 1.4 1992/09/06 04:40:07 tron Exp
# Makefile for the samples directory in the smail source tree
#
#    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992  Ronald S. Karr
# 
# See the file COPYING, distributed with smail, for restriction
# and warranty information.

SHELL=/bin/sh
MAKE=make
GET=sccs get
AR=ar
LINT=lint
CC=cc
CLEAN=sccs clean
MK=${MAKE} -${MAKEFLAGS} SHELL=${SHELL}
SRC_PREFIX=

ROOT=..
SUB_DIRS=generic queryprog
MISCSRC=Makefile
SRC=${MISCSRC}

all:
	@for i in ${SUB_DIRS}; do \
		echo "Build targets under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MK} SRC_PREFIX=${SRC_PREFIX}$$i/); \
	 done	# $(MAKE)

sources: ${SRC}
	@for i in ${SUB_DIRS}; do \
		echo "Get sources under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MK} SRC_PREFIX=${SRC_PREFIX}$$i/ sources); \
	 done	# $(MAKE)

${SRC}:
	${GET} $@

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done
	@for i in ${SUB_DIRS}; do \
		(cd $$i; ${MK} SRC_PREFIX=${SRC_PREFIX}$$i/ names); \
	 done	# $(MAKE)

depend:
	@for i in ${SUB_DIRS}; do \
		echo "Make dependencies under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MK} SRC_PREFIX=${SRC_PREFIX}$$i/ clean); \
	 done	# $(MAKE)

install:

clean:
	rm -f core a.out
	@for i in ${SUB_DIRS}; do \
		echo "Clean under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MK} SRC_PREFIX=${SRC_PREFIX}$$i/ clean); \
	 done	# $(MAKE)

clobber:
	rm -f core a.out
	@for i in ${SUB_DIRS}; do \
		echo "Clobber under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MK} SRC_PREFIX=${SRC_PREFIX}$$i/ clobber); \
	 done	# $(MAKE)

nuke:
	rm -f core a.out
	-${CLEAN}
	-${GET} Makefile
	@for i in ${SUB_DIRS}; do \
		echo "Nuke under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MK} SRC_PREFIX=${SRC_PREFIX}$$i/ nuke); \
	 done	# $(MAKE)
