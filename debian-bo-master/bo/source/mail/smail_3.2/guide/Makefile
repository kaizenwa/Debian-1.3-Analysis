#! /bin/make -f
#
#ident	"@(#)smail/guide:RELEASE-3_2:Makefile,v 1.7 1996/02/26 15:35:51 woods Exp"
#
# Makefile for the smail guide directories
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

ROOT=../..
SUB_DIRS=admin
MISCSRC=Makefile
SRC=${MISCSRC}

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

depend mkdefs install installman clean clobber:
	@for i in ${SUB_DIRS}; do \
		echo "Making $@ under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ $@); \
	 done
