#!/bin/make -f
# @(#) Makefile,v 1.4 1992/09/06 04:40:11 tron Exp
# Makefile for the generic configuration file samples
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

ROOT=../..
CONFSRC=routers query.sh
MISCSRC=Makefile
SRC=${OSSRC} ${MISCSRC}

all install depend clean clobber:

sources: ${SRC}

${SRC}:
	${GET} $@

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

nuke:	clobber
	-${CLEAN}
	-${GET} Makefile
