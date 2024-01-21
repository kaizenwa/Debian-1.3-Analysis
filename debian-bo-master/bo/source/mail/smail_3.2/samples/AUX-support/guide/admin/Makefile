#!/bin/make -f
# @(#) Makefile,v 1.5 1992/07/11 11:58:35 tron Exp
# Makefile for the smail administration guide
#
#    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992  Ronald S. Karr
# 
# See the file COPYING, distributed with smail, for restriction
# and warranty information.
#
# rbd	91.08.19	PostScript printing on A/UX
# rbd	92.08.17	For 3.1.26 (no changes to shell code)
#
SHELL=/bin/sh
MAKE=make
GET=sccs get
AR=ar
LINT=lint
CLEAN=sccs clean
MK=${MAKE} -${MAKEFLAGS}
SRC_PREFIX=

ROOT=../..
PARTS=intro install config cookbook utilities
MISC=Makefile
SRC=${PARTS} ${MISC}
NROFF=nroff
TROFF=troff

all:

nroff:	${PARTS}
	${NROFF} -ms ${PARTS}

troff:	${PARTS}
	${TROFF} -Tpsc -ms ${PARTS} | psdit | atprint

sources: ${SRC}

${SRC}:
	${GET} $@

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

install depend clean clobber:

nuke: clobber
	-${CLEAN}
	-${GET} Makefile
