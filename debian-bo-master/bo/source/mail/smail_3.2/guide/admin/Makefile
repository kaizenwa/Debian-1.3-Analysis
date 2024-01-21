#! /bin/make -f
#
#ident	"@(#)smail/guide/admin:RELEASE-3_2:Makefile,v 1.8 1996/02/26 15:36:05 woods Exp"
#
# Makefile for the smail administration guide
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
PARTS=intro install config cookbook utilities
MISC=Makefile README
SRC=${PARTS} ${MISC}
NROFF=nroff
TROFF=ptroff

all:

nroff:	${PARTS}
	@${NROFF} -ms ${PARTS}

troff:	${PARTS}
	${TROFF} -ms ${PARTS}

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

install depend clean clobber:
