#! /bin/make -f
#
#ident	"@(#)smail/contrib/akcs:RELEASE-3_2:Makefile,v 1.2 1996/02/26 15:34:06 woods Exp"
#
# Makefile for the smail contrib/akcs directory
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
AKCSSRC=akcs.c akcs.h directors transports
MISCSRC=Makefile README README_SHAR
SRC=${AKCSSRC} ${MISCSRC}

all install depend clean clobber:

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done
