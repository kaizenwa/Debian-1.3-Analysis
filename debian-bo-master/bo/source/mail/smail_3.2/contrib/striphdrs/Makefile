#! /bin/make -f
#
#ident	"@(#)smail/contrib/striphdrs:RELEASE-3_2:Makefile,v 1.2 1996/02/26 15:35:31 woods Exp"
#
# Makefile for the smail contrib/striphdrs directory
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
STRIPHDRSSRC=striphdrs.c
MISCSRC=Makefile README
SRC=${STRIPHDRSSRC} ${MISCSRC}

all install depend clean clobber:

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done
