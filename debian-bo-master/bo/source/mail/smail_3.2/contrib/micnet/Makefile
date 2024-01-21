#! /bin/make -f
#
#ident	"@(#)smail/contrib/micnet:RELEASE-3_2:Makefile,v 1.4 1996/02/26 15:34:36 woods Exp"
#
# Makefile for the smail contrib/micnet directory
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
MICNETSRC=
MISCSRC=Makefile README config.eg micname routers transports
SRC=${MICNETSRC} ${MISCSRC}

all install depend clean clobber:

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done
