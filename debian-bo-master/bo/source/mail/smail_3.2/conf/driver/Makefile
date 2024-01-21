#! /bin/make -f
#
#ident	"@(#)smail/conf/driver:RELEASE-3_2:Makefile,v 1.6 1996/02/26 15:33:15 woods Exp"
#
# Makefile for the smail program and associated utilities
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
DRIVERSRC=arpa-network bsd-network unix-generic
MISCSRC=Makefile
SRC=${DRIVERSRC} ${MISCSRC}

all install depend clean clobber:

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done
