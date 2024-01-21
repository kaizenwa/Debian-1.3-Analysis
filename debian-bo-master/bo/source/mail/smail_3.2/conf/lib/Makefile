#! /bin/make -f
#
#ident	"@(#)smail/conf/lib:RELEASE-3_2:Makefile,v 1.8 1996/02/26 15:33:28 woods Exp"
#
# Makefile for the smail configuration utilities directory
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
LIBSRC=arch.h checkdefs.sh inst.sh instm.sh mkdefs.sh mkdepend.sh mkdirs.sh \
       mkversion.sh sockets.h xexec.sh
MISCSRC=Makefile
SRC=${LIBSRC} ${MISCSRC}

all install depend clean clobber:

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done
