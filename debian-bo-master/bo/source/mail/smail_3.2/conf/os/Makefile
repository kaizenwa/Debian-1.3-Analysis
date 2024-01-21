#! /bin/make -f
#
#ident	"@(#)smail/conf/os:RELEASE-3_2:Makefile,v 1.10 1996/02/26 15:33:40 woods Exp"
#
# Makefile for the smail operating system description files
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
OSSRC=	template \
	386ix \
	3b2-s5.3+win \
	aix-ps2 \
	aix3.1 \
	aix3.2 \
	aux2.0 \
	aux3.0 \
	bsd4.2 \
	bsd4.3 \
	cpc3.0 \
	delta \
	forpro \
	hp-ux7.0 \
	hp-ux8.0 \
	irix3.2 \
	irix3.3 \
	irix4.0 \
	isc2.2.1 \
	mips-bsd4.3 \
	next2.0 \
	posix \
	sco3.2 \
	sco3.2+tcp \
	scs4.2 \
	sun_os3 \
	sun_os4 \
	sun_os4.1 \
	sys5.2 \
	sys5.3 \
	sys5.3+cmc \
	sys5.4 \
	ultrix1.2 \
	unixpc3.0 \
	unixpc3.5 \
	uts1.2 \
	uts2.0 \
	xenix5 \
	xenix5+tcp
MISCSRC=Makefile
SRC=${OSSRC} ${MISCSRC}

all install depend clean clobber:

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done
