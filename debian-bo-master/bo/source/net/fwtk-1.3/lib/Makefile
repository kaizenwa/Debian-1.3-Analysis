#
#  Copyright (c) 1993, Trusted Information Systems, Incorporated
#  All rights reserved.
# 
#  Redistribution and use are governed by the terms detailed in the
#  license document ("LICENSE") included with the toolkit.
#

#
#	Author: Marcus J. Ranum, Trusted Information Systems, Inc.
#
# RcsId: "Header: Makefile,v 1.3 94/11/01 11:55:14 mjr rel "

include	../Makefile.config

CFLAGS= -I.. $(COPT)
LIB=	../libfwall.a

# source for UDP based syslog(3)
SYSLOG=	syslog.o

OBJ=	config.o daemon.o pname.o nama.o mapu.o mapg.o conn.o hnam.o \
	mktemp.o rand.o enargv.o alldi.o lock.o urg.o $(SYSLOG)


all:	$(LIB)
$(LIB): $(OBJ)
	ar rcv $@ $(OBJ)
	$(RANLIB) $@

clean:
	rm -f $(LIB) *.o

install:	all
