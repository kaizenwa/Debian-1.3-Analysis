#
#  Copyright (c) 1993, Trusted Information Systems, Incorporated
#  All rights reserved.
#

#
#	Author: Marcus J. Ranum, Trusted Information Systems, Inc.
#
# RcsId: "Header: Makefile,v 1.1 93/10/20 11:17:18 mjr rel "

include ../../../Makefile.config

# if your system doesn't have setenv() use these
SETENV=setenv.o  getenv.o


CFLAGS= -I../../.. $(COPT)

FWLIB=		../../../libfwall.a
AUTHLIB=	../../../libauth.a

all:	login-ts

login-ts:	login-ts.o $(SETENV) $(FWLIB) $(AUTHLIB)
	$(CC) $(LDFL) -o $@ login-ts.o $(SETENV) $(AUTHLIB) $(FWLIB) $(AUXLIB)
	chmod 755 $@

clean:
	rm -f login-ts *.o


install:	all
	$(CP) login-ts $(DEST)
	chmod 755 $(DEST)/login-ts
