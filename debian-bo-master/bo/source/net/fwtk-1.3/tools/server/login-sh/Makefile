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
FWLIB=	../../../libfwall.a
AUTHLIB=	../../../libauth.a

all:	login-sh

login-sh:	login-sh.o $(SETENV) $(FWLIB) $(AUTHLIB)
	$(CC) $(LDFL) -o $@ login-sh.o $(SETENV) $(AUTHLIB) $(FWLIB)
	chmod 755 $@

clean:
	rm -f login-sh *.o

install:	all
	$(CP) login-sh $(DEST)
	chmod 755 $(DEST)/login-sh
