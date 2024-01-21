#
#  Copyright (c) 1993, Trusted Information Systems, Incorporated
#  All rights reserved.
# 
#  Redistribution and use are governed by the terms detailed in the
#  license document ("LICENSE") included with the toolkit.
#

#
#	Author: Marcus J. Ranum, Trusted Information Systems, Inc.
# RcsId: "Header: Makefile,v 1.3 94/11/01 11:56:55 mjr rel "

include	../Makefile.config

CFLAGS= -I.. $(COPT)

all: rlogin-gw

rlogin-gw: rlogin-gw.o ../libauth.a ../libfwall.a
	$(CC) $(LDFL) -o $@ rlogin-gw.o ../libauth.a ../libfwall.a $(AUXLIB)

clean:
	rm -f rlogin-gw rlogin-gw.o core

install:	all
	if [ -f $(DEST)/rlogin-gw ]; then \
		mv $(DEST)/rlogin-gw $(DEST)/rlogin-gw.old; \
	fi
	$(CP) rlogin-gw $(DEST)
	chmod 755 $(DEST)/rlogin-gw
