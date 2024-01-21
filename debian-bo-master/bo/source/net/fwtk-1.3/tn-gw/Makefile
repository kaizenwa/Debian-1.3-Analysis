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
# RcsId: "Header: Makefile,v 1.3 94/11/01 11:58:11 mjr rel "

include	../Makefile.config

CFLAGS= -I.. $(COPT)

all:	tn-gw

tn-gw:	tn-gw.o ../libauth.a ../libfwall.a
	$(CC) $(LDFL) -o $@ tn-gw.o ../libfwall.a ../libauth.a $(AUXLIB)

clean:
	rm -f tn-gw *.o

install:	all
	if [ -f $(DEST)/tn-gw ]; then \
		mv $(DEST)/tn-gw $(DEST)/tn-gw.old; \
	fi
	$(CP) tn-gw $(DEST)
	chmod 755 $(DEST)/tn-gw
