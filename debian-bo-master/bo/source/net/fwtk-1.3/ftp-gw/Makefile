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
# RcsId: "Header: Makefile,v 1.3 94/11/01 11:54:57 mjr rel "

include	../Makefile.config

#CFLAGS= -I.. $(COPT) -g -DBINDDEBUG -DBINDDEBUGPORT=6565
CFLAGS= -I.. $(COPT)

all:	ftp-gw

ftp-gw: ftp-gw.o ../libauth.a ../libfwall.a
	$(CC) $(LDFL) -o $@ ftp-gw.o ../libauth.a ../libfwall.a $(AUXLIB)

clean:
	rm -f ftp-gw *.o

install:	all
	if [ -f $(DEST)/ftp-gw ]; then \
		mv $(DEST)/ftp-gw $(DEST)/ftp-gw.old; \
	fi
	$(CP) ftp-gw $(DEST)
	chmod 755 $(DEST)/ftp-gw
