#
#  Copyright (c) 1993, Trusted Information Systems, Incorporated
#  All rights reserved.
# 
#  Redistribution and use are governed by the terms detailed in the
#  license document ("LICENSE") included with the toolkit.
#

#
#	Author: Marcus J. Ranum, Trusted Information Systems, Inc.
# RcsId: "Header: Makefile,v 1.4 94/11/01 11:56:48 mjr rel "

include	../Makefile.config

CFLAGS= -I.. $(COPT)

all: plug-gw

plug-gw: plug-gw.o ../libfwall.a
	$(CC) $(LDFL) -o $@ plug-gw.o ../libfwall.a $(AUXLIB)
	chmod 755 $@

clean:
	rm -f plug-gw plug-gw.o core

install:	all
	if [ -f $(DEST)/plug-gw ]; then \
		mv $(DEST)/plug-gw $(DEST)/plug-gw.old; \
	fi
	$(CP) plug-gw $(DEST)
	chmod 755 $(DEST)/plug-gw
