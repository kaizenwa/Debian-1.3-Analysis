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
# RcsId: "Header: Makefile,v 1.3 94/11/01 11:57:49 mjr rel "

include	../Makefile.config

CFLAGS= -I.. $(COPT)

all: smap

smap: smap.o ../libfwall.a
	$(CC) $(LDFL) -o $@ smap.o ../libfwall.a $(AUXLIB)

clean:
	rm -f smap smap.o core

install:	all
	if [ -f $(DEST)/smap ]; then \
		mv $(DEST)/smap $(DEST)/smap.old; \
	fi
	$(CP) smap $(DEST)
	chmod 755 $(DEST)/smap
