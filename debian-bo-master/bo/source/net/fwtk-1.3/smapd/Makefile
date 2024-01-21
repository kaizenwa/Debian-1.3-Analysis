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
# RcsId: "Header: Makefile,v 1.4 94/11/01 11:57:57 mjr rel "

include	../Makefile.config

CFLAGS= -DPARSEADDR -I.. $(COPT)

all:	smapd

smapd: smapd.o ../libfwall.a
	$(CC) $(LDFL) -o $@ smapd.o ../libfwall.a $(AUXLIB)

clean:
	rm -f smapd *.o

install:	all
	if [ -f $(DEST)/smapd ]; then \
		mv $(DEST)/smapd $(DEST)/smapd.old; \
	fi
	$(CP) smapd $(DEST)
	chmod 755 $(DEST)/smapd
