#
#  Copyright (c) 1993, Trusted Information Systems, Incorporated
#  All rights reserved.
# 
#  Redistribution and use are governed by the terms detailed in the
#  license document ("LICENSE") included with the toolkit.
#

#
#	Author: Marcus J. Ranum, Trusted Information Systems, Inc.
# RcsId: "Header: Makefile,v 1.3 94/11/01 11:55:45 mjr rel "

include	../Makefile.config

CFLAGS= -I.. $(COPT)

all: netacl

netacl: netacl.o ../libfwall.a
	$(CC) $(LDFL) -o $@ netacl.o ../libfwall.a $(AUXLIB)

clean:
	rm -f netacl netacl.o core

install:	all
	if [ -f $(DEST)/netacl ]; then \
		mv $(DEST)/netacl $(DEST)/netacl.old; \
	fi
	$(CP) netacl $(DEST)
	chmod 755 $(DEST)/netacl
