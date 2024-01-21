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

include ../../../Makefile.config

CFLAGS= $(COPT)

all:	flog

flog: flog.o
	$(CC) $(LDFL) -o $@ flog.o $(AUXLIB)

clean:
	rm -f flog *.o

install:	all
	$(CP) flog $(DEST)
