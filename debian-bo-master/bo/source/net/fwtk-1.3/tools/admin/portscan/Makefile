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

all:	portscan

portscan: portscan.o
	$(CC) $(LDFL) -o $@ portscan.o $(AUXLIB)

clean:
	rm -f portscan *.o

install:	all
