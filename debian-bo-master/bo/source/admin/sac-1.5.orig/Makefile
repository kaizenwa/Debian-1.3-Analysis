# $Copyright: $
# Copyright (c) 1995 by Steve Baker
# All Rights reserved
#
# This software is provided as is without any express or implied
# warranties, including, without limitation, the implied warranties
# of merchant-ability and fitness for a particular purpose.
#
# Thanks to Edward S. Marshall for extending the Makefile.
#

CC=gcc
CFLAGS=-Wall -O2 -fomit-frame-pointer -m486
LDFLAGS=-s

SAC_DEST=sac
RAW_DEST=rawtmp
RADIUS_DIR=/usr/adm/radacct
BINDIR=/usr/local/bin
MANS=sac.8 rawtmp.1
MANDIR=/usr/man

all:	sac rawtmp

sac:	sac.o
	$(CC) $(LDFLAGS) -o $(SAC_DEST) sac.o

sac.o:	sac.c
	$(CC) $(CFLAGS) -DRADIUS_DIR=\"${RADIUS_DIR}\" -c sac.c

rawtmp:	rawtmp.o
	$(CC) $(LDFLAGS) -o $(RAW_DEST) rawtmp.o

rawtmp.o:	rawtmp.c

clean:
	if [ -e $(SAC_DEST) ]; then rm $(SAC_DEST); fi
	if [ -e $(RAW_DEST) ]; then rm $(RAW_DEST); fi
	if [ -e sac.o -o -e raw.o ]; then rm *.o; fi

install:
	install -d $(BINDIR)
	if [ -e $(SAC_DEST) ]; then \
		install -s $(SAC_DEST) $(BINDIR)/$(SAC_DEST); \
	fi
	if [ -e $(RAW_DEST) ]; then \
		install -s $(RAW_DEST) $(BINDIR)/$(RAW_DEST); \
	fi
	for man in $(MANS); do \
		m=`expr match $$man '.*\(.\)'`; \
		install $$man $(MANDIR)/man$$m/$$man; \
	done
