#
# Makefile  --	xringd
#
# Copyright (c) 1995-1996 Angelo Haritsis. All rights reserved.
#
# $Id: Makefile,v 1.5 1996/02/17 15:50:07 ah Exp ah $
#

VERSION=1.10

# where we install
BINDIR=/usr/local/sbin
MANDIR=/usr/local/man

# Number of seconds (compiled-in) to wait after resetting the state machine
# before accepting any new rings; can also be set on cmd line (-t)
INITWAIT=15

# compiled-in configuration file; can stil be changed
# from the command line
CONF=/etc/xringd.conf

# compiled-in modem device; can be specified on command line (-c)
MODEM=/dev/modem

#DBG=-DRINGSM_DUMP -DDEBUG	# include ringsm debug code

DEFS=-DVERSION=\"$(VERSION)\" -DMODEM_FILE=\"$(MODEM)\" -DCONF_FILE=\"$(CONF)\" -DINIT_TIMEOUT=$(INITWAIT) $(DBG)

CC=gcc
CFLAGS =-Wall -O2 -m486 $(DEFS) -s
#CFLAGS = -g $(DEFS)
LDFLAGS=-s
#LDFLAGS=-g


OBJ = ringsm.o xringd.o

xringd: $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) -o $@

install: xringd
	-install -o root -g bin -m 755 xringd $(BINDIR)
	-install -o root -g root -m 755 xringd.8 $(MANDIR)/man8
	@echo "!!! You also need a config file, eg. /etc/xringd.conf"
dist:
	-mv -f xringd*.lsm xringd-$(VERSION).lsm > /dev/null 2>&1
	-perl -pi -e "s/Version:\t[0-9.]+/Version:\t$(VERSION)/;s/xringd-[0-9]+\.[0-9]+/xringd-$(VERSION)/g;" xringd-$(VERSION).lsm
	-perl -pi -e "s/xringd [0-9.]+/xringd $(VERSION)/;" README 
	cp xringd*.lsm $(HOME)/
	( cd .. ; tar --exclude \*.o --exclude RCS --exclude test \
		--exclude .gdb_history \
		-zcvSf $(HOME)/xringd-$(VERSION).tgz xringd \
	)

# util to show the internal kernel mdoem input counters
uartcount: uartcount.c
	$(CC) $(CFLAGS) $? -o $@

clean:
	rm -f *.o xringd errors tags
