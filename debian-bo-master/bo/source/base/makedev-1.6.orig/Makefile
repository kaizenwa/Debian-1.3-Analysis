#
# Makefile for MAKEDEV-C

# Here is stuff suitable for the standalone distribution.
# TESTING should not be set unless you're testing makedev.

TESTING=0

ifeq ($(TESTING),1)

# Settings for testing and developing makedev
CC=gcc
CFLAGS=-g -Wall -DTESTING
LDFLAGS=-qmagic
BINARY=MAKEDEV
CAN_BUILD_PARSER=1

else

# Settings for shipping makedev standalone
CC=gcc
CFLAGS=-O2 -fomit-frame-pointer
LDFLAGS=-s
BINARY=MAKEDEV

endif

# Man pages and other stuff
MAN5=devinfo.5 makedev.cfg.5
MAN8=$(BINARY).8

all: $(BINARY)

$(BINARY): parser.o devices.o
	$(CC) $(LDFLAGS) parser.o devices.o -o $@

ifeq ($(CAN_BUILD_PARSER),1)
%.c: %.syn
	ag -b $<
endif

%.o: %.c
	$(CC) $(CFLAGS) $< -c


install: all
	install -c -m 755 -s MAKEDEV /dev
	install -c -m 644 devinfo makedev.cfg /etc
	install -c -m 644 $(MAN5) /usr/man/man5
	install -c -m 644 $(MAN8) /usr/man/man8

parser.o: parser.c parser.h devices.h 
devices.o: devices.c devices.h version.h
.PHONY: all tidy distclean clean pristine

tidy: 
	-rm -f *~ core

distclean: tidy
	-rm -f *.o
ifeq ($(CAN_BUILD_PARSER),1)
	-rm -f parser.c parser.h
endif

clean: distclean
	-rm -f MAKEDEV MAKEDEV-C

