# Makefile for procinfo.

CC = gcc -Wall -Wstrict-prototypes
LDLIBS = -lcurses

# Pick your favorites:

CFLAGS = -O6 -fomit-frame-pointer
LDFLAGS = -s

# CFLAGS = -g -DDEBUG #-DPROC_DIR=\"as.d/\"
# LDFLAGS = -g

# CFLAGS = -pg -O6
# LDFLAGS = -pg


### End of configurable options.

all:		procinfo

procinfo:	procinfo.o routines.o

install: procinfo procinfo.8 lsdev.pl lsdev.8
	install procinfo /usr/local/bin/procinfo
	install lsdev.pl /usr/local/bin/lsdev
	install -m 644  procinfo.8 /usr/local/man/man8/procinfo.8
	install -m 644  lsdev.8 /usr/local/man/man8/lsdev.8

clean:
	rm -f procinfo procinfo.0 *.o *~ out

procinfo.o : procinfo.c procinfo.h 
routines.o : routines.c procinfo.h 
