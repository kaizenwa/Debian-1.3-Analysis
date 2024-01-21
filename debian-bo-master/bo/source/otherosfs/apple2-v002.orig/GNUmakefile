# Generic -*-Makefile-*- (assumes GnuMake)

SHELL=/bin/sh

# For Linux
GLIBC_HOME:=/usr

CC= gcc
CFLAGS+= -pipe			#use pipes between stages
#CFLAGS+= -ggdb 		#debugging
#CFLAGS+= -pg			#profiling
CFLAGS+= -O2 -fomit-frame-pointer #optimizing
#CFLAGS+= -Wall -Werror		#warnings
CFLAGS:=$(strip $(CFLAGS))

#LDFLAGS+=  -ggdb
#LDFLAGS+= $(GLIBC_HOME)/lib/libc.a#GNU library
LDFLAGS:=$(strip $(LDFLAGS))

YACC=bison
YFLAGS= --verbose
LEX=flex
LFLAGS= -8

#LOADLIBES+= $(GLIBC_HOME)/lib/libc.a#GNU library
#LOADLIBES+= -lfl		#flex lib
#LOADLIBES+= -lm		#math lib
LOADLIBES+= -lvga		#SVGA lib
#LOADLIBES+= $(GLIBC_HOME)/lib/libmcheck.a#malloc watcher
#LOADLIBES:=$(strip $(LOADLIBES))

MANPAGES=			# Manpages to install

BINDIR=/usr/bin			# Where to place the binary.
MANDIR=				# Where to place the man pages

HDR=				# Header files
SRC=				# Source files

OBJ_EMULATOR= 	apple2.o disk.o diskio.o keys.o misc.o interface.o

PROGS=	apple2

####################
# Choose a platform
#
#OS_TYPE= -DSUN_BSD
#OS_TYPE= -DHP_UX

####################
# Assorted defines
#
DEFINES=  $(DEFS)		#define DEFS in command line.
#DEFINES+= -DNDEBUG		#disable assert()
#DEFINES+= -DMOUSE_EMULATION	#Enable mouse emulation
#DEFINES+= -DPROTOTYPES		#use prototypes
DEFINES+= $(OS_TYPE)		#platform
DEFINES:=$(strip $(DEFINES))

####################

%.c : %.l
	$(LEX) -t $(LFLAGS) $< > $*.c

%.o : %.c
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $(DEFINES) $< -o $@

%.o : %.S
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $(DEFINES) $< -o $@

%.h %.c : %.y
	$(YACC) -d $(YFLAGS) $< -o $*.c

####################

.PHONY:	all clean realclean dep depend TAGS check remake

all: $(PROGS)

clean:
	$(RM) *.o *~ core*
	$(RM) TAGS
	if [ -d checkdir ] ; then cd checkdir; $(MAKE) clean; fi

realclean: clean
	$(RM) $(PROGS) .depend
	$(RM) -r autogen/*
	if [ -d checkdir ] ; then cd checkdir; $(MAKE) realclean; fi

install: $(PROGS)
	cp $(PROGS) $(BINDIR)

dep: depend

depend:
	$(CC) -MM *.c > .depend
	if [ -d checkdir ] ; then cd checkdir; $(MAKE) depend; fi

TAGS:
	etags -t *.[hcly]
	if [ -d checkdir ] ; then cd checkdir; $(MAKE) TAGS; fi

remake: realclean TAGS depend all

####################

apple2 : $(OBJ_EMULATOR)
	$(CC) $(LDFLAGS) $^ $(LOADLIBES) -o $@

ifeq (.depend, $(wildcard .depend))
include .depend
endif
