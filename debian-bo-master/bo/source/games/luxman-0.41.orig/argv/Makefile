# Generated automatically from Makefile.in by configure.
###############################################################################
# Makefile for the Argv library.
# $Id: Makefile.all.in,v 1.15 1994/02/28 02:28:24 gray Exp $
###############################################################################

#-----------------------------------------------------
# Note: I hacked this for the lux distribution -- fpm 
#-----------------------------------------------------

# default c-compiler
CC = gcc

DEFS =
INCS = -I.
LIBS =  -L$(srcdir) -largv

LIBRARY	= libargv.a
INFOFILE = argv.info

CCFLAGS = 
LDFLAGS = 

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL) -m 644

###############################################################################
# End of system configuration section.
###############################################################################

SHELL = /bin/sh

HFLS = argv.h
OBJS = argv.o compat.o

CFLAGS = $(CCFLAGS)
TEST = argv_test

ifeq ($(strip $(TOP_LEVEL)),)
all: error
else
all : $(LIBRARY)
endif

error:
	@echo '+-------------------------------------------+'
	@echo '| Please run the top level Makefile instead |'
	@echo '|            of this one                    |'
	@echo '+-------------------------------------------+'

clean :
	rm -f a.out core *.o *.t *~ *.*~ *.a

$(LIBRARY) : $(OBJS)
	ar cr $(LIBRARY) $?
	ranlib $@
	- chmod 660 $@

# %.o : %.c
.c.o :
	rm -f $@
	$(CC) $(CFLAGS) $(CPPFLAGS) $(DEFS) $(INCS) -c $< 

###############################################################################
#
# These dependencies are automatically generated.  Do not edit by hand.
#

argv.o : argv.c conf.h argv.h argv_loc.h
argv_test.o : argv_test.c argv.h 
compat.o : compat.c argv_loc.h conf.h compat.h 
