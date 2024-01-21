#
# Makefile for MPY = Message Passing Yorick based on the MPI standard
#
# $Id$

MPY_VERSION=1.0

# ---------------------------------------------------------------------------

# The Make-mpy template must be built by the configure script before
# this Makefile can be used.  See README and configure for details.
MAKE_TEMPLATE = ./Make-mpy

C_OPTIMIZE = -O
LD_OPTIMIZE = $(C_OPTIMIZE)

CODE_NAME = mpy
NON_SHARABLE = unused

OBJS = mpy.o

CODE_LIBRARY = libmpy.a

PKG_LIBS = -lmpy -lyorm
PKG_OBJS =

Y_INCLUDE = mpy.i
Y_OTHERS = fft.i matrix.i

# Use
# MAKE_TEMPLATE = $(Y_HOME)/Make-mpy
# Y_OTHERS = mpy.i
# PKG_LIBS = -lmpy
# to put the mpy routines into another custom version of Yorick

YWRAP_O =

SYS_LIBS =

CLEAN_UP = mpy
TIDY_UP=

# ---------------------------------------------------------------------------

all:: $(CODE_LIBRARY) $(CODE_NAME) startup

startup::
	../install-sh 0 ../Yorick/startup
	cp $(Y_INCLUDE) ../Yorick/startup

check: all
	echo "see testmp.i for testing procedures"

install: $(CODE_LIBRARY) $(CODE_NAME) installdirs
	cp Make-mpy $(Y_HOME)
	cp $(Y_INCLUDE) $(Y_SITE)/startup
	cp testmp.i $(Y_SITE)/include
	cp mpy $(Y_BINDIR)
	cp libmpy.a $(Y_LIBDIR)
	$(RANLIB) $(Y_LIBDIR)/libmpy.a

uninstall:
	$(RM) $(Y_HOME)/Make-mpy
	cd $(Y_SITE)/startup; $(RM) $(Y_INCLUDE);
	$(RM) $(Y_SITE)/include/testmp.i
	$(RM) $(Y_BINDIR)/mpy
	$(RM) $(Y_LIBDIR)/libmpy.a

installdirs:
	../install-sh 0 $(Y_SITE)
	../install-sh 0 $(Y_SITE)/startup
	../install-sh 0 $(Y_BINDIR)
	../install-sh 0 $(Y_LIBDIR)

distclean: clean
	$(RM) Make-mpy

MPY_DIR=MPY-$(MPY_VERSION)

distrib: tidy
	$(RM) -r $(MPY_DIR) $(MPY_DIR).tar.gz
	mkdir $(MPY_DIR)
	cp README Makefile configure mp*-sh mpy.c mpy.i testmp.i $(MPY_DIR)
	tar cvf $(MPY_DIR).tar $(MPY_DIR)
	$(RM) -r $(MPY_DIR)
	gzip $(MPY_DIR).tar

# Note: indirectly included headers separated by three spaces

# bound.o: bound.h
# track.o: track.h   bound.h
# trans.o: trans.h   track.h bound.h
# drat.o: drat.h   trans.h track.h bound.h
# ydrat.o: drat.h   trans.h track.h bound.h

# my_code2.o: my_code2.c my_code.h
# 	$(CC) $(CFLAGS) -DSPECIAL_SWITCH -c my_code2.c

mpy.o: mpy.c
	$(CC) $(CFLAGS) $(MPI_DEFS) $(MPI_INCLUDE) -c mpy.c

# ---------------------------------------------------------------------------

include $(MAKE_TEMPLATE)
