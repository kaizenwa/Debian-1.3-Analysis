#
# Makefile template for a Yorick-based code
#
# $Id: Makefile,v 1.1 1993/09/02 22:48:38 munro Exp munro $

# ---------------------------------------------------------------------------

# The Yorick/include/make.i library file contains interpreted code that can
# find the Maketmpl file at this site, and which can generate portions
# of this Makefile automatically.
MAKE_TEMPLATE = ../Yorick/Maketmpl

C_OPTIMIZE = -O
LD_OPTIMIZE = $(C_OPTIMIZE)

CODE_NAME = drat
NON_SHARABLE = unused

OBJS = bound.o track.o trans.o drat.o ydrat.o

CODE_LIBRARY = libdrat.a

PKG_LIBS = -ldrat -lyorm
PKG_OBJS =

Y_INCLUDE = drat.i
Y_OTHERS = fft.i matrix.i

# Use
# Y_OTHERS = drat.i
# PKG_LIBS = -ldrat
# to put the drat routines into another custom version of Yorick

YWRAP_O =

#SYS_LIBS = /home/munro/efenc205/libefence.a
SYS_LIBS =

CLEAN_UP = drat
TIDY_UP=

# ---------------------------------------------------------------------------

# all:: $(NON_SHARABLE)
all:: $(CODE_LIBRARY) $(CODE_NAME) startup

startup::
	../install-sh 0 ../Yorick/startup
	cp $(Y_INCLUDE) ../Yorick/startup

check: all
	./drat -batch check.i
	$(RM) junk*

DESTDIR =

install: $(CODE_LIBRARY) installdirs
	cp $(Y_INCLUDE) $(DESTDIR)$(Y_SITE)/startup
	../install-sh 4 $(DESTDIR)$(Y_BINDIR) drat
	cp libdrat.a $(DESTDIR)$(Y_LIBDIR)
	$(RANLIB) $(DESTDIR)$(Y_LIBDIR)/libdrat.a

uninstall:
	cd $(DESTDIR)$(Y_SITE)/startup; $(RM) $(Y_INCLUDE);
	$(RM) $(DESTDIR)$(Y_LIBDIR)/libdrat.a

installdirs:
	../install-sh 0 $(DESTDIR)$(Y_SITE)
	../install-sh 0 $(DESTDIR)$(Y_SITE)/startup
	../install-sh 0 $(DESTDIR)$(Y_BINDIR)
	../install-sh 0 $(DESTDIR)$(Y_LIBDIR)

distclean: clean
	$(SED) -n -e "1,/^# --END-CODE/p" Makefile >M.1
	cat M.1 >Makefile
	$(RM) M.1

# Note: indirectly included headers separated by three spaces

bound.o: bound.h
track.o: track.h   bound.h
trans.o: trans.h   track.h bound.h
drat.o: drat.h   trans.h track.h bound.h
ydrat.o: drat.h   trans.h track.h bound.h

# my_code2.o: my_code2.c my_code.h
# 	$(CC) $(CFLAGS) -DSPECIAL_SWITCH -c my_code2.c

# ---------------------------------------------------------------------------

include $(MAKE_TEMPLATE)
