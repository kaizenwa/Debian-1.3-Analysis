#
# Makefile for Yorick FFT and Matrix analysis packages
#
# $Id$
#

# ---------------------------------------------------------------------------

# The Yorick/include/make.i library file contains interpreted code that can
# find the Maketmpl file at this site, and which can generate portions
# of this Makefile automatically.
MAKE_TEMPLATE = ../Yorick/Maketmpl

C_OPTIMIZE = -O
LD_OPTIMIZE = $(C_OPTIMIZE)

CODE_NAME = yorm
NON_SHARABLE = unused

OBJS = cfft2.o cfftb.o cfftf.o cffti.o roll2.o dlamc3.o dgyor.o dgtsv.o \
       dgesv.o dgecon.o dgels.o dgelss.o dbdsqr.o dlasr.o dgesvd.o dgesv2.o \
       dgblas.o

CODE_LIBRARY = libyorm.a

# PKG_LIBS =
# PKG_OBJS = $(OBJS) $(YWRAP_O)
PKG_LIBS = -lyorm
PKG_OBJS =

Y_INCLUDE = fft.i matrix.i
Y_OTHERS =

# Use
# Y_OTHERS = fft.i matrix.i
# PKG_LIBS = -lyorm
# to put the fft and matrix routines into another custom version of Yorick

# YWRAP_O =
YWRAP_O = ywrap.o

SYS_LIBS =

# Set any special files to be zapped by the clean and tidy targets.
# (make clean removes both TIDY_UP and CLEAN_UP)
CLEAN_UP = yorm
TIDY_UP=

# ---------------------------------------------------------------------------

# all:: $(NON_SHARABLE)
all:: $(CODE_LIBRARY) $(CODE_NAME) startup

startup::
	../install-sh 0 ../Yorick/startup
	cp $(Y_INCLUDE) ../Yorick/startup

check: all
	./yorm -batch check.i
	$(RM) junk*

DESTDIR =

install: $(CODE_LIBRARY) installdirs
	cp $(Y_INCLUDE) $(DESTDIR)$(Y_SITE)/startup
	cp libyorm.a $(DESTDIR)$(Y_LIBDIR)
	$(RANLIB) $(DESTDIR)$(Y_LIBDIR)/libyorm.a

uninstall:
	cd $(DESTDIR)$(Y_SITE)/startup; $(RM) $(Y_INCLUDE);
	$(RM) $(DESTDIR)$(Y_LIBDIR)/libyorm.a

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

# my_code1.o: my_code1.h   my_code.h
# my_code2.o: my_code2.c my_code.h
#	$(CC) $(CFLAGS) -DSPECIAL_SWITCH -c my_code2.c
# my_code1.o: my_code3.h   my_code1.h my_code.h

# Compile this one non-optimized
# The real problem may be in the dgecon.c routines which call this in
# an attempt to compute things like the smallest x such that 1.0+x!=1.0.
# This patch works for Linux, which was the only place I have had
# problems.
dlamc3.o: dlamc3.c
	$(CC) $(CPPFLAGS) -g -c dlamc3.c

# ---------------------------------------------------------------------------

include $(MAKE_TEMPLATE)
