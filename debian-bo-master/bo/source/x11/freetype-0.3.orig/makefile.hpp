# This file is part of the FreeType project.
#
# It builds the library and test programs for cc (and X11) on HP-UX 9.0x.
#
# FreeType can be compiled either with HP C compiler (HP92453-01 A.09.61 
# HP C Compiler, as reported by ``what /bin/cc'') and GNU C compiler
# (2.6.3; later should work as well).  Please comment ``MAKE=gmake'' if
# you use HP make; and substitute all ``makefile.gcc'' in this makefile 
# with ``makefile.hpp'' if you want to use HP stock C compiler.
#
# Makefile for HP-UX: Primoz Peterlin <primoz.peterlin@biofiz.mf.uni-lj.si>

#  Put "MAKE=gmake" for GNU make
MAKE= gmake
#  HP make needs this
RM= rm

#  Put "CC=cc" for HP C compiler, "CC=gcc" for GNU C compiler
CC= gcc 
INCDIRS= -Ilib -I/usr/include/X11R5
#  HP C compiler
#CFLAGS= -Aa -O $(INCDIRS) -DX11
#  GNU C compiler
CFLAGS= -O -g $(INCDIRS) -DX11

SRC=  zoom.c timer.c win_x11.c

OBJ=  win_x11.o
OBJ1= zoom.o
OBJ2= timer.o


all: freetype timer zoom

freetype:
	cd lib; $(MAKE) -f makefile.gcc all

zoom: $(OBJ) $(OBJ1) lib/libttf.a
	$(CC) $(CFLAGS) -o $@ $(OBJ) $(OBJ1) lib/libttf.a \
	-lm -L/usr/lib/X11R5 -lX11 

timer: $(OBJ) $(OBJ2) lib/libttf.a
	$(CC) $(CFLAGS) -o $@ $(OBJ) $(OBJ2) lib/libttf.a \
	-lm -L/usr/lib/X11R5 -lX11

clean: do_clean
	cd lib; $(MAKE) -f makefile.gcc clean

distclean: do_clean
	cd lib; $(MAKE) -f makefile.gcc distclean
	-$(RM) dep.end zoom timer

do_clean:
	-$(RM) $(OBJ) $(OBJ1) $(OBJ2)

depend:
	cd lib; $(MAKE) -f makefile.gcc depend
	$(CC) -E $(INCDIRS) $(SRC) > dep.end
