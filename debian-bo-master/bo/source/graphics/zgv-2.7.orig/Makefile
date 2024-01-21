# Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
# Copyright (C) 1993-1995 Russell Marks. See README for license details.
#
# Makefile for zgv
#
# set BINDIR to directory for binaries, MANDIR to directory for man page,
# and JPEGDIR to directory where you have the Independant JPEG Group's
# JPEG software.
#
# Note you need the source to the JPEG software, or at least the
# required header files and libjpeg.a!
#
# Note also that YOU MUST USE VERSION *FIVE* of the JPEG library.
# Version four will no longer work, as the interfaces are incompatible.

# comment out if you don't want PNG support
PNGDEF = -DPNG_SUPPORT

BINDIR = /usr/bin
MANDIR = /usr/man/man1
JPEGDIR = /usr/src/jpeg-5

# if you want PNG support (see README.src for details) specify the
#  dirs the pnglib and zlib are below.
# if you don't want PNG support, don't worry; it doesn't hurt having
#  non-existant directories included, so leave them as they are.
PNGLIBDIR = /usr/src/pnglib
ZLIBDIR = /usr/src/zlib

INCDIRS = -I$(JPEGDIR) -I$(PNGLIBDIR) -I$(ZLIBDIR)



CC=gcc

# K&R-ish opts until I get around to ANSIfying the code
WARNOPTS = -Wall -Wno-implicit -Wno-return-type

#CFLAGS = $(INCDIRS) $(WARNOPTS) $(PNGDEF) -g	# debugging
#CFLAGS = $(INCDIRS) $(WARNOPTS) $(PNGDEF)	# quick compilation
# optimisation for proper versions :-)
CFLAGS = $(INCDIRS) $(WARNOPTS) $(PNGDEF) -O2 \
 -fomit-frame-pointer -finline-functions


# You shouldn't need to modify anything below this line.

ZGV_LIBS = -lvgagl -lvga $(JPEGDIR)/libjpeg.a
ifeq ($(PNGDEF),-DPNG_SUPPORT)
ZGV_LIBS += $(PNGLIBDIR)/pnglib.a $(ZLIBDIR)/libgz.a -lm
endif

all : zgv

ZGV_OBJS = zgv.o gifeng.o vgadisp.o readjpeg.o font.o 3deffects.o \
	readnbkey.o rcfile.o zgvlogopck.o helppage.o readpnm.o \
	readbmp.o readtga.o readpng.o magic.o resizepic.o mouse.o

MUNGLOGO_OBJS = munglogo.o gifeng.o

zgv : $(ZGV_OBJS)
	$(CC) $(CFLAGS) -o zgv $(ZGV_OBJS) $(ZGV_LIBS)

munglogo: $(MUNGLOGO_OBJS)
	$(CC) $(CFLAGS) -o munglogo $(MUNGLOGO_OBJS)

install : all
	install -m 4511 -o root -s zgv $(BINDIR)
	install -m 444 zgv.1 $(MANDIR)

clean :
	$(RM) *.o *~ zgv munglogo core.* zgvlogo.h

# Dependancies
3deffects.o: 3deffects.c 3deffects.h readnbkey.h zgv.h
font.o: font.c font.h
gifeng.o: gifeng.c zgv.h gifeng.h
helppage.o: helppage.c 3deffects.h font.h readnbkey.h rc_config.h mouse.h \
  rcfile.h
magic.o: magic.c magic.h
mouse.o: mouse.c mouse.h
munglogo.o: munglogo.c zgv.h gifeng.h
rcfile.o: rcfile.c rc_config.h mouse.h zgv.h
readbmp.o: readbmp.c readbmp.h zgv.h rcfile.h rc_config.h mouse.h
readjpeg.o: readjpeg.c 3deffects.h zgv.h readjpeg.h vgadisp.h rc_config.h \
  mouse.h rcfile.h
readnbkey.o: readnbkey.c readnbkey.h
readpnm.o: readpnm.c zgv.h rcfile.h rc_config.h mouse.h
readtga.o: readtga.c zgv.h rcfile.h rc_config.h mouse.h readtga.h
resizepic.o: resizepic.c resizepic.h
vgadisp.o: vgadisp.c zgv.h magic.h gifeng.h readjpeg.h readpnm.h readbmp.h \
  readtga.h readnbkey.h helppage.h 3deffects.h rc_config.h mouse.h rcfile.h \
  vgadisp.h
zgv.o: zgv.c zgv.h gifeng.h vgadisp.h readnbkey.h font.h 3deffects.h helppage.h \
  rc_config.h mouse.h rcfile.h readjpeg.h readpnm.h resizepic.h zgvlogopck.h
zgvlogopck.o: zgvlogopck.c zgvlogo.h

# more involved dependancy stuff
zgvlogo.h : munglogo zgvlogo.gif
	./munglogo
