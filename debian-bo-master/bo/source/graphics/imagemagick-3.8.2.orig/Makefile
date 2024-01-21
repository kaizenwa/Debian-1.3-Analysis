# Generated automatically from Makefile.in by configure.
#  Generic makefile for display, animate, montage, and import for
#  computers that do not have xmkmf.
#
#  Copyright 1997 E. I. du Pont de Nemours and Company
#
#  Permission to use, copy, modify, distribute, and sell this software and
#  its documentation for any purpose is hereby granted without fee,
#  provided that the above Copyright notice appear in all copies and that
#  both that Copyright notice and this permission notice appear in
#  supporting documentation, and that the name of E. I. du Pont de Nemours
#  and Company not be used in advertising or publicity pertaining to
#  distribution of the software without specific, written prior
#  permission.  E. I. du Pont de Nemours and Company makes no representations
#  about the suitability of this software for any purpose.  It is provided
#  "as is" without express or implied warranty.
#
#  E. I. du Pont de Nemours and Company disclaims all warranties with regard
#  to this software, including all implied warranties of merchantability
#  and fitness, in no event shall E. I. du Pont de Nemours and Company be
#  liable for any special, indirect or consequential damages or any
#  damages whatsoever resulting from loss of use, data or profits, whether
#  in an action of contract, negligence or other tortious action, arising
#  out of or in connection with the use or performance of this software.
#

# Set DoSharedLib to HasSharedLibraries to build ImageMagick with shared
# libraries.
#
# #define DoSharedLib  HasSharedLibraries

# By default, the maximum color value is 255.  To increase the maximum to
# 65535 define:
#
# QUANTUM_DEFINES= -DQuantumLeap

# If you have the HDF, JBIG, JPEG, MPEG, PNG, or TIFF libraries,
# uncomment HDF_*, JBIG_*, JPEG_*, MPEG_*, PNG_*, or TIFF_**
# respectively.  Make sure the path names are correct.  See README for
# more details.
#
#HDF_DEFINES= -DHasHDF
#HDF_INCLUDES= -I../hdf/hdf/src
#HDF_LIBRARIES= -Lhdf/hdf/src -ldf
#JBIG_DEFINES= -DHasJBIG
#JBIG_INCLUDES= -I../jbig/libjbig
#JBIG_LIBRARIES= -Ljbig/libjbig -ljbig
#JPEG_DEFINES= -DHasJPEG
#JPEG_INCLUDES= -I../jpeg
#JPEG_LIBRARIES= -Ljpeg -ljpeg
#MPEG_DEFINES= -DHasMPEG
#MPEG_INCLUDES= -I../mpeg
#MPEG_LIBRARIES= -Lmpeg -lmpeg
#PNG_DEFINES= -DHasPNG
#PNG_INCLUDES= -I../png -I../zlib
#PNG_LIBRARIES= -Lpng -lpng -Lzlib -lz
#TIFF_DEFINES= -DHasTIFF
#TIFF_INCLUDES= -I../tiff/libtiff
#TIFF_LIBRARIES= -Ltiff/libtiff -ltiff

MAGICK_INCLUDES= -Imagick

CC = gcc
DEFS =  -DSTDC_HEADERS=1 -DHAVE_DIRENT_H=1 -DSVR4=1 -DHasSharedMemory=1 -DHasShape=1 
INSTALL = ./install-sh -c
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_PROGRAM = ${INSTALL}
LDFLAGS =
LIBS =  -ltiff -lsocket -lXext -lm
X_INCLUDES =  -I/usr/openwin/include
X_LIBRARIES =  -L/usr/openwin/lib -R/usr/openwin/lib -lsocket  -lnsl -lX11

LOCALDIR= /usr/local/bin
LOCALMAN= /usr/local/man/mann

DEFINES= $(DEFS) $(MAGICK_INCLUDES) $(DIRENT_DEFINES) $(EXTENSIONS_DEFINES) \
  $(QUANTUM_DEFINES) $(DPS_DEFINES) $(DPS_INCLUDES) $(HDF_DEFINES) \
  $(HDF_INCLUDES) $(JBIG_DEFINES) $(JBIG_INCLUDES) $(JPEG_DEFINES) \
  $(JPEG_INCLUDES) $(MPEG_DEFINES) $(MPEG_INCLUDES) $(PNG_DEFINES) \
  $(PNG_INCLUDES) $(TIFF_DEFINES) $(TIFF_INCLUDES) $(X_INCLUDES)

SYS_LIBRARIES= $(DPS_LIBRARIES) $(JBIG_LIBRARIES) $(HDF_LIBRARIES) \
  $(MPEG_LIBRARIES) $(PNG_LIBRARIES) $(TIFF_LIBRARIES) $(JPEG_LIBRARIES) \
  $(X_LIBRARIES) $(LIBS)

DisplayObjects= display.o
ImportObjects= import.o
MontageObjects= montage.o
AnimateObjects= animate.o
ConvertObjects= convert.o
IdentifyObjects= identify.o
MogrifyObjects= mogrify.o
CombineObjects= combine.o

PROGRAMS= display import montage animate convert mogrify identify combine

CFLAGS= -O $(DEFINES)

all: libMagick.a $(PROGRAMS)

libMagick.a: 
	(cd magick; make CFLAGS="$(CFLAGS)")

display: $(DisplayObjects) magick/libMagick.a
	rm -f $@
	$(CC) -o $@ $(DisplayObjects) magick/libMagick.a $(SYS_LIBRARIES)

import: $(ImportObjects) magick/libMagick.a
	rm -f $@
	$(CC) -o $@ $(ImportObjects) magick/libMagick.a $(SYS_LIBRARIES)

montage: $(MontageObjects) magick/libMagick.a
	rm -f $@
	$(CC) -o $@ $(MontageObjects) magick/libMagick.a $(SYS_LIBRARIES)

animate: $(AnimateObjects) magick/libMagick.a
	rm -f $@
	$(CC) -o $@ $(AnimateObjects) magick/libMagick.a $(SYS_LIBRARIES)

convert: $(ConvertObjects) magick/libMagick.a
	rm -f $@
	$(CC) -o $@ $(ConvertObjects) magick/libMagick.a $(SYS_LIBRARIES)

identify: $(IdentifyObjects) magick/libMagick.a
	rm -f $@
	$(CC) -o $@ $(IdentifyObjects) magick/libMagick.a $(SYS_LIBRARIES)

mogrify: $(MogrifyObjects) magick/libMagick.a
	rm -f $@
	$(CC) -o $@ $(MogrifyObjects) magick/libMagick.a $(SYS_LIBRARIES)

combine: $(CombineObjects) magick/libMagick.a
	rm -f $@
	$(CC) -o $@ $(CombineObjects) magick/libMagick.a $(SYS_LIBRARIES)


install: display import montage animate convert mogrify combine
	$(INSTALL_PROGRAM) display $(LOCALDIR)/display
	$(INSTALL_DATA) display.man $(LOCALMAN)/display.n
	$(INSTALL_PROGRAM) import $(LOCALDIR)/import
	$(INSTALL_DATA) import.man $(LOCALMAN)/import.n
	$(INSTALL_PROGRAM) montage $(LOCALDIR)/montage
	$(INSTALL_DATA) montage.man $(LOCALMAN)/montage.n
	$(INSTALL_PROGRAM) animate $(LOCALDIR)/animate
	$(INSTALL_DATA) animate.man $(LOCALMAN)/animate.n
	$(INSTALL_PROGRAM) convert $(LOCALDIR)/convert
	$(INSTALL_DATA) convert.man $(LOCALMAN)/convert.n
	$(INSTALL_PROGRAM) identify $(LOCALDIR)/identify
	$(INSTALL_DATA) identify.man $(LOCALMAN)/identify.n
	$(INSTALL_PROGRAM) mogrify $(LOCALDIR)/mogrify
	$(INSTALL_DATA) mogrify.man $(LOCALMAN)/mogrify.n
	$(INSTALL_PROGRAM) combine $(LOCALDIR)/combine
	$(INSTALL_DATA) combine.man $(LOCALMAN)/combine.n
	$(INSTALL_DATA) miff.man $(LOCALMAN)/miff.n
	$(INSTALL_DATA) quantize.man $(LOCALMAN)/quantize.n
	$(INSTALL_DATA) ImageMagick.man $(LOCALMAN)/ImageMagick.n

clean::
	(cd magick; make clean)
	rm -f *.ln *.bak *.o core errs ,* *~ *.a .emacs_* make.log MakeOut
	rm -f config.cache
	rm -f display
	rm -f import
	rm -f montage
	rm -f animate
	rm -f convert
	rm -f mogrify
	rm -f identify
	rm -f combine

display.o: display.c magick/magick.h
import.o: import.c magick/magick.h
montage.o: montage.c magick/magick.h
animate.o: animate.c magick/magick.h
convert.o: convert.c magick/magick.h
mogrify.o: mogrify.c magick/magick.h
combine.o: combine.c magick/magick.h
