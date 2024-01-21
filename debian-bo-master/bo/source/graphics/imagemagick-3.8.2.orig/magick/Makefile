# Generated automatically from Makefile.in by configure.
#
#  Generic makefile for the ImageMagick image library for computers that do
#  not have xmkmf.
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

SHELL = /bin/sh

AR = ar
CC = gcc
DEFS =  -DSTDC_HEADERS=1 -DHAVE_DIRENT_H=1 -DSVR4=1 -DHasSharedMemory=1 -DHasShape=1 
INSTALL = .././install-sh -c
INSTALL_DATA = ${INSTALL} -m 644
RANLIB = ranlib

CFLAGS= $(DEFS)
CDEBUGFLAGS= $(DEFS)

prefix=/usr/local
exec_prefix=${prefix}
LOCALLIB=${exec_prefix}/lib
LOCALINC=${prefix}/include

MagickObjects= widget.o X.o image.o effects.o shear.o segment.o quantize.o \
  colors.o gems.o signature.o decode.o encode.o compress.o utility.o monitor.o \
  error.o PreRvIcccm.o

all: libMagick.a

libMagick.a: $(MagickObjects)
	rm -f libMagick.a
	$(AR) cru libMagick.a $(MagickObjects)
	$(RANLIB) libMagick.a

install: libMagick.a
	$(INSTALL_DATA) libMagick.a $(LOCALLIB)/libMagick.a
	$(INSTALL_DATA) PreRvIcccm.h $(LOCALINC)/PreRvIcccm.h
	$(INSTALL_DATA) X.h $(LOCALINC)/X.h
	$(INSTALL_DATA) compress.h $(LOCALINC)/compress.h
	$(INSTALL_DATA) image.h $(LOCALINC)/image.h
	$(INSTALL_DATA) gems.h $(LOCALINC)/gems.h
	$(INSTALL_DATA) magick.h $(LOCALINC)/magick.h
	$(INSTALL_DATA) utility.h $(LOCALINC)/utility.h
	$(INSTALL_DATA) widget.h $(LOCALINC)/widget.h
	$(INSTALL_DATA) plug-ins.h $(LOCALINC)/plug-ins.h

clean: 
	rm -f *.o libMagick.a core

widget.o:  widget.c magick.h
X.o:  X.c magick.h Colorlist.h
image.o:  image.c magick.h gems.h formats.h Colorlist.h
gems.o:  gems.c magick.h
shear.o:  shear.c magick.h
quantize.o:  quantize.c magick.h
colors.o:  colors.c magick.h Colorlist.h
signature.o:  signature.c magick.h
decode.o:  decode.c magick.h plug-ins.h
encode.o:  encode.c magick.h Colorlist.h plug-ins.h
compress.o:  compress.c magick.h
utility.o:  utility.c magick.h Colorlist.h
error.o:  error.c magick.h
monitor.o:  monitor.c magick.h
PreRvIcccm.o:  PreRvIcccm.c magick.h
