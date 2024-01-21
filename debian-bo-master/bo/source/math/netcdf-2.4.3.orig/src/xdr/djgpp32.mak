#   Copyright 1989, 1996 University Corporation for Atmospheric Research
#
#	PC Makefile for eXternal Data Representation library routines
#		used by the netCDF.

include ../djgpp32a.mak

LIBRARY		= libxdr.a
LIBOBJS		= xdr.o xdrarray.o xdrfloat.o xdrstdio.o byteordr.o

INCLUDES	= -I.

LIBS		= $(LIBRARY)

all:		$(LIBRARY)

test:		xdrtest.exe FORCE
	./xdrtest

install:	$(LIBDIR)/$(LIBRARY)

clean::		FORCE
	rm -f test.xdr

xdr.o:		xdr.c types.h xdr.h
xdrfloat.o:	xdrfloat.c types.h xdr.h
xdrarray.o:	xdrarray.c types.h xdr.h
xdrtest.o:	xdrtest.c types.h xdr.h
xdrstdio.o:	xdrstdio.c types.h xdr.h
byteordr.o:	byteordr.c

include ../djgpp32b.mak
