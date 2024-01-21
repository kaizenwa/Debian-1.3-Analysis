#   Copyright 1996 University Corporation for Atmospheric Research
#
#	PC Makefile for eXternal Data Representation library routines
#		used by the netCDF.

include ../emx1.mak

LIBRARY		= xdr.lib
LIBOBJS		= xdr.obj xdrarray.obj xdrfloat.obj xdrstdio.obj byteordr.obj

INCLUDES	= -I.

# The socket library is for swapl().
LIBS		= $(LIBRARY) -lsocket

all:		$(LIBRARY)

test:		xdrtest.exe FORCE
	xdrtest

install:	$(LIBDIR)/$(LIBRARY)

clean::		FORCE
	rm -f test.xdr

xdrtest.exe:	$(LIBRARY)

xdr.obj:	xdr.c types.h xdr.h
xdrfloat.obj:	xdrfloat.c types.h xdr.h
xdrarray.obj:	xdrarray.c types.h xdr.h
xdrtest.obj:	xdrtest.c types.h xdr.h
xdrstdio.obj:	xdrstdio.c types.h xdr.h
byteordr.obj:	byteordr.c

include ../emx2.mak
