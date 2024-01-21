#  Copyright 1993, UCAR/Unidata
#
#  Makefile for netCDF FORTRAN API under OS/2 using the EMX package
#  (gcc under OS/2) and AT&T's f2c utility.

include ../emx1.mak

PROGRAM		= ftest
LIBRARY		= ../libsrc/netcdf.lib
HEADER		= netcdf.inc

DEFINES		=
INCLUDES	= -I../libsrc -I../xdr

OBJS		= ftest.obj
LIBOBJS		= jackets.obj

LIBS		= ../libsrc/netcdf.lib ../xdr/xdr.lib $(F2CLIBS)


all:		netcdf.inc $(LIBRARY) $(PROGRAM).exe

test:		all FORCE
	$(PROGRAM)

install:	$(INCDIR)/$(HEADER)

clean::
	rm -f ftest.f jackets.c netcdf.inc *.cdf 


ftest.f:	dj32f2c/ftest.f
	$(CP) dj32f2c\ftest.f $@

jackets.c:	dj32f2c/jackets.c
	$(CP) dj32f2c\jackets.c $@

netcdf.inc:	dj32f2c/netcdf.inc
	$(CP) dj32f2c\netcdf.inc $@

$(PROGRAM).exe:	../libsrc/netcdf.lib ../xdr/xdr.lib

ftest.obj:	netcdf.inc
jackets.obj:	../libsrc/netcdf.h

include ../emx2.mak
