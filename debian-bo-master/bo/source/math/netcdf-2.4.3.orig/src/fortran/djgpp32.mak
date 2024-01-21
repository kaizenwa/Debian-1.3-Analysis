#  Copyright 1993, UCAR/Unidata
#
#  Makefile for netCDF FORTRAN API under MS/DOS using the DJGPP package
#  (gcc under MS/DOS) and AT&T's f2c utility.

include ../djgpp32a.mak

PROGRAM		= ftest
LIBRARY		= ../libsrc/libnc.a
HEADER		= netcdf.inc

DEFINES		= -DNO_SYSTEM_XDR_INCLUDES
INCLUDES	= -I../libsrc -I../xdr

OBJS		= ftest.o
LIBOBJS		= jackets.o

LIBS		= ../libsrc/libnc.a ../xdr/libxdr.a -lf2c -lm


all:		netcdf.inc $(LIBRARY) $(PROGRAM).exe

test:		all FORCE
	./$(PROGRAM)

install:	$(INCDIR)/$(HEADER)

clean::
	rm -f ftest.f jackets.c netcdf.inc *.cdf 


ftest.f:	dj32f2c/ftest.f
	copy dj32f2c\ftest.f $@

jackets.c:	dj32f2c/jackets.c
	copy dj32f2c\jackets.c $@

netcdf.inc:	dj32f2c/netcdf.inc
	copy dj32f2c\netcdf.inc $@

ftest.o:	netcdf.inc
jackets.o:	../libsrc/netcdf.h

include ../djgpp32b.mak
