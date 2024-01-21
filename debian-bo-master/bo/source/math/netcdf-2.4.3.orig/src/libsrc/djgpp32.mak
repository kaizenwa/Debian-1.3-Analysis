#   Copyright 1993,1996 University Corporation for Atmospheric Research
#
#  DOS Makefile for netCDF library

include ../djgpp32a.mak

HEADER		= netcdf.h

INCLUDES	= -I../xdr
DEFINES		= -DSWAP -DNO_SYSTEM_XDR_INCLUDES -DDOS_FS

LIBRARY		= libnc.a
LIBS		= $(LIBRARY) ../xdr/libxdr.a

LIBOBJS		= array.o attr.o cdf.o dim.o error.o file.o globdef.o \
		  iarray.o putget.o putgetg.o sharray.o string.o var.o \
		  xdrposix.o

all:		$(LIBRARY)

test:		cdftest.exe FORCE
	./cdftest

install:	$(LIBDIR)/$(LIBRARY) $(INCDIR)/$(HEADER)

clean::
	rm -f test.cdf local_nc.h

local_nc.h:	local_nc.gen
	$(CP) $? $@

array.o: array.c
array.o: ./local_nc.h
array.o: ./netcdf.h
array.o: ./alloc.h
attr.o: attr.c
attr.o: ./local_nc.h
attr.o: ./netcdf.h
attr.o: ./alloc.h
cdf.o: cdf.c
cdf.o: ./local_nc.h
cdf.o: ./netcdf.h
cdf.o: ./alloc.h
cdftest.o: cdftest.c
cdftest.o: ./netcdf.h
dim.o: dim.c
dim.o: ./local_nc.h
dim.o: ./netcdf.h
dim.o: ./alloc.h
error.o: error.c
error.o: ./local_nc.h
error.o: ./netcdf.h
file.o: file.c
file.o: ./local_nc.h
file.o: ./netcdf.h
file.o: ./alloc.h
globdef.o: globdef.c
globdef.o: ./netcdf.h
iarray.o: iarray.c
iarray.o: ./local_nc.h
iarray.o: ./netcdf.h
iarray.o: ./alloc.h
putget.o: putget.c
putget.o: ./local_nc.h
putget.o: ./netcdf.h
putget.o: ./alloc.h
putgetg.o: putgetg.c
putgetg.o: ./local_nc.h
putgetg.o: ./netcdf.h
sharray.o: sharray.c
sharray.o: ./local_nc.h
sharray.o: ./netcdf.h
sharray.o: ./alloc.h
string.o: string.c
string.o: ./local_nc.h
string.o: ./netcdf.h
string.o: ./alloc.h
var.o: var.c
var.o: ./local_nc.h
var.o: ./netcdf.h
var.o: ./alloc.h
xdrposix.o: xdrposix.c
xdrposix.o: ./netcdf.h
xdrposix.o: ./local_nc.h
xdrposix.o: ./netcdf.h

include ../djgpp32b.mak
