#   Copyright 1993,1996 University Corporation for Atmospheric Research
#
#  DOS Makefile for netCDF library

include ../emx1.mak

HEADER		= netcdf.h

INCLUDES	= -I../xdr
DEFINES		= -DSWAP -DDOS_FS

LIBRARY		= netcdf.lib
LIBS		= $(LIBRARY) ../xdr/xdr.lib -lsocket

LIBOBJS		= array.obj attr.obj cdf.obj dim.obj error.obj file.obj \
		  globdef.obj iarray.obj putget.obj putgetg.obj sharray.obj \
		  string.obj var.obj xdrposix.obj

all:		local_nc.h $(LIBRARY)

test:		cdftest.exe FORCE
	./cdftest

install:	$(LIBDIR)/$(LIBRARY) $(INCDIR)/$(HEADER)

clean::
	rm -f test.cdf local_nc.h

cdftest.exe:	$(LIBRARY)

local_nc.h:     local_nc.gen
	$(CP) $? $@

array.obj: array.c
array.obj: ./local_nc.h
array.obj: ./netcdf.h
array.obj: ./alloc.h
attr.obj: attr.c
attr.obj: ./local_nc.h
attr.obj: ./netcdf.h
attr.obj: ./alloc.h
cdf.obj: cdf.c
cdf.obj: ./local_nc.h
cdf.obj: ./netcdf.h
cdf.obj: ./alloc.h
cdftest.obj: cdftest.c
cdftest.obj: ./netcdf.h
dim.obj: dim.c
dim.obj: ./local_nc.h
dim.obj: ./netcdf.h
dim.obj: ./alloc.h
error.obj: error.c
error.obj: ./local_nc.h
error.obj: ./netcdf.h
file.obj: file.c
file.obj: ./local_nc.h
file.obj: ./netcdf.h
file.obj: ./alloc.h
globdef.obj: globdef.c
globdef.obj: ./netcdf.h
iarray.obj: iarray.c
iarray.obj: ./local_nc.h
iarray.obj: ./netcdf.h
iarray.obj: ./alloc.h
putget.obj: putget.c
putget.obj: ./local_nc.h
putget.obj: ./netcdf.h
putget.obj: ./alloc.h
putgetg.obj: putgetg.c
putgetg.obj: ./local_nc.h
putgetg.obj: ./netcdf.h
sharray.obj: sharray.c
sharray.obj: ./local_nc.h
sharray.obj: ./netcdf.h
sharray.obj: ./alloc.h
string.obj: string.c
string.obj: ./local_nc.h
string.obj: ./netcdf.h
string.obj: ./alloc.h
var.obj: var.c
var.obj: ./local_nc.h
var.obj: ./netcdf.h
var.obj: ./alloc.h
xdrposix.obj: xdrposix.c
xdrposix.obj: ./netcdf.h
xdrposix.obj: ./local_nc.h
xdrposix.obj: ./netcdf.h

include ../emx2.mak
