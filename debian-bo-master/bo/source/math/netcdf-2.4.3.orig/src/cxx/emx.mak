# Copyright 1996 University Corporation for Atmospheric Research
#
# OS/2 Makefile for C++ netCDF library

include ../emx1.mak

INCLUDES	= -I../libsrc -I../xdr

DEFINES		= -DSTRSTREAM_H_SPEC="$(STRSTREAM_H_SPEC)"

LIBRARY		= netcdfxx.lib

LIBS		= $(LIBRARY) ../libsrc/netcdf.lib ../xdr/xdr.lib -lstdcpp

LIBOBJS		= netcdf.obj ncvalues.obj

PROGRAM		= nctst
OBJS		= nctst.obj

all:		$(LIBRARY) nctst.exe

test:		expected.dos nctst.exe $(FORCE)
	nctst > test.out
	$(DIFF) expected test.out
	@echo "*** C++ test successful ***"

install:	$(LIBDIR)/$(LIBRARY) \
		$(INCDIR)/netcdf.hh $(INCDIR)/ncvalues.hh 

clean::
	rm -f test.out example.nc

$(INCDIR)/netcdf.hh:	$(INCDIR) netcdf.hh
	$(CP) netcdf.hh $(INCDIR)
$(INCDIR)/ncvalues.hh:	$(INCDIR) ncvalues.hh
	$(CP) netcdf.hh $(INCDIR)

include ../emx2.mak
