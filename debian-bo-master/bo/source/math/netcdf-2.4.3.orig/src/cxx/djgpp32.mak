#   Copyright 1996 University Corporation for Atmospheric Research
#
#  DOS Makefile for C++ netCDF library

include ../djgpp32a.mak

INCLUDES	= -I../libsrc -I../xdr

LIBRARY		= libncxx.a

LIBS		= $(LIBRARY) ../libsrc/libnc.a ../xdr/libxdr.a -lgpp

LIBOBJS		= netcdf.o ncvalues.o

PROGRAM		= nctst
OBJS		= nctst.o

all:		$(LIBRARY) $(PROGRAM).exe

test:		expected.dos $(PROGRAM).exe $(FORCE)
	./$(PROGRAM) > test.out
	$(DIFF) expected.dos test.out
	@echo "*** C++ test successful ***" ;

install:	$(LIBDIR)/$(LIBRARY) \
		$(INCDIR)/netcdf.hh $(INCDIR)/ncvalues.hh 

clean::
	rm -f test.out example.nc

$(INCDIR)/netcdf.hh:	$(INCDIR) netcdf.hh
	$(CP) netcdf.hh $(INCDIR)
$(INCDIR)/ncvalues.hh:	$(INCDIR) ncvalues.hh
	$(CP) netcdf.hh $(INCDIR)

include ../djgpp32b.mak
