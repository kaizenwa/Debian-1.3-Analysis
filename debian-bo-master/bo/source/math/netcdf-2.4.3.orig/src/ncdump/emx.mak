#   Copyright 1996, University Corporation for Atmospheric Research
#
#  EMS Makefile for ncdump

include ../emx1.mak

PROGRAM		= ncdump

INCLUDES	= -I../libsrc -I../xdr

OBJS		= ncdump.obj vardata.obj dumplib.obj

LIBS		= ../libsrc/netcdf.lib ../xdr/xdr.lib

all:		$(PROGRAM).exe

test:		$(PROGRAM).exe test0.cdl ../ncgen/ncgen.exe FORCE
	..\ncgen\ncgen -o test0.cdf -n test0.cdl
	$(PROGRAM) test0.cdf > test1.cdl
	..\ncgen\ncgen -o test1.cdf -n test1.cdl
	$(PROGRAM) -n test0 test1.cdf > test2.cdl
	$(DIFF) test1.cdl test2.cdl 
	@echo "Test successful."

install:	$(BINDIR)/$(PROGRAM).exe

clean::
	rm -f test0.cdf test1.cdf test1.cdl test2.cdl

$(PROGRAM).exe:	../libsrc/netcdf.lib ../xdr/xdr.lib

$(PROGRAM).obj:	../libsrc/netcdf.h $(PROGRAM).c
vardata.obj:	../libsrc/netcdf.h vardata.c
dumplib.obj:	dumplib.c

include ../emx2.mak
