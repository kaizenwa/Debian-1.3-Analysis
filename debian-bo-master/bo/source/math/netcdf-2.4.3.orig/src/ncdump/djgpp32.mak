#   Copyright 1989, University Corporation for Atmospheric Research
#
#  DOS Makefile for ncdump

include ../djgpp32a.mak

PROGRAM		= ncdump

INCLUDES	= -I../libsrc -I../xdr

OBJS		= ncdump.o vardata.o dumplib.o

LIBS		= ../libsrc/libnc.a ../xdr/libxdr.a

all:		$(PROGRAM).exe

test:		$(PROGRAM).exe test0.cdl ../ncgen/ncgen.exe FORCE
	../ncgen/ncgen -o test0.cdf -n test0.cdl
	./$(PROGRAM) test0.cdf > test1.cdl
	../ncgen/ncgen -o test1.cdf -n test1.cdl
	./$(PROGRAM) -n test0 test1.cdf > test2.cdl
	$(DIFF) test1.cdl test2.cdl 
	@echo "Test successful."

install:	$(BINDIR)/$(PROGRAM).exe

clean::
	rm -f test0.cdf test1.cdf test1.cdl test2.cdl

$(PROGRAM).o:	../libsrc/netcdf.h
vardata.o:	../libsrc/netcdf.h

include ../djgpp32b.mak
