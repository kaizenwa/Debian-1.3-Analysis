#  Copyright 1993, UCAR/Unidata
#
#  DOS makefile for ncgen

include ../djgpp32a.mak

PROGRAM		= ncgen

INCLUDES	= -I../libsrc

LIBS		= ../libsrc/libnc.a ../xdr/libxdr.a

OBJS		= main.o $(PROGRAM)tab.o load.o escapes.o \
		  getfill.o init.o close.o genlib.o


all:		$(PROGRAM).exe

test:   	$(PROGRAM).exe test0.cdl ntest ctest $(FTEST) FORCE

install:	$(BINDIR)/$(PROGRAM).exe

clean::
	rm -f $(PROGRAM)tab.c $(PROGRAM)tab.h $(PROGRAM)yy.c \
	    test0 test0.nc test0.for test0.exe test1.nc test1.cdl test2.cdl \
		ftest0.nc ftest1.cdl test0.c ctest0.nc ctest1.cdl

# test "-b" option of $(PROGRAM)
ntest:		./$(PROGRAM).exe test0.cdl test1.cdl
	./$(PROGRAM) -b test1.cdl
	../ncdump/ncdump test1.nc > test2.cdl
	$(DIFF) test1.cdl test2.cdl
	@echo "*** $(PROGRAM) -b test successful ***"

# test "-c" option of $(PROGRAM)
ctest:		./$(PROGRAM).exe test0.cdl test1.cdl
	./$(PROGRAM) -c -o ctest0.nc test0.cdl > test0.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) test0.c
	$(CC) -o test0 $(LFLAGS) test0.o $(LIBS)
	coff2exe test0
	./test0
	../ncdump/ncdump -n test1 ctest0.nc > ctest1.cdl
	$(DIFF) test1.cdl ctest1.cdl
	@echo "*** $(PROGRAM) -c test successful ***"

# test "-f" option of $(PROGRAM)
ftest:		$(PROGRAM).exe test0.cdl ../fortran/netcdf.inc test1.cdl
	./$(PROGRAM) -f -o ftest0.nc test0.cdl > test0.f
	$(F2C) $(F2CFLAGS) -I../fortran test0.f
	$(CC) -o test0.exe $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) test0.c \
	    $(LIBS) -lf2c -lm
	rm test0.c
	./test0
	../ncdump/ncdump -n test1 ftest0.nc > ftest1.cdl
	$(DIFF) test1.cdl ftest1.cdl
	@echo "*** $(PROGRAM) -f test successful ***" 

test1.cdl:	test0.nc
	../ncdump/ncdump -n test1 test0.nc > test1.cdl

test0.nc:	$(PROGRAM).exe test0.cdl
	$(PROGRAM) -b test0.cdl

load.o:			../libsrc/netcdf.h generic.h $(PROGRAM).h
$(PROGRAM)tab.o:	../libsrc/netcdf.h generic.h $(PROGRAM).h \
			$(PROGRAM)yy.c $(PROGRAM)tab.h
getfill.o:		../libsrc/netcdf.h generic.h
init.o:			../libsrc/netcdf.h $(PROGRAM).h
init.o:			generic.h
close.o:		../libsrc/netcdf.h $(PROGRAM).h
close.o:		generic.h

include ../djgpp32b.mak
