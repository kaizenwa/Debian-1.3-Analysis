#  Copyright 1993, UCAR/Unidata
#
#  OS/2 makefile for ncgen

include ../emx1.mak

PROGRAM		= ncgen

INCLUDES	= -I../libsrc

LIBS		= ../libsrc/netcdf.lib ../xdr/xdr.lib

OBJS		= main.obj $(PROGRAM)tab.obj load.obj escapes.obj \
		  getfill.obj init.obj close.obj genlib.obj


all:		$(OBJS) $(PROGRAM).exe

test:   	$(PROGRAM).exe test0.cdl ntest ctest $(FTEST) FORCE

install:	$(BINDIR)/$(PROGRAM).exe

clean::
	rm -f $(PROGRAM)tab.c $(PROGRAM)tab.h $(PROGRAM)yy.c netcdf.inc \
	    test0 test0.nc test0.for test0.exe test1.nc test1.cdl test2.cdl \
		ftest0.nc ftest1.cdl test0.c ctest0.nc ctest1.cdl


# test "-b" option of $(PROGRAM)
ntest:		$(PROGRAM).exe test0.cdl test1.cdl
	$(PROGRAM) -b test1.cdl
	..\ncdump\ncdump test1.nc > test2.cdl
	$(DIFF) test1.cdl test2.cdl
	@echo "*** $(PROGRAM) -b test successful ***"

# test "-c" option of $(PROGRAM)
ctest:		$(PROGRAM).exe test0.cdl test1.cdl
	$(PROGRAM) -c -o ctest0.nc test0.cdl > test0.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) test0.c
	$(CC) -o test0.exe $(CFLAGS) $(LDFLAGS) test0.obj $(LIBS)
	test0
	..\ncdump\ncdump -n test1 ctest0.nc > ctest1.cdl
	$(DIFF) test1.cdl ctest1.cdl
	@echo "*** $(PROGRAM) -c test successful ***"

# test "-f" option of $(PROGRAM)
ftest:		$(PROGRAM).exe test0.cdl netcdf.inc test1.cdl
	$(PROGRAM) -f -o ftest0.nc test0.cdl > test0.f
	$(F2C) $(F2CFLAGS) test0.f
	$(CC) -o test0.exe $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) test0.c \
	    $(LIBS) $(F2CLIBS)
	rm test0.c
	test0
	..\ncdump\ncdump -n test1 ftest0.nc > ftest1.cdl
	$(DIFF) test1.cdl ftest1.cdl
	@echo "*** $(PROGRAM) -f test successful ***" 

netcdf.inc:	../fortran/netcdf.inc
	$(CP) ..\fortran\netcdf.inc $@

test1.cdl:	test0.nc
	..\ncdump\ncdump -n test1 test0.nc > test1.cdl

test0.nc:	$(PROGRAM).exe test0.cdl
	$(PROGRAM) -b test0.cdl

$(PROGRAM).exe:	../libsrc/netcdf.lib ../xdr/xdr.lib

load.obj:		../libsrc/netcdf.h generic.h $(PROGRAM).h load.c
$(PROGRAM)tab.obj:	../libsrc/netcdf.h generic.h $(PROGRAM).h \
				$(PROGRAM)yy.c $(PROGRAM)tab.h
getfill.obj:		../libsrc/netcdf.h generic.h getfill.c
init.obj:		../libsrc/netcdf.h $(PROGRAM).h
init.obj:		generic.h init.c
close.obj:		../libsrc/netcdf.h $(PROGRAM).h generic.h close.c
main.obj:		main.c
escapes.obj:		escapes.c
genlib.obj:		genlib.c

include ../emx2.mak
