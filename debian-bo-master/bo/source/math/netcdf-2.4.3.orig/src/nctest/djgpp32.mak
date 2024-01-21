#  Copyright 1993, 1996 UCAR/Unidata
#
#  DOS makefile for exhaustive test of netCDF package.

include ../djgpp32a.mak

PROGRAM		= nctest

INCLUDES	= -I../libsrc

LIBS		= ../libsrc/libnc.a ../xdr/libxdr.a

OBJS		=  driver.o \
		   varget.o vargetg.o varput.o varputg.o vardef.o vartests.o \
		   vputget.o vputgetg.o cdftests.o dimtests.o rec.o \
		   atttests.o misctest.o add.o error.o emalloc.o val.o slabs.o

all:		$(PROGRAM).exe

test:		all FORCE
	./$(PROGRAM)

install:

clean::
	rm -f *.cdf

add.o: add.c
add.o: ../libsrc/netcdf.h
add.o: ./testcdf.h
add.o: ./add.h
add.o: ./emalloc.h
atttests.o: atttests.c
atttests.o: ../libsrc/netcdf.h
atttests.o: ./testcdf.h
atttests.o: ./add.h
atttests.o: ./error.h
atttests.o: ./emalloc.h
atttests.o: ./tests.h
atttests.o: ./val.h
bug.o: bug.c
bug.o: ../libsrc/netcdf.h
cdftests.o: cdftests.c
cdftests.o: ../libsrc/netcdf.h
cdftests.o: ./testcdf.h
cdftests.o: ./add.h
cdftests.o: ./error.h
cdftests.o: ./tests.h
dimtests.o: dimtests.c
dimtests.o: ../libsrc/netcdf.h
dimtests.o: ./testcdf.h
dimtests.o: ./add.h
dimtests.o: ./error.h
dimtests.o: ./tests.h
driver.o: driver.c
driver.o: ../libsrc/netcdf.h
driver.o: ./tests.h
emalloc.o: emalloc.c
emalloc.o: ./error.h
emalloc.o: ./emalloc.h
error.o: error.c
error.o: ../libsrc/netcdf.h
error.o: ./error.h
fixed1.o: fixed1.c
fixed1.o: ../libsrc/netcdf.h
fixed2.o: fixed2.c
fixed2.o: ../libsrc/netcdf.h
misctest.o: misctest.c
misctest.o: ../libsrc/netcdf.h
misctest.o: ./testcdf.h
misctest.o: ./add.h
misctest.o: ./error.h
nctime.o: nctime.c
nctime.o: ../libsrc/netcdf.h
nctime0.o: nctime0.c
nctime0.o: ../libsrc/netcdf.h
rec.o: rec.c
rec.o: ../libsrc/netcdf.h
rec.o: ./testcdf.h
rec.o: ./val.h
rec.o: ./error.h
rec.o: ./tests.h
slabs.o: slabs.c
slabs.o: ../libsrc/netcdf.h
slabs.o: ./testcdf.h
slabs.o: ./add.h
slabs.o: ./error.h
slabs.o: ./tests.h
val.o: val.c
val.o: ../libsrc/netcdf.h
val.o: ./testcdf.h
val.o: ./val.h
val.o: ./error.h
vardef.o: vardef.c
vardef.o: ../libsrc/netcdf.h
vardef.o: ./testcdf.h
vardef.o: ./add.h
vardef.o: ./error.h
vardef.o: ./tests.h
varget.o: varget.c
varget.o: ../libsrc/netcdf.h
varget.o: ./testcdf.h
varget.o: ./error.h
varget.o: ./tests.h
vargetg.o: vargetg.c
vargetg.o: ../libsrc/netcdf.h
vargetg.o: ./testcdf.h
vargetg.o: ./error.h
vargetg.o: ./tests.h
varput.o: varput.c
varput.o: ../libsrc/netcdf.h
varput.o: ./testcdf.h
varput.o: ./val.h
varput.o: ./error.h
varput.o: ./tests.h
varputg.o: varputg.c
varputg.o: ../libsrc/netcdf.h
varputg.o: ./testcdf.h
varputg.o: ./val.h
varputg.o: ./error.h
varputg.o: ./tests.h
vartests.o: vartests.c
vartests.o: ../libsrc/netcdf.h
vartests.o: ./testcdf.h
vartests.o: ./add.h
vartests.o: ./error.h
vartests.o: ./tests.h
vputget.o: vputget.c
vputget.o: ../libsrc/netcdf.h
vputget.o: ./testcdf.h
vputget.o: ./add.h
vputget.o: ./val.h
vputget.o: ./error.h
vputget.o: ./tests.h
vputget.o: ./emalloc.h
vputgetg.o: vputgetg.c
vputgetg.o: ../libsrc/netcdf.h
vputgetg.o: ./testcdf.h
vputgetg.o: ./add.h
vputgetg.o: ./val.h
vputgetg.o: ./error.h
vputgetg.o: ./tests.h
vputgetg.o: ./emalloc.h

include ../djgpp32b.mak
