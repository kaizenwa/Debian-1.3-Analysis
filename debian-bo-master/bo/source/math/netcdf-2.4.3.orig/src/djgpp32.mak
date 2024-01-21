# $Id: djgpp32.mak,v 1.10 1996/02/23 20:49:26 steve Exp $
#
#          Makefile to build netcdf library and utilities on MSDOS

include djgpp32a.mak

.SUFFIXES:

all:		xdr/all libsrc/all nctest/all $(FORTRAN_ALL) \
		ncgen/all ncdump/all cxx/all
test:		xdr/test libsrc/test nctest/test $(FORTRAN_TEST) \
		ncgen/test ncdump/test cxx/test
install:	xdr/install libsrc/install $(FORTRAN_INSTALL) \
		ncgen/install ncdump/install cxx/install
clean:		xdr/clean libsrc/clean nctest/clean $(FORTRAN_CLEAN) \
		ncgen/clean ncdump/clean cxx/clean

xdr/all:	FORCE
	cd xdr
	$(MAKE) -w -f djgpp32.mak all
	cd ..
libsrc/all:	FORCE
	cd libsrc
	$(MAKE) -w -f djgpp32.mak all
	cd ..
nctest/all:	FORCE
	cd nctest
	$(MAKE) -w -f djgpp32.mak all
	cd ..
fortran/all:	FORCE
	cd fortran
	$(MAKE) -w -f djgpp32.mak all
	cd ..
ncgen/all:	FORCE
	cd ncgen
	$(MAKE) -w -f djgpp32.mak all
	cd ..
ncdump/all:	FORCE
	cd ncdump
	$(MAKE) -w -f djgpp32.mak all
	cd ..
cxx/all:	FORCE
	cd cxx
	$(MAKE) -w -f djgpp32.mak all
	cd ..

xdr/test:	FORCE
	cd xdr
	$(MAKE) -w -f djgpp32.mak test
	cd ..
libsrc/test:	FORCE
	cd libsrc
	$(MAKE) -w -f djgpp32.mak test
	cd ..
nctest/test:	FORCE
	cd nctest
	$(MAKE) -w -f djgpp32.mak test
	cd ..
fortran/test:	FORCE
	cd fortran
	$(MAKE) -w -f djgpp32.mak test
	cd ..
ncgen/test:	FORCE
	cd ncgen
	$(MAKE) -w -f djgpp32.mak test
	cd ..
ncdump/test:	FORCE
	cd ncdump
	$(MAKE) -w -f djgpp32.mak test
	cd ..
cxx/test:	FORCE
	cd cxx
	$(MAKE) -w -f djgpp32.mak test
	cd ..

xdr/install:	FORCE
	cd xdr
	$(MAKE) -w -f djgpp32.mak install
	cd ..
libsrc/install:	FORCE
	cd libsrc
	$(MAKE) -w -f djgpp32.mak install
	cd ..
fortran/install:	FORCE
	cd fortran
	$(MAKE) -w -f djgpp32.mak install
	cd ..
ncgen/install:	FORCE
	cd ncgen
	$(MAKE) -w -f djgpp32.mak install
	cd ..
ncdump/install:	FORCE
	cd ncdump
	$(MAKE) -w -f djgpp32.mak install
	cd ..
cxx/install:	FORCE
	cd cxx
	$(MAKE) -w -f djgpp32.mak install
	cd ..

xdr/clean:	FORCE
	cd xdr
	$(MAKE) -w -f djgpp32.mak clean
	cd ..
libsrc/clean:	FORCE
	cd libsrc
	$(MAKE) -w -f djgpp32.mak clean
	cd ..
nctest/clean:	FORCE
	cd nctest
	$(MAKE) -w -f djgpp32.mak clean
	cd ..
fortran/clean:	FORCE
	cd fortran
	$(MAKE) -w -f djgpp32.mak clean
	cd ..
ncgen/clean:	FORCE
	cd ncgen
	$(MAKE) -w -f djgpp32.mak clean
	cd ..
ncdump/clean:	FORCE
	cd ncdump
	$(MAKE) -w -f djgpp32.mak clean
	cd ..
cxx/clean:	FORCE
	cd cxx
	$(MAKE) -w -f djgpp32.mak clean
	cd ..

FORCE:
