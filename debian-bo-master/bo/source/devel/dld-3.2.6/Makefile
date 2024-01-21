ARCHIVE = libdld.a
ARCHIVE++ = libdld++.a
LIBS = dld.o find_exec.o define.o get_func.o get_symbol.o \
	list_undef.o mk_dummy.o ref.o ul_file.o ul_symbol.o \
	remove.o error.o
INCLUDES = dld.h defs.h

prefix = /usr/local/
exec_prefix = $(prefix)/
libdir = $(prefix)lib/
infodir = $(prefix)info/
includedir = $(prefix)include/

CC= gcc
CFLAGS = -O6 -I.
CXXFLAGS = -O6 -I.

all:	${ARCHIVE}
all++:	${ARCHIVE++}

install:	$(ARCHIVE) dld.info
	-cp $(ARCHIVE) $(libdir)
	-cp dld.info $(infodir) 
                                # makeinfo called so as to make dld.info there
                                # not in debian it isn't! GRT
	-cp dld.h $(includedir)

install++:	$(ARCHIVE++) install
	-cp $(ARCHIVE++) $(libdir)

prefix = /usr/local/
infodir = $(prefix)info/

info:	$(infodir)/dld.info
$(infodir)/dld.info:	dld.texinfo
	makeinfo dld.texinfo
                      # makeinfo will build things in a polite way GRT

srcdir=$(HOME)/dld/
dvidir=../dvi/
dvi:	$(dvidir)dld.dvi
$(dvidir)dld.dvi:	$(srcdir)dld.texinfo $(dvidir)dld.fn
#	cd $(dvidir);texi2dvi $(srcdir)dld.texinfo
	-(cd $(dvidir);texindex dld.??)
	cd $(dvidir);tex $(srcdir)dld.texinfo
$(dvidir)dld.fn:
	cd $(dvidir);tex $(srcdir)dld.texinfo
xdvi:	$(dvidir)dld.dvi
	xdvi $(dvidir)dld.dvi

$(ARCHIVE): ${LIBS} ${INCLUDES}
	rm -f $(ARCHIVE)
	ar q $(ARCHIVE) ${LIBS}
	ranlib $(ARCHIVE)

$(ARCHIVE++): ${LIBS} gxxload.o ${INCLUDES}
	rm -f $(ARCHIVE++)
	ar q $(ARCHIVE++) ${LIBS} gxxload.o
	ranlib $(ARCHIVE++)

clean:
	-rm -f ${ARCHIVE} *.o *~ *.rej *.info  
                                               # get rid of the info file too
                                               # GRT
	cd test; make clean

check:	all
	cd test; make all

check++:	all
	cd test; make all++

.c.o:
	${CC} -c ${CFLAGS} $*.c

cfiles= define.c defs.h dld.c dld.h error.c find_exec.c get_func.c \
	get_symbol.c list_undef.c mk_dummy.c ref.c remove.c \
	ul_file.c ul_symbol.c gxxload.cc

TAGS:
tags:	$(cfiles)
	etags $(cfiles)

#			  DISTRIBUTION STUFF

allfiles = README dld-3.2.6.lsm COPYING ChangeLog TODO Makefile $(cfiles)

tfiles = EXPECTED-OUTPUT Makefile SAMPLE_INPUT add1.c call_add1.c \
	add2.c call_add2.c chain1.c chain2.c chain3.c get-sym.c \
	hello.c list-undefined.c main.c need.c overlay.c print_arg.c \
	print_global.c read-a.out.c reload-test.c reload-test.s \
	reload.c remove.c simple.c test-define.c gxxtest.cc sub.cc

dist:	dld.tar.gz # dld-bin.tar.gz
	mv -f dld.tar.*z $(HOME)/dist/

.cc.o:
	${CXX} -c ${CXXFLAGS} $*.cc
	# mv -f dld-bin.tar.*z $(HOME)/dist/
	cp -f dld*.lsm $(HOME)/dist/
tar.gz:	dld.tar.gz
dld.tar.gz:	dld.tar
	-rm -f dld.tar.*z
	gzip dld.tar
	chmod 664 dld.tar.*z
tar.Z:	dld.tar.Z
dld.tar.Z:	dld.tar
	-rm -f dld.tar.Z
	compress dld.tar
	chmod 664 dld.tar.Z
shar.Z:	dld.shar.Z
dld.shar.Z:	dld.shar
	-rm -f dld.shar.Z
	compress dld.shar
	chmod 664 dld.shar.Z

tar:	dld.tar
dld.tar:	temp/dld
	cd temp; tar cohf ../dld.tar dld
	chmod 664 dld.tar
shar:	dld.shar
dld.shar:	temp/dld
	cd temp; shar dld >../dld.shar
	chmod 664 dld.shar
dclshar:	dld.com
com:	dld.com
dld.com:	temp/dld
	cd temp; dclshar dld >../dld.com
	chmod 664 dld.com
zip:	dld.zip
dld.zip:	temp/dld
	cd temp; zip -ru ../dld.zip dld
	chmod 664 dld.zip
pubzip:	temp/dld
	cd temp; zip -ru $(HOME)/pub/dld.zip dld
	chmod 664 $(HOME)/pub/dld.zip

dld-bin.tar.gz:	dld-bin.tar
	-rm -f dld-bin.tar.*z
	gzip dld-bin.tar
	chmod 664 dld-bin.tar.*z
dld-bin.tar:	 $(libdir)libdld.a $(infodir)dld.info $(includedir)dld.h
	cd $(prefix); tar cohf dld-bin.tar include/dld.h \
		lib/libdld.a info/dld.info src/dld/COPYING src/dld/README
	cd $(prefix); chmod 664 dld-bin.tar
	mv $(prefix)/dld-bin.tar ./

temp/dld:	$(allfiles)
	-rm -rf temp
	mkdir temp
	mkdir temp/dld
	ln $(allfiles) dld.texinfo temp/dld
	mkdir temp/dld/test
	cd test; ln $(tfiles) ../temp/dld/test

diffs:	pubdiffs
pubdiffs:	temp/dld
	mv temp/dld temp/ndld
	cd temp;unzip $(HOME)/pub/dld.zip
	-rm -f dld.pat
	-diff -rc temp/dld temp/ndld > dld.pat
	-rm -rf temp
	ls -l dld.pat

distdiffs:	temp/dld
	mv temp/dld temp/ndld
	cd temp;zcat $(HOME)/dist/dld-*.src.tar.gz | tar xvf -
	-rm -f dld.pat
	-diff -rc temp/dld temp/ndld > dld.pat
	-rm -rf temp
	ls -l dld.pat
