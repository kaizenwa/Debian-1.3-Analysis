# Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996 Free Software Foundation, Inc.
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this software; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
#
# As a special exception, the Free Software Foundation gives permission
# for additional uses of the text contained in its release of GUILE.
#
# The exception is that, if you link the GUILE library with other files
# to produce an executable, this does not by itself cause the
# resulting executable to be covered by the GNU General Public License.
# Your use of that executable is in no way restricted on account of
# linking the GUILE library code into it.
#
# This exception does not however invalidate any other reasons why
# the executable file might be covered by the GNU General Public License.
#
# This exception applies only to the code released by the
# Free Software Foundation under the name GUILE.  If you copy
# code from other Free Software Foundation releases into a copy of
# GUILE, as the General Public License permits, the exception does
# not apply to the code that you add in this way.  To avoid misleading
# anyone as to the status of such modified files, you must delete
# this exception notice from them.
#
# If you write modifications of your own for GUILE, it is your choice
# whether to permit this exception to apply to your modifications.
# If you do not wish that, delete this exception notice.  

# "Makefile" for scm4e6 Scheme Interpreter
# Author: Aubrey Jaffer

SHELL = /bin/sh
#CC =
CFLAGS = #-g
#LIBS =
LD = $(CC) #-g

# directory where COPYING and Init.scm reside.
#IMPLPATH = /usr/local/src/scm/
#this one is good for bootstrapping
IMPLPATH = `pwd`/
# Pathname where Init.scm resides.
IMPLINIT = $(IMPLPATH)Init.scm
DFLAG = -DIMPLINIT=\"$(IMPLINIT)\"

# If pathname where Init.scm resides is not known in advance then
# SCM_INIT_PATH is the environment variable whose value is the
# pathname where Init.scm resides.

intro:
	@echo
	@echo "This is scm$(VERSION).  Read \"scm.info\" (from \"scm.texi\")"
	@echo "to learn how to build and install SCM."
	@echo "Here is a quick guide:"
	@echo
	@echo "  From: bos@scrg.cs.tcd.ie"
	@echo "  Build and install scripts using GNU autoconf are"
	@echo "  available as scmconfig.tar.gz in the SCM distribution"
	@echo "  directories.  See README.unix in scmconfig.tar.gz for"
	@echo "  further instructions."
	@echo
	@echo "  Alternatively:"
	@echo "      make scmlit"
	@echo
	@echo "  If you are on a non-unix system, create an empty file"
	@echo "  \"scmflags.h\".  Then compile time.c, repl.c, scl.c,"
	@echo "  sys.c, eval.c, subr.c, unif.c, and rope.c.  Then link"
	@echo "  them to create a \"scmlit\" executable."
	@echo
	@echo "  Once you have built scmlit successfully, test it:"
	@echo "      make checklit"
	@echo "  If this reports no errors, use scmlit to build.scm"
	@echo "  fancier versions of scm, with optional features."

ofiles = time.o repl.o scl.o sys.o eval.o subr.o unif.o rope.o \
	continue.o findexec.o
# ramap.o

all:	scmlit
scmlit:	$(ofiles) scm.o
	$(LD) -o scmlit $(ofiles) scm.o $(LIBS)
scm.o:	scm.c scm.h scmfig.h patchlvl.h scmflags.h
	$(CC) $(CFLAGS) -c $(DFLAG) scm.c
scmflags.h:	scmflags
scmflags:
	echo "#ifndef IMPLINIT" > scmflags.h
	echo "#define IMPLINIT \"$(IMPLINIT)\"" >> scmflags.h
	echo "#endif" >> scmflags.h

.c.o:
	$(CC) -c $(CFLAGS) $< -o $@
scl.o:	scl.c scm.h scmfig.h scmflags.h
eval.o:	eval.c scm.h scmfig.h scmflags.h setjump.h
unif.o:	unif.c scm.h scmfig.h scmflags.h
#ramap.o:	ramap.c scm.h scmfig.h scmflags.h
repl.o:	repl.c scm.h scmfig.h scmflags.h setjump.h
sys.o:	sys.c scm.h scmfig.h scmflags.h setjump.h
time.o:	time.c scm.h scmfig.h scmflags.h
subr.o:	subr.c scm.h scmfig.h scmflags.h
rope.o:	rope.c scm.h scmfig.h scmflags.h
continue.o:	continue.c continue.h setjump.h
	$(CC) $(CFLAGS) -c continue.c

srcdir=$(HOME)/scm/

udscm:
	$(srcdir)build.scm -hsystem -Fcautious bignums arrays inexact \
	engineering-notation dump dynamic-linking -o udscm

myscm:	udscm $(ifiles)
	./udscm -o scm
mylib:
	$(srcdir)build.scm -hsystem -Fcautious bignums arrays inexact \
	engineering-notation dump dynamic-linking -tlib
pgscm:
	$(srcdir)build.scm -hsystem -Fcautious bignums arrays inexact \
	engineering-notation dump dynamic-linking -o udscm \
	 --compiler-options=-pg --linker-options=-pg
	./udscm -o pgscm
mydebug:
	$(srcdir)build.scm -hsystem -Fcautious bignums arrays inexact \
	engineering-notation dump dynamic-linking debug -ogdbscm \
	 --compiler-options=-Wall --linker-options=-Wall
mydlls:
	$(srcdir)build.scm -h system -t dll -c sc2.c rgx.c crs.c edline.c \
		record.c gsubr.c ioext.c posix.c unix.c socket.c \
		ramap.c
myturtle:
	$(srcdir)build.scm -h system -F turtlegr -t dll

checklit:	r4rstest.scm
	./scmlit -fr4rstest.scm -e'(test-sc4)(test-cont)(test-delay)(gc)'
check:	r4rstest.scm
	./scm -fr4rstest.scm -e'(test-sc4)(test-cont)(test-delay)(gc)'
bench:
	echo `whoami`@`hostname` testing scm >> BenchLog
	echo  on `date` >> BenchLog
	ls -l scm >> BenchLog
	size scm >> BenchLog
	uname -a >> BenchLog
	./scm -lbench.scm
	cat bench.log >> BenchLog
	echo >> BenchLog
	echo
	tail -20 BenchLog
benchlit:
	echo `whoami`@`hostname` testing scmlit >> BenchLog
	echo  on `date` >> BenchLog
	ls -l scmlit >> BenchLog
	size scmlit >> BenchLog
	uname -a >> BenchLog
	./scmlit -lbench.scm
	cat bench.log >> BenchLog
	echo >> BenchLog
	echo
	tail -20 BenchLog

dvidir=../dvi/
dvi:	$(dvidir)scm.dvi
$(dvidir)scm.dvi:	$(srcdir)scm.texi $(dvidir)scm.fn Makefile
#	cd $(dvidir);texi2dvi $(srcdir)scm.texi
	-(cd $(dvidir);export set TEXINPUTS=$(srcdir):$$TEXINPUTS;texindex scm.??)
	cd $(dvidir);export set TEXINPUTS=$(srcdir):$$TEXINPUTS;tex $(srcdir)scm.texi
$(dvidir)scm.fn:
	cd $(dvidir);tex $(srcdir)scm.texi
xdvi:	$(dvidir)scm.dvi
	xdvi -s 3 $(dvidir)scm.dvi
htmldir=../public_html/
html:	$(htmldir)scm_toc.html
$(htmldir)scm_toc.html:	$(srcdir)scm.texi
	cd $(htmldir);make scm_toc.html

################ INSTALL DEFINITIONS ################

prefix = /usr/local/
exec_prefix = $(prefix)/
# directory where `make install' will put executable.
bindir = $(exec_prefix)bin/
libdir = $(exec_prefix)lib/
# directory where `make install' will put manual page.
man1dir = $(prefix)man/man1/
infodir = $(prefix)info/
includedir = $(prefix)include/

info:	$(infodir)/scm.info
$(infodir)/scm.info:	scm.texi
	makeinfo scm.texi -o $(infodir)/scm.info

infoz:	$(infodir)/scm.info.gz
$(infodir)/scm.info.gz:	$(infodir)/scm.info
	-rm $(infodir)/scm.info*.gz
	gzip $(infodir)/scm.info*

install:	scm.1
	test -d $(bindir) || mkdir $(bindir)
	test -d $(man1dir) || mkdir $(man1dir)
	-cp scm $(bindir)
	-strip $(bindir)scm
	-cp scm.1 $(man1dir)
	test -d $(IMPLPATH) || mkdir $(IMPLPATH)
	-cp Init.scm $(IMPLPATH)
	-cp Link.scm $(IMPLPATH)
	-cp Transcen.scm $(IMPLPATH)
	-cp COPYING $(IMPLPATH)

installlib:
	test -d $(includedir) || mkdir $(includedir)
	cp scm.h $(includedir)scm.h
	cp scmfig.h $(includedir)scmfig.h
	test -d $(libdir) || mkdir $(libdir)
	cp libscm.a $(libdir)libscm.a

uninstall:
	-rm $(bindir)scm
	-rm $(man1dir)scm.1
	-rm $(includedir)scm.h
	-rm $(includedir)scmfig.h
	-rm $(libdir)libscm.a
#	-rm $(IMPLPATH)Init.scm
#	-cp $(IMPLPATH)Link.scm
#	-rm $(IMPLPATH)Transcen.scm
#	-rm $(IMPLPATH)COPYING

scm.doc:	scm.1
	nroff -man $< | ul -tunknown >$@

#### Stuff for maintaining SCM below ####

VERSION = 4e6
ver = $(VERSION)
RM_R = rm -rf
cfiles = scm.c time.c repl.c ioext.c scl.c sys.c eval.c subr.c sc2.c \
	unif.c rgx.c crs.c dynl.c record.c posix.c socket.c unix.c \
	rope.c ramap.c gsubr.c edline.c Iedline.scm continue.c \
	findexec.c
ufiles = pre-crt0.c ecrt0.c gmalloc.c unexec.c unexelf.c
confiles = scmconfig.h.in mkinstalldirs acconfig-1.5.h install-sh \
	configure configure.in Makefile.in COPYING README.unix

hfiles = scm.h scmfig.h setjump.h patchlvl.h continue.h
ifiles = Init.scm Transcen.scm Link.scm
tfiles = r4rstest.scm example.scm pi.scm pi.c split.scm bench.scm
dfiles = ANNOUNCE README COPYING scm.1 scm.doc QUICKREF \
	scm.texi ChangeLog 
mfiles = Makefile build.scm build.bat
vfiles = setjump.mar setjump.s
afiles = $(dfiles) $(cfiles) $(hfiles) $(ifiles) $(tfiles) $(mfiles) \
	$(vfiles) $(ufiles)

makedev = make -f $(HOME)/makefile.dev
CHPAT=$(HOME)/bin/chpat
dest = $(HOME)/dist/
temp/scm:	$(afiles)
	-$(RM_R) temp
	mkdir temp
	mkdir temp/scm
	ln $(afiles) temp/scm

dist:	$(dest)scm$(VERSION).tar.gz
$(dest)scm$(VERSION).tar.gz:	temp/scm
	$(makedev) DEST=$(dest) PROD=scm ver=$(VERSION) tar.gz
shar:	scm.shar
scm.shar:	temp/scm
	$(makedev) PROD=scm shar
dclshar:	scm.com
com:	scm.com
scm.com:	temp/scm
	$(makedev) PROD=scm com
zip:	scm.zip
scm.zip:	temp/scm
	$(makedev) PROD=scm zip
distzip:	scm$(VERSION).zip
scm$(VERSION).zip:	temp/scm turtle turtlegr.c grtest.scm require.scm
	$(makedev) DEST=$(dest) PROD=scm ver=$(VERSION) zip
	cd ..; zip -9ur $(dest)scm$(VERSION).zip \
		scm/turtle scm/turtlegr.c scm/grtest.scm scm/require.scm
	mv $(dest)scm$(VERSION).zip /c/scm/dist/
pubzip:	temp/scm
	$(makedev) DEST=$(HOME)/pub/ PROD=scm zip

diffs:	pubdiffs
pubdiffs:	temp/scm
	$(makedev) DEST=$(HOME)/pub/ PROD=scm pubdiffs
distdiffs:	temp/scm
	$(makedev) DEST=$(dest) PROD=scm ver=$(ver) distdiffs


HOBBITVERSION = 4d
hobfiles = README.hob hobbit.doc hobbit.tms hobbit.scm scmhob.h

hobtemp/scm:	$(hobfiles)
	-$(RM_R) hobtemp
	mkdir hobtemp
	mkdir hobtemp/scm
	ln $(hobfiles) hobtemp/scm

hobdist:	$(dest)hobbit$(HOBBITVERSION).tar.gz
$(dest)hobbit$(HOBBITVERSION).tar.gz:	hobtemp/scm
	$(makedev) DEST=$(dest) PROD=scm ver=-hob$(HOBBITVERSION) \
		tar.gz TEMP=hobtemp/
	mv $(dest)scm-hob$(HOBBITVERSION).tar.gz \
		$(dest)hobbit$(HOBBITVERSION).tar.gz
hobbit$(HOBBITVERSION).zip:	hobtemp/scm
	$(makedev) TEMP=hobtemp/ name=hobbit$(HOBBITVERSION) PROD=scm zip

new:
	$(CHPAT) scm$(VERSION) scm$(ver) ANNOUNCE ../jacal/ANNOUNCE \
		../wb/README ../wb/ANNOUNCE \
		../public_html/README.html ../dist/README \
		../public_html/SLIB.html ../public_html/JACAL.html \
		../public_html/SCM.html ../public_html/Hobbit.html \
		/c/scm/dist/install.bat /c/scm/dist/makefile \
		/c/scm/dist/mkdisk.bat
	$(CHPAT) $(VERSION) $(ver) README scm.texi patchlvl.h \
		Init.scm ../public_html/SCM.html Makefile

configtemp/scm:	$(confiles)
	-$(RM_R) configtemp/scm
	-mkdir configtemp
	mkdir configtemp/scm
	ln $(confiles) configtemp/scm
confdist:	scmconfig.tar.gz
scmconfig.tar.gz:	configtemp/scm
	cd configtemp; tar cohf ../scmconfig.tar scm
	chmod 664 scmconfig.tar
	-rm -f scmconfig.tar.*z
	gzip scmconfig.tar
	chmod 664 scmconfig.tar.*z

lint:	lints
lints:	$(cfiles) $(hfiles)
	lint $(CPPFLAGS) $(ALL_CFLAGS) $(cfiles) | tee lints
#	lint $(CPPFLAGS) $(ALL_CFLAGS) $(cfiles) | tee lintes

#seds to help find names not unique in first 8 characters (name8s)
# for BSD nm format
SED_TO_STRIP_NM=sed -e '/.*\.o$$/d' -e 's/.* _//' -e 's/.* T //'
#old, bad for T [^_] on suns: SED_TO_STRIP_NM=sed -e '/.*\.o$$/d' -e 's/.* _//'
# For a System V nm where plain C identifiers have _ prepended:
#SED_TO_STRIP_NM=sed -e '/^_[A-Za-z][A-za-z0-9_]*[ 	|].*|extern|!d' -e 's/|/ /g' -e 's/^_//'
# For a System V nm where plain C identifiers have nothing prepended:
#SED_TO_STRIP_NM=sed -e '/^[A-Za-z][A-za-z0-9_]*[ 	|].*|extern|!d' -e 's/|/ /g'

name8:	name8s
name8s:	scm
	nm scm |\
	$(SED_TO_STRIP_NM) |\
	sort -u|\
	awk '{	if (substr(l,1,8)==substr($$1,1,8)) {\
			if (p) print l;\
			print $$1;p=0;stat=1\
		}else p=1;\
		l=$$1\
	     }END{exit stat}' -
ctags:	$(hfiles) $(cfiles)
	etags $(hfiles) $(cfiles)
TAGS:
tags:	$(hfiles) $(cfiles) $(ifiles) $(vfiles) $(ufiles)\
	hobbit.scm scm.texi README build.scm # $(mfiles) ChangeLog
	etags $(hfiles) $(cfiles) $(ifiles) $(vfiles) $(ufiles)\
	hobbit.scm scm.texi README build.scm # $(mfiles) ChangeLog
mostlyclean:
clean:
	-rm -f *~ *.bak *.orig *.rej core a.out ramap.o $(ofiles) scm.o \
	lints tmp* \#* *\#
	-$(RM_R) *temp
distclean:	clean
	-rm -f $(EXECFILES) *.o a.out TAGS
realclean:	distclean
	-rm -f scm.doc
realempty:	temp/scm
	-rm -f $(afiles)
