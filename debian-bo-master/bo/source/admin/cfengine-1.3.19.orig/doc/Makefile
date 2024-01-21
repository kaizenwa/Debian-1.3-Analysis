# Generated automatically from Makefile.in by configure.
#
# Makefile for CFEngine's documentation
#
# This file is intended as a guide. You may need to modify it a little on your
# system.  Note that you need the GNU texinfo package to run tex on this
# documentation!!  A postscript version is included since some people have had
# problems here.
#

version = cfengine-1.3.19


DOCS = cfengine.info   cfengine.html

prefix = /usr/local
infodir = ${prefix}/info

INSTALL = /local/gnu/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644

MKINSTALLDIR = ../bin/mkinstalldirs

MAKEINFO = /local/gnu/bin/makeinfo
TEX = 
TEXINDEX = /local/gnu/bin/texindex
DVIPS = 
TEXI2HTML = /local/bin/texi2html

CP = ln
RM = rm -f

# Needs to agree with VPATH
MAKEINFOFLAGS = -I.

.SUFFIXES: .ps .info .html .dvi .texinfo

all: ${DOCS}

install: cfengine.info
	${MKINSTALLDIR} ${infodir}
	for x in cfengine.info* ; \
	do \
	${INSTALL_DATA} $$x ${infodir}; \
	done

dist: ${version}.ps

${version}.ps: cfengine.dvi
	${DVIPS} -f cfengine.dvi > $@

cfengine.dvi: version.texi

cfengine.html: version.texi

cfengine.info: version.texi

clean:
	${RM} cfengine.info* cfengine.aux cfengine.log cfengine.cp
	${RM} cfengine.pg cfengine.tp cfengine.vr cfengine.fn
	${RM} cfengine.ky cfengine.toc cfengine.cps cfengine.vrs
	${RM} cfengine.fns  cfengine.mb cfengine.mbs  
	${RM} cfengine.f cfengine.fs *~

distclean: clean
	${RM} cfengine.dvi cfengine.ps

.info.html:
	-${TEXI2HTML} $<

.texinfo.info:
	-${MAKEINFO} ${MAKEINFOFLAGS} $<

.texinfo.dvi:
	-${TEX} $<
	-${TEXINDEX} $*.vr $*.mb $*.cp
	echo rescanning || ${TEX} $<
