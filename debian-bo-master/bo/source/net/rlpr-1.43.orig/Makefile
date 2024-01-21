# Generated automatically from Makefile.in by configure.
# $Id: Makefile.in,v 1.2 1996/11/04 05:41:29 meem Exp meem $
#
#

SHELL           = /bin/sh
srcdir 		= .
prefix 		= /usr
exec_prefix     = $(prefix)
bindir          = $(exec_prefix)/bin
mandir          = $(prefix)/man


VERSION	        = 1.43
PACKAGE		= rlpr
distdir		= $(PACKAGE)-$(VERSION)

#
# set up common commands
#
CC              = gcc
INSTALL         = /usr/bin/install
INSTALL_PROGRAM = /usr/bin/install
INSTALL_DATA    = ${INSTALL} -m 644
LN_S            = ln -s

#
# files in the distribution
#

PROGRAMS   	= rlpr rlprd
SOURCES 	= rlpr.c rlpr-net.c rlpr-dbfile.c rlpr-common.c rlprd.c getopt.c getopt1.c 
HEADERS 	= rlpr.h rlpr-net.h rlpr-dbfile.h rlpr-common.h rlprd.h wait.h getopt.h
MANS    	= rlprrc.5 rlprd.8 rlpr.1
DOCS    	= README CHANGES INSTALL
CONFIGS 	= configure Makefile.in config.h.in
MISC    	= rlpr-$(VERSION).lsm
DISTFILES       = $(SOURCES) $(HEADERS) $(MANS) $(DOCS) $(MISC) $(CONFIGS)

CPPFLAGS        = 
CFLAGS 		= -g -O2 
COMPILE         = $(CC) -c -DHAVE_CONFIG_H $(CPPFLAGS) $(CFLAGS)
LDFLAGS         = 
LIBS            = -lbsd 
LINK            = $(CC) $(LDFLAGS) -o $@

rlpr_OBJECTS    = rlpr.o rlpr-net.o rlpr-dbfile.o rlpr-common.o getopt.o getopt1.o 
rlprd_OBJECTS   = rlprd.o rlpr-common.o getopt.o getopt1.o

.c.o:
	$(COMPILE) $<

all: $(PROGRAMS)

rlpr:  $(rlpr_OBJECTS) $(rlpr_DEPENDENCIES)
	$(LINK) $(rlpr_OBJECTS) $(LIBS)

rlprd: $(rlprd_OBJECTS) $(rlprd_DEPENDENCIES)
	$(LINK) $(rlprd_OBJECTS) $(LIBS)

clean: 
	-rm -f *.o *~ $(PROGRAMS) core

distclean: clean
	-rm -f Makefile config.status config.cache config.log stamp-h config.h

lpr: rlpr install
	@if [ ! `whoami` = "root" ]; then \
	 echo "for rlpr to be installed as lpr, you must be root.."; \
	 exit 1; \
	fi
	@bsdlpr=`which lpr` || { \
	 echo "lpr cannot be found on your system.. aborting.."; \
	 exit 1; \
	}; \
	echo "saving $$bsdlpr as $${bsdlpr}.bsd"; \
	mv $$bsdlpr $${bsdlpr}.bsd; \
	echo "symlinking $$bsdlpr to rlpr"; \
	$(LN) $(bindir)/rlpr $$bsdlpr

install: all
	@if [ ! -d $(bindir) ]; then \
	 echo "install directory $(bindir) isn't a directory, aborting.."; \
	 exit 1; \
	fi
	@for f in $(PROGRAMS); do \
	 echo "installing $$f to $(bindir) setuid root..."; \
	  $(INSTALL) -m 4755 -o root -g root -s $$f $(bindir) 2> /dev/null || { \
	  echo "*** unable to install $$f root"; \
	  if [ $$f = "rlpr" ]; then \
	    echo "*** please be sure to read documentation about using rlprd as a proxy"; \
	  elif [ $$f = "rlprd" ]; then \
	    echo "*** to be of use, rlprd must be installed setuid root. if you need it"; \
	    echo -e "*** for your configuration, please rerun \"make install\" as root"; \
	  fi; \
	  echo "installing $$f to $(bindir) non-setuid root..."; \
	  $(INSTALL_PROGRAM) -s $$f $(bindir); \
	}; \
	done
	@echo; \
	if [ ! -d $(mandir) ]; then \
	 echo "manpage directory $(mandir) isn't a directory, not installing manpages.."; \
	else for f in $(MANS); do \
	  ext=`echo $$f | cut -d. -f2`; \
	  echo "installing $$f to $(mandir)/man$${ext}"; \
	   $(INSTALL) -d $(mandir)/man$${ext}; \
	   $(INSTALL_DATA) $$f $(mandir)/man$${ext}; \
	done \
	fi 
	@echo; \
	echo "installation successful. please see README for details on getting started"

uninstall:
	-rm -f $(bindir)/rlpr
	-rm -f $(bindir)/rlprd
	-rm -f $(mandir)/man1/rlpr.1
	-rm -f $(mandir)/man5/rlprrc.5
	-rm -f $(mandir)/man8/rlprd.8

${srcdir}/configure: configure.in aclocal.m4
	cd ${srcdir} && autoconf

${srcdir}/config.h.in: stamp-h.in
${srcdir}/stamp-h.in: configure.in 
	cd ${srcdir} && autoheader
	echo timestamp > ${srcdir}/stamp-h.in
config.h: stamp-h
stamp-h: config.h.in config.status
	./config.status

Makefile: Makefile.in config.status
	./config.status

config.status: configure
	./config.status --recheck

TAGS:
	etags ${srcdir}/*.[ch]

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
