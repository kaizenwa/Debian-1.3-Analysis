# ===========================================================================
#
#                          Makefile for TkDesk 1.0b4
#
# ===========================================================================

#
# Please adjust the setting of each variable to suit your platform and
# installation.  Please take extra care not to mix header files and
# libraries of different versions of Tcl/Tk.
#

# --------------------------------
# TkDesk Installation:
# --------------------------------

#
# Where the executables will be installed:

BINDIR = /usr/local/bin
#BINDIR = $(HOME)/bin

# Where TkDesk will look for the tkdesk script when started (this is
# usually the same as BINDIR):

SEARCH_BINDIR = $(BINDIR)

#
# TkDesk's library will be installed here:

LIBDIR = /usr/local/lib/TkDesk
#LIBDIR = $(HOME)/lib/TkDesk

# Where TkDesk will look for its library files when started (this is
# usually the same as LIBDIR):

SEARCH_LIBDIR = $(LIBDIR)

# --------------------------------
# Tcl/Tk Paths and Libraries:
# You need either Tcl/Tk 7.4/4.0, 7.5/4.1, or 7.6/4.2!
# --------------------------------

#
# Location of the Tcl include files:

#TCLINCDIR = /usr/include/tcl       # Linux with Slackware 3.0
TCLINCDIR = /usr/local/include
#TCLINCDIR = $(HOME)/include

# Location of the Tcl library:
# (Ignore the "-L" bit - the linker needs it.)

#TCLLIBDIR =
TCLLIBDIR = -L/usr/local/lib
#TCLLIBDIR = -L$(HOME)/lib

#
# Flag for linking the Tcl library to TkDesk:

#TCLLIB = -ltcl
#TCLLIB = -ltcl7.4
#TCLLIB = -ltcl7.5
TCLLIB = -ltcl7.6

#
# Location of the Tk include files:

#TKINCDIR = /usr/include/tcl        # Linux with Slackware 3.0
TKINCDIR = /usr/local/include
#TKINCDIR = $(HOME)/include

# Location of the Tk library:
# (Ignore the "-L" bit - the linker needs it.)

#TKLIBDIR =
TKLIBDIR = -L/usr/local/lib
#TKLIBDIR = -L$(HOME)/lib

#
# Flag for linking the Tk library to TkDesk:

#TKLIB = -ltk
#TKLIB = -ltk4.0
#TKLIB = -ltk4.1
TKLIB = -ltk4.2

# --------------------------------
# X11 Paths:
# --------------------------------

#
# Location of X11 include files:

XINCDIR = /usr/X11R6/include
#XINCDIR = /usr/include/X11

# Location of libX11.*:
# (Ignore the "-L" bit - the linker needs it.)

XLIBDIR = -L/usr/X11R6/lib
#XLIBDIR = -L/usr/lib/X11

# --------------------------------
# System Settings:
# --------------------------------

# Flags for system-specific other libs, if required:
# (PLEASE NOTE:
# If you're linking with Tcl >=7.5 with dynamic loading enabled you need
# to add the appropriate library which allows for dynamic loading on
# your system, such as -ldl for Linux and SGI or -ldld for HP-UX.)

#OTHERLIBS = -lieee           # older Linux with Tcl 7.4
#OTHERLIBS = -ldl -lieee      # older Linux with Tcl 7.5 and dynamic loading
OTHERLIBS = -ldl              # newer Linux with Tcl 7.5 and dynamic loading
#OTHERLIBS = -ldl -lnsl -lsocket   # for Solaris
#OTHERLIBS = -ldl -lsocket    # SGI with IRIX 5.3 and Tcl 7.5
#OTHERLIBS = -ldld            # HP-UX 9.05 and Tcl 7.5
#OTHERLIBS =

# --------------------------------
# Compiler and Linker Settings:
# --------------------------------

#
# Choose your compiler on the following line:

CC = gcc
#CC = cc

#
# Your favorite CFLAGS:

CFLAGS = -O2
#CFLAGS = -O
#CFLAGS = -Wall -g

#
# And options for the linker:

LDFLAGS = -s


# ---------------------------------------------------------------------------
# --------------------  End of Configuration Section ------------------------
# ---------------------------------------------------------------------------


PKGNAME = tkdesk
VERSION = 1.0b4
FVERSION = 1b4
PKGDIR = $(PKGNAME)-$(VERSION)

TCLLIBDIRFLAG = $(TCLLIBDIR)
TKLIBDIRFLAG = $(TKLIBDIR)
XLIBDIRFLAG = $(XLIBDIR)

MAKELIB = make CC="$(CC)" CFLAGS="$(CFLAGS) -I$(TCLINCDIR) -I$(TKINCDIR) -I$(XINCDIR)"

all: config shell tkdesk
	@echo "========= Done."
	@echo "You can try out TkDesk before installing it by commenting"
	@echo "out the fifth line in the file tkdesk and then executing:"
	@echo "./tkdesksh tkdesk -configdir ./tcldesk/configs"

shell: makelibs tkAppInit.o
	$(CC) $(LDFLAGS) tkAppInit.o -o tkdesksh libdesk/libdesk.a \
		netscape-remote/libnetscape.a \
		blt/src/libBLT.a $(TKLIBDIRFLAG) $(TKLIB) \
		$(XLIBDIRFLAG) -lX11 itcl/src/libitcl.a \
		$(TCLLIBDIRFLAG) $(TCLLIB) $(OTHERLIBS) -lm


static: makelibs tkAppInit.o
	$(CC) $(LDFLAGS) tkAppInit.o -o tkdesksh-static libdesk/libdesk.a \
		netscape-remote/libnetscape.a \
		blt/src/libBLT.a /usr/local/lib/libtk4.2.a \
		$(XLIBDIRFLAG) -lX11 itcl/src/libitcl.a \
		/usr/local/lib/libtcl7.6.a $(OTHERLIBS) -lm

tkAppInit.o: tkAppInit.c
	$(CC) $(CFLAGS) -I$(TCLINCDIR) -I$(TKINCDIR) -I$(XINCDIR) -c tkAppInit.c

config:
	@rm -f config.tkdesk
	@echo "CC=\"$(CC)\"" >config.tkdesk
	@echo "CFLAGS=\"$(CFLAGS)\"" >>config.tkdesk
	@echo "TCLINCDIR=\"$(TCLINCDIR)\"" >>config.tkdesk
	@echo "TCLLIBDIR=\"$(TCLLIBDIR)\"" >>config.tkdesk
	@echo "TKINCDIR=\"$(TKINCDIR)\"" >>config.tkdesk
	@echo "TKLIBDIR=\"$(TKLIBDIR)\"" >>config.tkdesk
	@echo "XINCDIR=\"$(XINCDIR)\"" >>config.tkdesk
	@echo "XLIBDIR=\"$(XLIBDIR)\"" >>config.tkdesk
	@echo "======== Configuring in blt..."
	@rm -f blt/config.cache blt/config.BLT
	@(cd blt; ./configure --with-cc="$(CC)" --with-cflags="$(CFLAGS)")
	@(cd libdesk; rm -f config.h; ln -s ../blt/src/bltConfig.h config.h)
	@echo ""; echo "======== Configuring in itcl..."
	@(cd itcl; ./configure)

makelibs:
	cd blt; make lib-static
	cd itcl; make libitcl.a
	cd netscape-remote; $(MAKELIB) libnetscape.a
	cd libdesk; $(MAKELIB) libdesk.a

#
# The following target is intentionally called tkdesk, so I don't overwrite
# my working version of the tkdesk file.
#
tkdesk: thetools
	@echo '#!/bin/sh' >tkdesk
	@echo '#-*- tcl -*- \' >>tkdesk
	@echo "exec $(SEARCH_BINDIR)/tkdesksh \"\$$0\" \"\$$@\"" >>tkdesk
	@echo "" >>tkdesk
	@echo "set tkdesk(library) \"$(SEARCH_LIBDIR)\"" >>tkdesk
	@echo "set tkdesk(in_development) 0" >>tkdesk
	@echo "set tkdesk(debug) 0" >>tkdesk
	@cat tkdesk.main >>tkdesk
	@chmod 755 tkdesk

thetools:
	@(cd tools ;\
	for f in ed-tkdesk cd-tkdesk od-tkdesk; do \
	echo '#!/bin/sh' >$$f ;\
	echo '#-*- tcl -*- \' >>$$f ;\
	echo "exec $(SEARCH_BINDIR)/tkdesksh \"\$$0\" \"\$$@\"" >>$$f ;\
	echo "" >>$$f ;\
	cat $$f.main >>$$f ;\
	chmod 755 $$f ;\
	done)

install: shell tkdesk
	@echo "=== Installing TkDesk's library..."
	-mkdir $(LIBDIR)
	-mkdir $(LIBDIR)/configs
	-mkdir $(LIBDIR)/configs/.trash
	cp -r tcldesk/* $(LIBDIR)
	rm -f $(LIBDIR)/configs/_*
	find $(LIBDIR) -type f -exec chmod a+r {} \;
	find $(LIBDIR) -type d -exec chmod a+rx {} \;
	@echo "=== Installing the executables..."
	@rm -f $(BINDIR)/tkdesksh $(BINDIR)/tkdesk $(BINDIR)/pauseme $(BINDIR)/ed-tkdesk $(BINDIR)/cd-tkdesk $(BINDIR)/od-tkdesk
	cp tkdesksh $(BINDIR)
	cp tkdesk tools/pauseme tools/ed-tkdesk tools/cd-tkdesk tools/od-tkdesk $(BINDIR)
	chmod 755 $(BINDIR)/*tkdesk* $(BINDIR)/pauseme
	@echo "=== Installation complete."

uninstall:
	@echo "=== Uninstalling TkDesk's library..."
	-rm -r $(LIBDIR)
	@echo "=== Uninstalling TkDesk's executables..."
	-rm $(BINDIR)/tkdesksh $(BINDIR)/tkdesk $(BINDIR)/pauseme $(BINDIR)/ed-tkdesk $(BINDIR)/cd-tkdesk $(BINDIR)/od-tkdesk
	@echo "=== Done."

clean:
	cd blt; make clean
	cd itcl; make clean
	cd netscape-remote; make clean
	cd libdesk ; $(MAKELIB) clean
	cd tcldesk ; rm -f #*#
	rm -f *.o tkdesksh

tags:
	cd libdesk ; etags *.c
	cd tcldesk ; etags *.tcl

export:
	@echo "Making distribution for $(PKGDIR) ..."
	@echo "Did you think about updating tkdesk.main and CFLAGS?"
	@(cd .. ;\
	mkdir $(PKGDIR) ;\
	mkdir $(PKGDIR)/blt ;\
	mkdir $(PKGDIR)/blt/cf ;\
	mkdir $(PKGDIR)/blt/src ;\
	mkdir $(PKGDIR)/doc ;\
	mkdir $(PKGDIR)/itcl ;\
	mkdir $(PKGDIR)/itcl/src ;\
	mkdir $(PKGDIR)/netscape-remote ;\
	mkdir $(PKGDIR)/libdesk ;\
	mkdir $(PKGDIR)/tcldesk ;\
	mkdir $(PKGDIR)/tcldesk/cb_tools ;\
	mkdir $(PKGDIR)/tcldesk/cb_tools/bitmaps ;\
	mkdir $(PKGDIR)/tcldesk/doc ;\
	mkdir $(PKGDIR)/tcldesk/images ;\
	mkdir $(PKGDIR)/tcldesk/images/ficons16 ;\
	mkdir $(PKGDIR)/tcldesk/images/ficons32 ;\
	mkdir $(PKGDIR)/tcldesk/images/next ;\
	mkdir $(PKGDIR)/tcldesk/sounds ;\
	mkdir $(PKGDIR)/tcldesk/configs ;\
	mkdir $(PKGDIR)/tcldesk/configs/.trash ;\
	mkdir $(PKGDIR)/tools ;\
	cp TkDesk/{COPYING,INSTALL,Makefile,TODO,license.terms,README} \
		TkDesk/{*.c,tkdesk.main} $(PKGDIR) ;\
	cp TkDesk/blt/{MANIFEST,Makefile.in,README,README.BLT,*.h} \
		TkDesk/blt/{config.BLT,configure,configure.in} \
		$(PKGDIR)/blt ;\
	cp TkDesk/blt/cf/* $(PKGDIR)/blt/cf ;\
	cp TkDesk/blt/src/{*.in,*.c,bltInt.h,bltList.h} $(PKGDIR)/blt/src ;\
	cp TkDesk/doc/{guide.sgml,guide.txt,guide.ps} $(PKGDIR)/doc ;\
	cp TkDesk/itcl/{Makefile.in,README,configure,configure.in} \
		$(PKGDIR)/itcl ;\
	cp TkDesk/itcl/src/{*.in,*.c,*.h} $(PKGDIR)/itcl/src ;\
	cp TkDesk/netscape-remote/{Makefile,README,*.c} \
		$(PKGDIR)/netscape-remote ;\
	cp TkDesk/libdesk/{Makefile,*.c,*.h} $(PKGDIR)/libdesk ;\
	cp TkDesk/tcldesk/*.tcl $(PKGDIR)/tcldesk ;\
	cp TkDesk/tcldesk/cb_tools/{*.tcl,tclIndex} \
		$(PKGDIR)/tcldesk/cb_tools ;\
	cp TkDesk/tcldesk/cb_tools/bitmaps/*.xbm \
		$(PKGDIR)/tcldesk/cb_tools/bitmaps ;\
	cp TkDesk/tcldesk/doc/{License,Guide,*.html} $(PKGDIR)/tcldesk/doc ;\
	cp TkDesk/tcldesk/doc/QuickStart $(PKGDIR)/tcldesk/doc ;\
	cp TkDesk/CHANGES $(PKGDIR)/tcldesk/doc ;\
	cp TkDesk/tcldesk/images/{*.xbm,*.xpm} $(PKGDIR)/tcldesk/images ;\
	cp TkDesk/tcldesk/images/ficons16/*.xpm \
		$(PKGDIR)/tcldesk/images/ficons16 ;\
	cp TkDesk/tcldesk/images/ficons32/*.xpm \
		$(PKGDIR)/tcldesk/images/ficons32 ;\
	cp TkDesk/tcldesk/images/next/*.xpm \
		$(PKGDIR)/tcldesk/images/next ;\
	cp TkDesk/tcldesk/sounds/{*.voc,*.wav,*.au} $(PKGDIR)/tcldesk/sounds ;\
	cp TkDesk/tcldesk/configs/{AppBar,ButtonBar,Commands,Directories} \
		TkDesk/tcldesk/configs/{FileTags,Local,Popups} \
		TkDesk/tcldesk/configs/{Sounds,System} \
		$(PKGDIR)/tcldesk/configs ;\
	cp TkDesk/tools/{*.main,mkindex,pauseme} $(PKGDIR)/tools ;\
	gtar czvpf $(PKGDIR).tar.gz $(PKGDIR) ;\
	mv $(PKGDIR).tar.gz TkDesk ;\
	rm -rf $(PKGDIR) )

floppy:
	mcopy $(PKGNAME)-$(VERSION).tar.gz a:tkdsk$(FVERSION).tgz
	mdir a:


