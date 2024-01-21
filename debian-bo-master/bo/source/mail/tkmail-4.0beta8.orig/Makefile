#
# Makefile for TkMail 4.0
#
# Paul Raines (raines@slac.stanford.edu)
# 		Hacked from Makefile for TkMan from Tom Phelps
#

# This Makefile includes the most critical and site specific
# settings needed to run TkMail. See the SETTINGS section at
# the top of the tkmail file for other settings.
SHELL = /bin/sh

include Config.mk

files = tkmail-$(VERSION)/README \
    tkmail-$(VERSION)/BETANOTES \
    tkmail-$(VERSION)/TODO \
    tkmail-$(VERSION)/install-sh \
    tkmail-$(VERSION)/Makefile \
    tkmail-$(VERSION)/Config.mk \
    tkmail-$(VERSION)/configure \
    tkmail-$(VERSION)/configure.in \
    tkmail-$(VERSION)/scripts/ChangeLog \
    tkmail-$(VERSION)/scripts/Makefile.in \
    tkmail-$(VERSION)/scripts/altedit.tk \
    tkmail-$(VERSION)/scripts/compose.tk \
    tkmail-$(VERSION)/scripts/example.tkmail4rc \
    tkmail-$(VERSION)/scripts/fancylb.tk \
    tkmail-$(VERSION)/scripts/file.tcl \
    tkmail-$(VERSION)/scripts/help.txt \
    tkmail-$(VERSION)/scripts/ispell.tk \
    tkmail-$(VERSION)/scripts/list.xbm \
    tkmail-$(VERSION)/scripts/mail.tk \
    tkmail-$(VERSION)/scripts/mime.tk \
    tkmail-$(VERSION)/scripts/newvar.sed \
    tkmail-$(VERSION)/scripts/note.xbm \
    tkmail-$(VERSION)/scripts/options.tk \
    tkmail-$(VERSION)/scripts/options2.tk \
    tkmail-$(VERSION)/scripts/pen.xbm \
    tkmail-$(VERSION)/scripts/tclIndex \
    tkmail-$(VERSION)/scripts/tkmail \
    tkmail-$(VERSION)/scripts/utils.tk \
    tkmail-$(VERSION)/scripts/viewer.tcl \
    tkmail-$(VERSION)/mfv/ChangeLog \
    tkmail-$(VERSION)/mfv/Makefile.in \
    tkmail-$(VERSION)/mfv/README \
    tkmail-$(VERSION)/mfv/dotlock.c \
    tkmail-$(VERSION)/mfv/mfv.c \
    tkmail-$(VERSION)/mfv/regtest.c \
    tkmail-$(VERSION)/mfv/tclAppInit.c \
    tkmail-$(VERSION)/mfv/tclExtdInt.h \
    tkmail-$(VERSION)/mfv/tclXsignal.c \
    tkmail-$(VERSION)/mfv/tclXutil.c \
    tkmail-$(VERSION)/mfv/tkAppInit.c \
    tkmail-$(VERSION)/mfv/compat \
    tkmail-$(VERSION)/tkbind/ChangeLog \
    tkmail-$(VERSION)/tkbind/README \
    tkmail-$(VERSION)/tkbind/bindxtnd.tcl \
    tkmail-$(VERSION)/tkbind/dsearch.tcl \
    tkmail-$(VERSION)/tkbind/emacs.list \
    tkmail-$(VERSION)/tkbind/entry.tcl \
    tkmail-$(VERSION)/tkbind/entry \
    tkmail-$(VERSION)/tkbind/example.tkbindrc \
    tkmail-$(VERSION)/tkbind/isearch.tcl \
    tkmail-$(VERSION)/tkbind/prompt.tcl \
    tkmail-$(VERSION)/tkbind/rectangle.tcl \
    tkmail-$(VERSION)/tkbind/text \
    tkmail-$(VERSION)/tkbind/text.tcl \
    tkmail-$(VERSION)/tkbind/tkBindTest

all:
	cd mfv; ${MAKE} all
	cd scripts; ${MAKE} all

install:
	cd scripts; ${MAKE} install
	cd mfv; ${MAKE} install
tar:
	cd ..; \
	rm -f tkmail*.tar.g*[zZ]*; \
	ln -s tkmail4 tkmail-$(VERSION); \
	tar chvf tkmail-$(VERSION).tar $(files); \
	gzip tkmail-$(VERSION).tar; \
	rm -rf tkmail-$(VERSION)
	@echo "*** Did you remember to cvs commit first?"

clean:
	cd mfv; ${MAKE} clean
	cd scripts; ${MAKE} clean

distclean:
	rm -rf config.cache config.log config.status
	rm -rf mfv/Makefile scripts/Makefile
