# Note: Typing 'make' from this level will rebuild xsok from scratch.
#       Afterwards, type 'make install' or 'make install.fsstnd'
#	(as root) to install the game in default directories.
#	A manual is in the doc subdirectory and can be TeXed by 'make manual'.
#
#       You may change src/Imakefile for different configurations.
#       But then, you're on your own...

all:
	(cd src && xmkmf && $(MAKE) && strip xsok)
	(cd lib && $(MAKE))
	(cd src && $(MAKE) testname)

manual:
	(cd doc && $(MAKE) xsok.dvi)

# different install targets: imake default, local, Linux FSSTND
install:
	(cd src && $(MAKE) install)

install.local:
	(cd src && $(MAKE) install.local)

install.fsstnd:
	(cd src && $(MAKE) install.fsstnd)

clean:
	(cd lib && $(MAKE) clean)
	(cd src && xmkmf && $(MAKE) clean)
	(cd doc && $(MAKE) clean)
	(cd solver && $(MAKE) clean)
	rm -f src/Makefile
	find . -name "*~" -exec rm \{\} \;

distrib:
	$(MAKE) clean
	(cd ..; tar cvfz $(HOME)/xsok-1.01-src.tar.gz xsok-1.01)

bindistrib:
	(cd /; tar cvfz $(HOME)/xsok-1.01-bin.tar.gz var/games/xsok/*.score \
	 usr/games/bin/xsok usr/man/man6/xsok.6x usr/games/lib/xsok \
	usr/doc/xsok)
