# Makefile for figlet version 2.1 (28 Apr 1995)
# (C) 1993, 1994 Glenn Chappell and Ian Chai

# Please make sure DESTDIR, MANDIR, DEFAULTFONTDIR and
#   DEFAULTFONTFILE are defined to reflect the situation
#   on your computer.  See README for details.

# Don't change this even if your shell is different. The only reason
# for changing this is if sh is not in the same place.
SHELL = /bin/sh

# Where the executables should be put
DESTDIR = /usr/games

# Where the man page should be put
MANDIR = /usr/man/man6

# Where figlet will search first for fonts (the ".flf" files) and
#   control files (the ".flc" files).
DEFAULTFONTDIR = /usr/games/lib/figlet.dir

# The filename of the font to be used if no other is specified
#   (standard.flf is recommended, but any other can be used).
#   This font file should reside in the directory specified by
#   DEFAULTFONTDIR.
DEFAULTFONTFILE = standard.flf

##
##  END OF CONFIGURATION SECTION
##

SOURCES = figlet.c

all: figlet chkfont

figlet: $(SOURCES)
	$(CC) $(CFLAGS) -DDEFAULTFONTDIR=\"$(DEFAULTFONTDIR)\" \
	   -DDEFAULTFONTFILE=\"$(DEFAULTFONTFILE)\" \
	   $(SOURCES) -o figlet
	chmod 755 figlet

chkfont: chkfont.c
	$(CC) $(CFLAGS) -o chkfont chkfont.c

clean:
	rm -f *.o figlet chkfont

install: figlet chkfont
	- mkdir $(DEFAULTFONTDIR)
	chmod 755 $(DEFAULTFONTDIR)
	cp figlet $(DESTDIR)
	chmod 755 $(DESTDIR)/figlet
	cp chkfont $(DESTDIR)
	chmod 755 $(DESTDIR)/chkfont
	cp figlist $(DESTDIR)
	chmod 755 $(DESTDIR)/figlist
	cp showfigfonts $(DESTDIR)
	chmod 755 $(DESTDIR)/showfigfonts
	cp fonts/*.flf $(DEFAULTFONTDIR)
	chmod 644 $(DEFAULTFONTDIR)/*.flf
	cp fonts/*.flc $(DEFAULTFONTDIR)
	chmod 644 $(DEFAULTFONTDIR)/*.flc
	cp figlet.6 $(MANDIR)
	chmod 644 $(MANDIR)/figlet.6
