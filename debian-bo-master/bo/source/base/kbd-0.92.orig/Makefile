# Note: keytables used to be in /usr/lib, here it is in /usr/lib/kbd
# (and BINDIR, DATADIR also occur in src/Makefile).
MANDIR  = /usr/man
BINDIR  = /usr/bin
DATADIR = /usr/lib/kbd
# If you change the names of any of the following subdirs,
# also change paths.h.
KEYMAPDIR = keytables
FONTDIR   = consolefonts
TRANSDIR  = consoletrans
SUBDIRS = $(KEYMAPDIR) $(FONTDIR) $(TRANSDIR)

DIR	= kbd-0.92

.EXPORT_ALL_VARIABLES:

all:
	cd src && make all

install:
	cd src && make install
#	chown root $(BINDIR)/resizecons && chmod u+s $(BINDIR)/resizecons
	for i in man?; do \
		install -d -m 755 $(MANDIR)/$$i; \
		install -m 644 $$i/* $(MANDIR)/$$i; \
	done
	install -d $(DATADIR)
	for i in $(SUBDIRS); do \
		install -d -m 755 $(DATADIR)/$$i ;\
		install -m 644 $$i/* $(DATADIR)/$$i ;\
	done

tar: reallyclean # analyze.c loadkeys.c
	cd ..; 	tar cvf - ./$(DIR) | gzip > $(DIR).tar.gz

clean:
	cd src && make clean

reallyclean spotless: clean
	find . -name "*~" -exec rm {} ";"
	cd src && make reallyclean

