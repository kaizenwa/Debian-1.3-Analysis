
PREFIX = /usr/local

BINDIR = $(PREFIX)/bin
MANDIR = $(PREFIX)/man
MAN1DIR= $(MANDIR)/man1


default:
	@echo "Make what?  (install, clean, source)"


perlcheck:
	@test -x "`which perl`" || (echo; echo "*** Can't find Perl! ***"; echo; exit 1)
	@p=`which perl`; test "`head -1 $$p | grep \#\!`" = "" || (echo; \
		echo "*** '$$p' is a shell script!"; \
		echo "    edit 'signify' by hand to call perl binary"; \
		echo; )


install: perlcheck
	umask 022; mkdir -p $(MAN1DIR) $(BINDIR)
	perl=`which perl`; sed -e "1s|.*|#! $$perl|" signify.pl >$(BINDIR)/signify
	cp signify.1 $(MAN1DIR)
	chmod 755 $(BINDIR)/signify
	chmod 644 $(MAN1DIR)/signify.1


clean:
	-rm -f stamp-build
	-rm -f debian/files
	-rm -rf debian-tmp
	-rm -f signify.txt
	find . \( -name "#*" -o -name "*~" \) -print | xargs rm -f


source: clean
	umask 022; nroff -man signify.1 | col -b >signify.txt
	DIR=`basename $$PWD`; cd ..; rm -f $$DIR.tar.gz; tar cf - $$DIR | gzip -9 >$$DIR.tar.gz
	DIR=`basename $$PWD`; cd ..; rm -f $$DIR.zip;    zip -r9lq $$DIR.zip $$DIR
