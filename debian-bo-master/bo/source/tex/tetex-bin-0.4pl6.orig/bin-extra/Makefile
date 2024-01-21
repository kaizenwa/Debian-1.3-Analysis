SCRIPTS= MakeTeXMF MakeTeXPK MakeTeXTFM MakeTeXmkdir MakeTeXnames allcm allneeded append_db dvi2fax dvihp dvired fontexport fontimport mkfontdesc mkindex ps2frag texconfig texhash texi2dvi xdvi troffmpx makempx makeindex pslatex texi2html

FILES = $(SCRIPTS) mf mp latex amstex tex alldc kpsepath kpsewhich kpsexpand
bindir = $(prefix)/bin/$(platform)

clean distclean default all:

install: test-prefix test-platform $(bindir)
	cd $(bindir) && rm -f $(FILES)
	cp $(SCRIPTS) $(bindir)
	ln -s allcm $(bindir)/alldc
	ln -s kpsetool $(bindir)/kpsepath
	ln -s kpsetool $(bindir)/kpsewhich
	ln -s kpsetool $(bindir)/kpsexpand
	ln -s virmf $(bindir)/mf
	ln -s virmp $(bindir)/mp
	ln -s virtex $(bindir)/amstex
	ln -s virtex $(bindir)/latex
	ln -s virtex $(bindir)/tex
ifeq (aix4,$(findstring aix4, $(platform)))  
	set -x; for i in $(FILES); do ./fix-aix4-sh $(bindir)/$$i; done
endif
ifeq (ultrix,$(findstring ultrix, $(platform)))  
	set -x; for i in $(FILES); do ./fix-ultrix-sh $(bindir)/$$i; done
endif


$(bindir):
	mkdir -p $(bindir)

test-prefix:	
ifeq ($(prefix),)
	echo "prefix is unset."
	@exit 1
endif

test-platform:
ifeq ($(platform),)
	echo "platform is unset."
	@exit 1
endif
