# Makefile for debiandoc-sgml

prefix=/usr/local
bindir=$(prefix)/bin
libdir=$(prefix)/lib/debiandoc-sgml
docdir=$(prefix)/doc
mydocdir=$(docdir)/debiandoc-sgml
htmldocdir=$(mydocdir)/markup.html
sgmllibdir=$(libdir)/sgml
catalog=$(sgmllibdir)/catalog
pathlibdir=$(libdir)/bin
speclibdir=$(libdir)/spec
mandir=$(prefix)/man
man1dir=$(prefix)/man/man1
man1=1

INSTALL=install
INSTALL_DIR=$(INSTALL) -d -m 755
INSTALL_SCRIPT=$(INSTALL) -c -m 755
INSTALL_DATA=$(INSTALL) -c -m 644

STDCONVERSIONS= debiandoc2html debiandoc2lout debiandoc2text debiandoc2textov
PROGRAMS= $(STDCONVERSIONS) debiandoc2ps
MAN1S= debiandoc2ps
STDCONVMAN= debiandoc2ps
HELPERS= saspconvert
SPECS= text html lout
SGMLS= catalog debiandoc.dtd general

PATH:=:$(PATH)
RM=/bin/rm

version=1.0

all:		$(STDCONVERSIONS) markup.html-stamp

$(STDCONVERSIONS):	template mkconversions Makefile
		sh mkconversions $(version) $(speclibdir) $(sgmllibdir) $(pathlibdir)

markup.html-stamp:	markup.sgml debiandoc.dtd spec-html
		nsgmls -l -D. markup.sgml >markup.sasp
		./saspconvert <markup.sasp >markup.sasp-lout size book
		rm -rf markup.html
		sgmlspl spec-html <markup.sasp-lout markup.html/
		touch markup.html-stamp

install:	all
		$(INSTALL_DIR) $(bindir) $(sgmllibdir) $(pathlibdir) \
				$(speclibdir) $(htmldocdir) $(man1dir)
		set -e; for f in $(PROGRAMS); do $(INSTALL_SCRIPT) $$f $(bindir); done
		set -e; for f in $(HELPERS); do $(INSTALL_SCRIPT) $$f $(pathlibdir); done
		set -e; for f in $(SGMLS); do $(INSTALL_DATA) $$f $(sgmllibdir); done
		set -e; for f in $(SPECS); \
		 do $(INSTALL_DATA) spec-$$f $(speclibdir)/$$f; done
		set -e; for f in $(MAN1S); \
		 do $(INSTALL_DATA) $$f.1 $(man1dir)/$$f.$(man1); done
		set -e; for f in $(STDCONVERSIONS); do \
		 ln -s $(STDCONVMAN).$(man1) $(man1dir)/$$f.$(man1); done
		set -e; for f in markup.html/*; \
		 do $(INSTALL_DATA) $$f $(htmldocdir)/.; done
		$(INSTALL_DATA) markup.sgml $(mydocdir)/markup.sgml

clean:
		rm -rf {markup,test}.{ps,text,lout,sasp,html}*
		rm -f $(STDCONVERSIONS) *.new lout.li *.lix *.ldx *~

#clean:
#		$(RM) *.html/*
#		$(RM) -f *.html-stamp *.sasp *.*-sasp *.plain *.ostruck *~
#		$(RM) -f lout.li {policy,programmer}.ld *.lix *.ldx ps *.new
