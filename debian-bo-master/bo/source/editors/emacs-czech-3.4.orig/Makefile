# Makefile for installation of emacs-czech
# $Id: Makefile,v 3.4 1997/02/26 20:16:13 pdm Exp $


EMACS=emacs
#EMACS=xemacs

LISPDIR=/usr/local/lib/emacs/site-lisp/czech/lisp
#LISPDIR=/usr/local/site-lisp/czech

INFODIR=/usr/local/lib/emacs/site-lisp/czech/info
#INFODIR=/usr/local/info

GZIP=yes   # comment this out if you have not gzip
CSTEX=cstex


### NO CONFIGURATION AFTER THIS LINE ###

DISTDIR=emacs-czech-3.4

compile:
	rm -f *.elc
	$(EMACS) -batch -l compile-czech.el

install: install-lisp install-info
	@echo "Installation completed."

install-lisp: compile
	if [ ! -d $(LISPDIR) ]; then mkdir -p $(LISPDIR); fi
	cp -f *.elc $(LISPDIR)

install-info: info
	if [ ! -d $(INFODIR) ]; then mkdir -p $(INFODIR); fi
	if [ $(GZIP) = yes ]; then rm -f *.gz; gzip -9 *.info*; fi
	cp -f *.info* $(INFODIR)

clean:
	rm -f *.elc *.aux *.dvi *.fns *.ilg *.ind *.kys *.log *.toc *.vrs \
	emacs-czech.[^p]? emacs-czech.pg

mrproper: clean
	rm -f *.info* *.ps

info: emacs-czech.info-1

emacs-czech.info-1: emacs-czech.texi
	-makeinfo emacs-czech.texi
	$(MAKE) texinfo

texinfo: emacs-czech.info

emacs-czech.info: emacs-czech.texi
	$(EMACS) -batch emacs-czech.texi -f texinfo-format-buffer -f save-buffer

dvi: emacs-czech.dvi

emacs-czech.dvi: emacs-czech.texi
	TEX=$(CSTEX) texi2dvi emacs-czech.texi

ps: emacs-czech.ps

emacs-czech.ps: emacs-czech.dvi
	dvips emacs-czech

package: info ps
	rm -rf $(DISTDIR)
	rm -f $(DISTDIR).tar.gz
	mkdir $(DISTDIR)
	cp czech*.el compile-czech.el iso02-syn.el cztexinfo.tex *.texi *.ps *.info* Makefile COPYING FILES README logo.xpm $(DISTDIR)
	chmod 644 $(DISTDIR)/*
	tar cvf $(DISTDIR).tar $(DISTDIR)
	gzip -9 $(DISTDIR).tar

