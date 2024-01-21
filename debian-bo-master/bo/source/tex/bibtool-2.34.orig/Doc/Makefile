##*****************************************************************************
## $Id: Makefile,v 2.38 1995/01/01 18:25:15 gerd Exp $
##=============================================================================
## 
## This file is part of BibTool.
## It is distributed under the GNU General Public License.
## See the file COPYING for details.
## 
## (c) 1995 Gerd Neugebauer
## 
## Net: gerd@imn.th-leipzig.de
## 
##*****************************************************************************

LATEX     = latex
BIBTEX    = bibtex
MAKEINDEX = makeindex

DIR_SEP   = /

.SUFFIXES: .tex .toc .dvi .aux .bbl .ind .idx $(SUFFIXES)

.tex.dvi:
	$(LATEX) $<
.tex.toc:
	$(LATEX) $<
.tex.idx:
	$(LATEX) $<
.tex.aux:
	$(LATEX) $<
.aux.bbl:
	$(BIBTEX) $*

bibtool.dvi: bibtool.tex bibtool.bbl bibtool.ind
	$(LATEX) bibtool
	$(LATEX) bibtool

bibtool.ind ind index: bibtool.idx
	$(MAKEINDEX) -s bibtool.ist bibtool

config.tex: ..$(DIR_SEP)Makefile ..$(DIR_SEP)config.h
	cd ..; $(MAKE) Doc$(DIR_SEP)config.tex

clean:
	$(RM) *.bak *.BAK *~ *.log *.ilg *.blg

veryclean:
	$(RM) *.bak *.BAK *~ *.log *.toc *.aux *.idx *.ind *.ilg *.bbl *.blg


