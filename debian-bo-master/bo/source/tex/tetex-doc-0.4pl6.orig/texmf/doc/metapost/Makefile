# Makefile for texmf/doc/metapost:
# ================================

MP	= mp

#DVIPS	= dvips -Pold
DVIPS	= dvips -Pold -D300

LATEX	= latex
BIBTEX	= bibtex

TROFF	= troff -Tpost -ms -mpictures | dpost
GROFF	= groff -Tps -mgs

###

MPMAN_FIGS = \
	manfig.0   manfig.2   manfig.3   manfig.104 manfig.204 manfig.5   \
	manfig.6   manfig.7   manfig.8   manfig.109 manfig.209 manfig.110 \
	manfig.210 manfig.310 manfig.111 manfig.211 manfig.311 manfig.411 \
	manfig.13  manfig.14  manfig.17  manfig.18  manfig.19  manfig.20  \
	manfig.21  manfig.22  manfig.123 manfig.223 manfig.24  manfig.25  \
	manfig.26  manfig.28  manfig.29  manfig.30  manfig.31  manfig.32  \
	manfig.33  manfig.34  manfig.35  manfig.36  manfig.37  manfig.38  \
	manfig.40  manfig.42  manfig.45  manfig.48  manfig.49  manfig.50  \
	manfig.51  manfig.52

MPGRAPH_FIGS = \
	mpgraph.1  mpgraph.2  mpgraph.3  mpgraph.4  mpgraph.5  mpgraph.6  \
	mpgraph.7  mpgraph.8  mpgraph.9  mpgraph.10 mpgraph.11
MPGRPAH_DATA = \
	agepop91.d agepopm.d countries.d energy.d lead.d matmul.d timepop.d

EXAMPLE_FIGS = \
	examples.1 examples.2 examples.3 examples.4 examples.5 examples.6 \
	examples.7 examples.8 examples.9 

###

default: all
all: mpman.ps mpgraph.ps

### MetaPost manuals

mpman.ps: mpman.dvi $(MPMAN_FIGS)
	@echo "WARNING: mpman.dvi was build using the obsolete font rpsyr."
	@echo "You can safely ignore one warning about a checksum mismatch."
	$(DVIPS) mpman.dvi -o mpman.ps

mpgraph.ps: mpgraph.dvi $(MPGRAPH_FIGS)
	@echo "WARNING: mpgraph.dvi was build using the obsolete versions of"
	@echo "the fonts logo10 logo8 logosl10. You can safely ignore dvips's"
	@echo "checksum warnings for these fonts."
	$(DVIPS) mpgraph.dvi -o mpgraph.ps

$(MPMAN_FIGS): manfig.mp
	$(MP) manfig.mp

$(MPGRAPH_FIGS): mpgraph.mp $(MPGRAPH_DATA)
	$(MP) mpgraph.mp

### LaTeX example

$(EXAMPLE_FIGS): examples.mp
	$(MP) examples.mp

### troff and/or groff example

grdemo.ps: grdemo.ms figs.1
	cat grdemo.ms | $(GROFF) > grdemo.ps

demo.ps: demo.ms figs.1 /usr/lib/tmac/tmac.pictures
	cat demo.ms | $(TROFF) > demo.ps

figs.1: figs.mp
	test -d /usr/lib/font/devpost || $(MAKE) /usr/lib/font/devpost
	test -d /usr/lib/font/devpost && $(MP) -T figs.mp

/usr/lib/tmac/tmac.pictures:
	@echo
	@echo "WARNING: You don't seem to have the troff \`mpictures' macros"
	@echo "which are needed to typeset the \`demo.ps' example document."
	@echo "If your troff implementation doesn't provide these macros"
	@echo "you're out of luck, I'm afraid.  Sorry, I can't help you!"
	@echo 
	@echo "Perhaps you may want to try \`grdemo.ps' as an alternative"
	@echo "if you have GNU groff installed on your system?"
	@echo
	@false

/usr/lib/font/devpost:
	@echo
	@echo "WARNING: You don't seem to have a suitable troff implementation"
	@echo "on your system that matches the assumptions built into the"
	@echo "MetaPost support programs.  To run MetaPost in troff mode"
	@echo "you'll probably need a troff implementation from Bell Labs."
	@echo "Using GNU groff as an alternative won't work, I'm afraid."
	@echo
	@false
	

### pattern rules

.dvi.ps:
	$(DVIPS) $< -o $@

### cleanup targets

clean:
	rm -f $(MPMAN_FIGS) $(MPGRAPH_FIGS) $(EXAMPLE_FIGS)
	rm -f *.mpx *.log *.aux mpxerr*

distclean: clean
	rm -f mpman.ps mpgraph.ps

.PHONY: clean
.SUFFIXES: .dvi .ps
