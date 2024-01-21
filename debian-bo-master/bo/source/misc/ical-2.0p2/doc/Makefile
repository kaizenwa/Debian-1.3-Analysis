# This Makefile uses programs available locally.
# So don't complain if it does not work for you.

MAN  = ical
IDOC = interface
L2HFLAGS = -address sanjay@lcs.mit.edu
DIRT = *.dvi *.aux *.log *.toc tmp.*
RDOCS = $(MAN).man $(MAN).doc $(IDOC).tex $(IDOC).ps

all: $(RDOCS) $(IDOC)/$(IDOC).html scrub

release: $(RDOCS) scrub

$(MAN).man: $(MAN).html
	html2man $(MAN).html > $@

$(MAN).doc: $(MAN).html
	html2tk $(MAN).html > $@

$(IDOC).tex: $(IDOC).gtex
	glatex -f $(IDOC).gtex

$(IDOC).ps: $(IDOC).gtex
	cp $(IDOC).gtex tmp.gtex
	glatex tmp.gtex
	latex tmp.tex
	latex tmp.tex
	dvips tmp.dvi
	mv tmp.ps $(IDOC).ps

$(IDOC)/$(IDOC).html: $(IDOC).tex
	latex $(IDOC).tex
	latex2html $(L2HFLAGS) $(IDOC).tex
	perl -i ./cleanhtml interface/interface.html

clean: scrub

scrub:
	rm -f $(DIRT)
