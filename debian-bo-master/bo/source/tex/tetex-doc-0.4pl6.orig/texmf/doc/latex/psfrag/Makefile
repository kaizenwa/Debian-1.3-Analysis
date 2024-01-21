SHELL=/bin/sh

.SUFFIXES: .ps .dvi .dtx

default: pfgguide.ps clean
all: default

.dvi.ps:
	dvips -o $*.ps $*.dvi

.dtx.dvi:
	latex $*.dtx
	latex $*.dtx

clean:
	rm -f *.aux *.log

realclean: clean
	rm -f *.ps
