LATEX=latex
RLATEX=./rlatex
FIG2DEV=fig2dev
SHELL=/bin/sh

IMAGES=bootloader.tex map.tex image.tex parameter.tex other.tex

.SUFFIXES:	.fig .tex .dvi

.fig.tex:
		$(FIG2DEV) -L latex $*.fig $*.tex

.tex.dvi:
		$(RLATEX) $*

all:		user.dvi tech.dvi

user.dvi:	user.tex

tech.dvi:	tech.tex $(IMAGES)

Manual.txt:	user.tex
		perl t2a.pl <user.tex >Manual.txt

again:
		$(LATEX) user
		$(LATEX) tech

clean:
		rm -f *.toc *.aux *.log

spotless:	clean
		rm -f *.dvi $(IMAGES)

dummies:
		for n in $(IMAGES); do echo '(omitted)' >$$n; done
