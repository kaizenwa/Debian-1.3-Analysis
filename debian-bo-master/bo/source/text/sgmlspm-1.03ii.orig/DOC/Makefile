########################################################################
# Makefile for producing HTML and LaTeX (and thence, DVI and
# Postscript) versions of the SGML documentation.
#
# version 1.03
#
# *** NOTE ***
#
# This will _not_ work unless you have the Docbook 2.2.1 DTD properly
# installed on your system, and sgmls or nsgmls can find it using the
# public identifier "-//HaL and O'Reilly//DTD DocBook//EN".  You will
# also need the character-entity files with the public identifiers
#
# "ISO 8879:1986//ENTITIES Publishing//EN"
#    and
# "ISO 8879:1986//ENTITIES Numeric and Special Graphic//EN"
#
# installed.  Copies of the source files for these are included in
# the Extras/ subdirectory.
########################################################################

#
# Beginning of user configuration options.
#

# Which program do you use: sgmls or nsgmls?
SGMLS = nsgmls

# What is the command for running LaTeX on a file?
LATEX = latex

# Do you want to supply a full path for sgmlspl?
SGMLSPL = sgmlspl

# If you want Postscript, what is the command for converting DVI to Postscript?
DVIPS = dvips

# What SGML declaration do you use with the Docbook 2.2.1 DTD?
SGMLDECL = /usr/local/lib/sgml/sgmldecl/docbook.dcl

#
# End of user configuration options.
#

SHELL = /bin/sh
LATEXSPEC = tolatex.pl
HTMLSPEC = tohtml.pl

all: latex html

latex: LaTeX/sgmlspm.tex LaTeX/sgmlspl.tex
dvi: LaTeX/sgmlspm.dvi LaTeX/sgmlspl.dvi
ps: PS/sgmlspm.ps PS/sgmlspl.ps
html:	HTML/SGMLSpm/sgmlspm.html HTML/sgmlspl/sgmlspl.html


LaTeX/sgmlspm.tex: sgmlspm.sgml ${LATEXSPEC}
	@echo Converting sgmlspm.sgml to LaTeX...; \
	cd LaTeX; ln -sf ../sample.pl .; \
	if [ \! -r sgmlspm.refs -o -r .redo_sgmlspm ]; then \
	  echo \(Preliminary LaTeX pass for sgmlspm.sgml...\); \
	  ${SGMLS} ${SGMLDECL} ../sgmlspm.sgml \
	    | $(SGMLSPL) ../${LATEXSPEC} sgmlspm > sgmlspm.tex; \
	fi; \
	echo \(Main LaTeX pass for sgmlspm.sgml...\); \
	${SGMLS} ${SGMLDECL} ../sgmlspm.sgml \
	  | $(SGMLSPL) ../${LATEXSPEC} sgmlspm >sgmlspm.tex

LaTeX/sgmlspm.dvi: LaTeX/sgmlspm.tex
	@echo Converting sgmlspm.tex to DVI...; \
	cd LaTeX; ${LATEX} sgmlspm.tex; ${LATEX} sgmlspm.tex

PS/sgmlspm.ps: LaTeX/sgmlspm.dvi
	@echo Converting sgmlspm.dvi to Postscript...; \
	${DVIPS} -o PS/sgmlspm.ps LaTeX/sgmlspm.dvi

HTML/SGMLSpm/sgmlspm.html: sgmlspm.sgml ${HTMLSPEC}
	@cd HTML/SGMLSpm; ln -sf ../../sample.pl .; \
	echo Converting sgmlspm.sgml to HTML...; \
	if [ \! -r sgmlspm.refs -o -r .redo_sgmlspm ]; then \
	  echo \(Preliminary HTML pass for sgmlspm.sgml...\); \
	  ${SGMLS} ${SGMLDECL} ../../sgmlspm.sgml \
	    | $(SGMLSPL) ../../${HTMLSPEC} sgmlspm; \
	fi; \
	echo \(Main HTML pass for sgmlspm.sgml...\); \
	${SGMLS} ${SGMLDECL} ../../sgmlspm.sgml \
	  | $(SGMLSPL) ../../${HTMLSPEC} sgmlspm;

LaTeX/sgmlspl.tex: sgmlspl.sgml ${LATEXSPEC}
	@echo Converting sgmlspl.sgml to LaTeX...; \
	cd LaTeX; \
	if [ \! -r sgmlspl.refs -o -r .redo_sgmlspl ]; then \
	  echo \(Preliminary LaTeX pass for sgmlspl.sgml...\); \
	  ${SGMLS} ${SGMLDECL} ../sgmlspl.sgml \
	    | $(SGMLSPL) ../${LATEXSPEC} sgmlspl > sgmlspl.tex; \
	fi; \
	echo \(Main LaTeX pass for sgmlspl.sgml...\); \
	${SGMLS} ${SGMLDECL} ../sgmlspl.sgml \
	  | $(SGMLSPL) ../${LATEXSPEC} sgmlspl > sgmlspl.tex

LaTeX/sgmlspl.dvi: LaTeX/sgmlspl.tex
	@echo Converting sgmlspl.tex to DVI...; \
	cd LaTeX; \
	${LATEX} sgmlspl.tex; ${LATEX} sgmlspl.tex

PS/sgmlspl.ps: LaTeX/sgmlspl.dvi
	@echo Converting sgmlspl.dvi to Postscript...; \
	${DVIPS} -o PS/sgmlspl.ps LaTeX/sgmlspl.dvi

HTML/sgmlspl/sgmlspl.html: sgmlspl.sgml ${HTMLSPEC}
	@cd HTML/sgmlspl; \
	echo Converting sgmlspl.sgml to HTML...; \
	if [ ! -r sgmlspl.refs -o -r .redo_sgmlspl ]; then \
	  echo \(Preliminary HTML pass for sgmlspl.sgml...\); \
	  ${SGMLS} ${SGMLDECL} ../../sgmlspl.sgml \
	    | $(SGMLSPL) ../../${HTMLSPEC} sgmlspl; \
	fi; \
	echo \(Main HTML pass for sgmlspl.sgml...\); \
	${SGMLS} ${SGMLDECL} ../../sgmlspl.sgml \
	  | $(SGMLSPL) ../../${HTMLSPEC} sgmlspl;

clean:
	rm -f *~ core LaTeX/* HTML/SGMLSpm/* HTML/sgmlspl/* PS/*

