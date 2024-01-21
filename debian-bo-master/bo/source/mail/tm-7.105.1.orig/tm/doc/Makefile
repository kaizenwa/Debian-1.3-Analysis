#
# $Id: Makefile,v 4.1 1996/10/05 13:24:40 morioka Exp morioka $
#

EMACS=mule
MAKEINFO=$(EMACS) -batch -q -no-site-file $< -l texinfmt -f texinfo-format-buffer -f save-buffer
# MAKEINFO=$(EMACS) -batch -q -no-site-file $< -l texinfmt -f texinfo-every-node-update -f texinfo-format-buffer -f save-buffer
# MAKEINFO=makeinfo $<

TEXI2DVI = texi2dvi $<

TEXFILES= signature-jp.tex

INFO_JA	= tm-ja.info tm-view-ja.info tm-edit-ja.info
INFO_EN	= tm_en.info

DVI_JA	= tm-ja.dvi tm-view-ja.dvi tm-edit-ja.dvi signature-jp.dvi
DVI_EN	= tm_en.dvi

GOMI	= *.aux *.toc *.log \
	*.cp *.cps *.cpss *.fn *.fns *.fnss *.vr *.vrs *.ky *.kys \
	*.pg *.pgs *.tocs *.tp *.tps *.ilg *.ind   \
	$(DVIFILES) *.ps *~


.SUFFIXES:	.oti .texi .info .dvi .ol .tex

.oti.texi:
	$(EMACS) -batch -q -no-site-file $< -l ol2texi.el -f ol-to-texi-buffer -f save-buffer

.texi.info:
	$(MAKEINFO)

.texi.dvi:
	$(TEXI2DVI)

.ol.tex:
	ol2 < $< | plain2 -tex -tstyle=a4j > $@

.tex.dvi:
	jlatex $<
	jlatex $<


all:	world

world:	ja en


ja:	info-ja dvi-ja

en:	info-en info-ja

info:	info-ja info-en

info-ja:	$(INFO_JA)

info-en:	$(INFO_EN)

#tm-v-mode-ja.texi: tm-v-mode-ja.oti

#tm-mh-e_ja.info: tm-mh-e_ja.texi
#	$(EMACS) -batch -q -no-site-file $< -l texinfmt -f texinfo-every-node-update -f texinfo-format-buffer -f save-buffer


tex:	$(TEXFILES)

$(TEXFILES):	$(OLFILES)


dvi:	dvi-ja dvi-en

dvi-ja:	$(DVI_JA)

dvi-en:	$(DVI_EN)


clean:
	rm $(GOMI)
