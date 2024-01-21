SHELL=/bin/sh

all: texdraw.ps txdexamp.ps

texdraw.ps: texdraw.dvi
	dvips -o texdraw.ps texdraw

txdexamp.dvi: txdexamp.latex
	latex $<

texdraw.dvi: texdraw.texi
	texi2dvi $<

txdexamp.ps: txdexamp.dvi
	dvips -o txdexamp.ps txdexamp

clean:
	rm -f texdraw.cps texdraw.fns texdraw.vr texdraw.tp texdraw.ky texdraw.pg texdraw.ps1 texdraw.ps2 texdraw.ps3 texdraw.ps4 texdraw.ps5 texdraw.ps6 texdraw.ps7 texdraw.ps8 texdraw.ps9 texdraw.p10 texdraw.p11 texdraw.p12 texdraw.p13 texdraw.toc texdraw.log texdraw.dvi texdraw.aux texdraw.cp texdraw.fn txdexamp.ps1 txdexamp.ps2 txdexamp.ps3 txdexamp.ps4 txdexamp.ps5 txdexamp.ps6 txdexamp.ps7 txdexamp.ps8 txdexamp.ps9 txdexamp.p10 txdexamp.p11 txdexamp.p12 txdexamp.log txdexamp.aux txdexamp.dvi txdexamp.p13

realclean:
	rm -f txdexamp.ps texdraw.ps
