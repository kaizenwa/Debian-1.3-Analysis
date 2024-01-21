default: denis1.ps denis2.ps pst-test.ps samples.ps chartest.ps rm-junk
.SUFFIXES: .ps .dvi

.tex.dvi:
	tex $<

.dvi.ps:
	dvips $< -o

samples.dvi: samples.tex
	latex samples
	latex samples

clean: rm-junk
	rm -f denis1.ps denis2.ps pst-test.ps samples.ps chartest.ps
	rm -f denis1.dvi denis2.dvi pst-test.dvi samples.dvi chartest.dvi

rm-junk:
	rm -f *.aux *.log
