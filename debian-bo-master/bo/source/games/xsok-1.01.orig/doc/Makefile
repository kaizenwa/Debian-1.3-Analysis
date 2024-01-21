all: xsok.ps

xsok.ps: xsok.dvi
	dvips -f < xsok.dvi > xsok.ps

xsok.dvi: xsok.tex
	latex xsok
	latex xsok
	latex xsok

clean:
	rm -f *~ *.log *.aux *.toc *.dvi *.ps
