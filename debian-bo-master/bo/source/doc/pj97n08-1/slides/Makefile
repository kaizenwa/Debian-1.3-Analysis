TARGET=slides
FIGURES=cactus.eps bigcactus.eps queue.eps clones.eps fib.eps
OTHERFIG=the-king-final.eps
PRINTER=ps

.SUFFIXES: .eps .fig

$(TARGET).dvi: $(TARGET).tex $(FIGURES) $(OTHERFIG)
	latex2e $(TARGET)

$(TARGET).ps: $(TARGET).dvi
	dvips -t landscape $(TARGET).dvi -P $(PRINTER) -o $@

.fig.eps:
	fig2dev -L ps $< $@

clean:
	rm -f *.dvi *~ *.log *.aux *.bbl *.blg *.ps *.toc *.lof *.bak nohup.out
	rm -f $(FIGURES)




