SHELL=/bin/sh

all:	examples.dvi examples.ps

examples.ps:	examples.dvi
	dvips -o examples.ps examples

examples.dvi:
	latex examples

clean:
	rm -f examples.ps *.tmp *.log *.aux

realclean: clean
