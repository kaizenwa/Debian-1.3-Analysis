SHELL=/bin/sh

all: guide.ps essential.ps

guide.ps:	guide.dvi
	dvips -o guide.ps guide

essential.ps:	essential.dvi
	dvips -o essential.ps essential

clean:
	rm -f guide.ps essential.ps

realclean: clean

.PHONY: all clean realclean
