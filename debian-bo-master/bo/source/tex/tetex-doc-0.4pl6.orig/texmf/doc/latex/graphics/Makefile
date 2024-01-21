SHELL=/bin/sh

all: grfguide.ps

grfguide.ps:	grfguide.dvi
	dvips -o grfguide.ps grfguide

clean:
	rm -f grfguide.ps

realclean: clean
