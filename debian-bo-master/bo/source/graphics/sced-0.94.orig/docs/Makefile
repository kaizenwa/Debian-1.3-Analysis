all: elk_guide.ps guide.ps

view: guide.ps
	ghostview $< &

text:
	nroff -mm $< > $@

guide.ps: guide.mm
	gtroff -mm $< | grops > $@

elk_guide.ps: elk_guide.mm
	gtroff -mm $< | grops > $@

