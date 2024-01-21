# Makefile for xsok-1.00

XSOKZIP = gzip -f -9

TARGETS = Sokoban.def.gz Xsok.def.gz Cyberbo.def.gz \
	floor.xpm.gz objects.xpm.gz

all: $(TARGETS)

Sokoban.def.gz: Sokoban
	../src/combine -s Sokoban && $(XSOKZIP) Sokoban.def

# truncate filename to 14 chars
Cyberbo.def.gz: Cyberbox
	../src/combine -s Cyberbox && mv Cyberbox.def Cyberbo.def \
	&& $(XSOKZIP) Cyberbo.def

Xsok.def.gz: Xsok
	../src/combine -s Xsok && $(XSOKZIP) Xsok.def

floor.xpm.gz: floor.xpm
	$(XSOKZIP) -c floor.xpm > floor.xpm.gz

objects.xpm.gz: objects.xpm
	$(XSOKZIP) -c objects.xpm > objects.xpm.gz

# clean target:
clean:
	rm -f *~ */*~ $(TARGETS)
