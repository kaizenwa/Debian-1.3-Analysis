SHELL=/bin/sh

subdirs=latex/rotating latex/rotfloat latex/general latex/graphics texdraw latex/psfrag

default: all helpindex

all:
	for d in $(subdirs); do \
	  if test -d $$d; then (cd $$d; $(MAKE) $@); \
	  else true; fi; done

helpindex:
	./mkhtml > helpindex.html

clean:
	for d in $(subdirs); do \
	  if test -d $$d; then (cd $$d; $(MAKE) $@); \
	  else true; fi; done

realclean:
	for d in $(subdirs); do \
	  if test -d $$d; then (cd $$d; $(MAKE) $@); \
	  else true; fi; done
