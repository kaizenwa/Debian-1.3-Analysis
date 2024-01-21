
include	../Makefile.config

DIRS=	admin server
#DIRS=	admin client server

all:
	for a in $(LIB) $(DIRS); do \
		( cd $$a; echo $@: `pwd`; $(MAKE) $@ ); \
	done

clean:
	for a in $(LIB) $(DIRS); do \
		( cd $$a; echo $@: `pwd`; $(MAKE) $@ ); \
	done

install:
	for a in $(LIB) $(DIRS); do \
		( cd $$a; echo $@: `pwd`; $(MAKE) $@ ); \
	done
