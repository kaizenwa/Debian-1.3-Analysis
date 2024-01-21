DIRS=	login-sh login-ts ftpd

include ../../Makefile.config

all:
	for a in $(DIRS); do \
		( cd $$a; echo $@: `pwd`; $(MAKE) $@ ); \
	done

clean:
	for a in $(DIRS); do \
		( cd $$a; echo $@: `pwd`; $(MAKE) $@ ); \
	done

install:
	for a in $(DIRS); do \
		( cd $$a; echo $@: `pwd`; $(MAKE) $@ ); \
	done
