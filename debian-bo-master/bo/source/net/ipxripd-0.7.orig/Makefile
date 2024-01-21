
OBJS = ipxd.o ipxripd.o ipxsapd.o ipxsap.o ipxrip.o ipxkern.o ipxutil.o

CFLAGS = -Wall -O2 -g

all: ipxd

dep:
	gcc -M *.c >.depend

install: all
	install --strip ipxd -m 755 /usr/sbin
	install ipxd.8 -m 755 /usr/man/man8
	install ipx_ticks.5 -m 755 /usr/man/man5

release: 
	mkdir ../release/router
	ln *.c makefile ../release/router

ipxd: $(OBJS)
	$(CC) -o $@ $(OBJS) $(LFLAGS)

clean: 
	rm -f *.o *.a *~ *.bak ipxd .depend

realclean: clean
	rm -f *.tgz


SRCPATH=$(shell pwd)
SRCDIR=$(shell basename $(SRCPATH))
DISTFILE=$(SRCDIR).tgz

dist: tgz
	make dep
	make all

tgz: realclean
	(cd ..; \
         tar cvf - $(SRCDIR) | \
            gzip -9 > $(DISTFILE); \
         mv $(DISTFILE) $(SRCDIR))

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
