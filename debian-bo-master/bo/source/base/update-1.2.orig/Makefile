
CC = gcc

DEFS =
#DEFS = -DNO_BDFLUSH
CFLAGS = $(DEFS) -O3 -fno-strength-reduce -fomit-frame-pointer -Wall

BINDIR = /sbin
MANDIR = /usr/man/man8


update: update.o

install: update
	cp -f update $(BINDIR)/update
	strip $(BINDIR)/update
	if [ -d $(MANDIR) ] ; then cp update.8 $(MANDIR) ; fi

clean:
	rm -f update core *.o *~

# depencies
update.o: update.c
