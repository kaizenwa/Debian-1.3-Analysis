.PHONY: all install clean

INSTALL		= /usr/bin/install
DESTDIR		=
CFLAGS		= -O2 -fomit-frame-pointer -I/usr/src/linux/include
CC		= gcc

all: iproute

install: iproute
	$(INSTALL) iproute $(DESTDIR)/sbin

clean:
	rm -f iproute iproute.c iproute.o

iproute: iproute.o

iproute.c: iproute-2.1.17-delta-x.c.gz
	zcat $< >$@
