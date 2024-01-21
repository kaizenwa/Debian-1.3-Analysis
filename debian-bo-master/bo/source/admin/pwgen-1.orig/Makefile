# Makefile for pwgen.

# Compile-time program options
OPTS	= -DCAPITALIZE -DALTPHON -DNUMERALS -DRAND48

# Standard items
CC	= cc
CFLAGS	= -g
LD	= cc
#LDFLAGS	= -g
LIBS	= -lm
INSTALL	= cp
INFLAGS	=

# Where to install
DESTDIR	= /usr/local/bin
MANDIR	= /usr/local/man/man1

# End configuration section

all: pwgen

test: pwgen
	./pwgen 8 20

install: all
	$(INSTALL) pwgen $(DESTDIR)
	$(INSTALL) pwgen.1 $(MANDIR)
	chmod 555 $(DESTDIR)/pwgen
	chmod 444 $(MANDIR)/pwgen.1

pwgen:	pwgen.o
	$(LD) $(LDFLAGS) -o pwgen pwgen.o $(LIBS)

pwgen.o: pwgen.c
	$(CC) $(CFLAGS) $(OPTS) -c -o pwgen.o pwgen.c

clean:
	rm -f core a.out pwgen *.o *.old *~

tar:	clean
	cd ..; tar cvf pwgen.tar pwgen

dist:	clean
	cd ..; shar pwgen pwgen/* >pwgen.shar
