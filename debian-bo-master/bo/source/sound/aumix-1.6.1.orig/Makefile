CC      = gcc
#
# Set the CFLAGS environment variable if you want optimization.
# Comment out -DGPM if you don't want mouse support.
# Comment out -DNCURSES if you don't want interactive capability.
OPTS    = -s -Wall -DNCURSES -DGPM
# Set the CFLAGS environment variable if you want optimization.
#CFLAGS = 
#
# Comment out -lgpm below if you don't want mouse support.
# Comment out -lncurses if you don't want interactive capability.
LIBS    = -lncurses -lgpm
INSTALL = install
BINDIR  = /usr/local/bin
MANDIR = /usr/local/man/man1

all: aumix

aumix: aumix.c
	$(CC) -o aumix aumix.c $(LIBS) $(OPTS) $(CFLAGS)

install: all
	$(INSTALL) -m 755 -o root -g root aumix $(BINDIR)
	$(INSTALL) -m 644 -o root -g root aumix.1.gz $(MANDIR)

clean:
	rm -f aumix
