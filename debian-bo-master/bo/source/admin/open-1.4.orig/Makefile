
CC=gcc
CFLAGS=-O2 -Wall -ansi
LDFLAGS=$(CFLAGS) -s 

SRC1=open.c
OBJ1=open.o
OBJ2=switchto.o
PROG1=open
PROG2=switchto
PROGS=$(PROG1) $(PROG2)
OPEN=open
MAN=open.1 switchto.1
BINDIR=/usr/local/bin
MANDIR=/usr/local/man/man1



all: $(PROGS)


open: $(OBJ1) 
	$(CC) $(LDLAGS) -o $@ $<

switchto: $(OBJ2) 
	$(CC) $(LDLAGS) -o $@ $<

open.o: open.h

clean:
	-rm -f $(OBJ1) $(OBJ2)

realclean: clean
	-rm -f $(PROGS)


install: $(PROGS)
	install -c $(PROGS) $(BINDIR)
	install -c $(MAN)  $(MANDIR)

setuid:
	chmod 4711 $(BINDIR)/$(PROG1)
	chmod 4711 $(BINDIR)/$(PROG2)
