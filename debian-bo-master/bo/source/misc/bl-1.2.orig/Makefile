CFLAGS=-m486 -O2 -Wall -s -N
LDFLAGS=
CC=gcc
BINDIR=/usr/local/bin
MANDIR=/usr/local/man/man1

all: bl 

bl: bl.c 
	$(CC) $(CFLAGS) bl.c -o bl

man:
	tbl bl.1 | nroff -mandoc | col | more

clean: 
	rm -f bl bl.o

install: bl
	install -c bl $(BINDIR)
	install -c bl.1 $(MANDIR)

