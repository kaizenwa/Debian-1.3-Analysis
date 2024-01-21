TCL = /usr/bin/tclsh
LIB = /usr/local/lib/zircon
SYS = Solaris
BINDIR = /usr/bin
CC = gcc

all:
	$(MAKE) -f Makefile.$(SYS) WISH=$(TCL) LIBDIR=$(LIB) BINDIR=$(BINDIR) CC=$(CC) all

install:
	$(MAKE) -f Makefile.$(SYS) WISH=$(TCL) LIBDIR=$(LIB) BINDIR=$(BINDIR) CC=$(CC) install

