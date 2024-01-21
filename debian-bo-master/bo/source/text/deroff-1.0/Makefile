# Makefile for deroff
# $Id: Makefile,v 1.1 1996/12/27 23:21:22 david Rel david $

prefix=/usr
LEXSRC=deroff.l
SRC=$(LEXSRC:.l=.c)
BIN=$(LEXSRC:.l=)
MAN=$(LEXSRC:.l=.1)

LEXFLAGS=-8
CCFLAGS=-Wall -DHAVE_GETOPT_LONG -O2

all:	$(BIN)

$(SRC): $(LEXSRC)
	$(LEX) $(LEXFLAGS) -t $^ > $@

$(BIN): $(SRC)
	$(CC) $(CCFLAGS) -o $@ $^ $(LDFLAGS)

install: $(MAN)
	install -s -o root -g root -m 755 $(BIN) $(prefix)/bin
	install    -o root -g root -m 644 $(MAN) $(prefix)/man/man1

clean:
	$(RM) *.o $(SRC) $(BIN)

distclean: clean
	$(RM) $(BIN)
