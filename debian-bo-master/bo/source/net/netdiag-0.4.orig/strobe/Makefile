# Strobe v0.92 (c) 1994,1995 Julian Assange, All rights reserved.
# (proff@suburbia.apana.org.au || proff@four.net || proff@gnu.ai.mit.edu)

# Linux modifications: A. Schiffler, andreas@karlsberg.usask.ca

INSTALLDIR=/usr/bin
LIBDIR=/usr/lib
MANDIR=/usr/man/man1

CC=gcc
FLAGS=-O
ETC_SERVICES=/etc/services

# Don't change anything from this point on.

OBJS=strobe.o
BIN=strobe
MAN=strobe.1
DATA=strobe.services
EXTRA=Makefile INSTALL VERSION

DEFS=-DLIB_STROBE_SERVICES='"$(LIBDIR)/$(DATA)"'\
     -DSTROBE_SERVICES='"$(DATA)"'\
     -DETC_SERVICES='"$(ETC_SERVICES)"'

CFLAGS=$(FLAGS) $(DEFS)

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJS) $(LIBS)
$(OBJS):
$(DATA):
$(MAN):
$(INSTALLDIR)/$(BIN): $(BIN)
	-rm -f $(INSTALLDIR)/$(BIN)
	install -m 755 -s $(BIN) $(INSTALLDIR)/$(BIN)
$(LIBDIR)/$(DATA): $(DATA)
	-rm -f $(LIBDIR)/$(DATA)
	install -m 644 $(DATA) $(LIBDIR)/$(DATA)
$(MANDIR)/$(MAN): $(MAN)
	-rm -f $(MANDIR)/$(MAN)
	install -m 644 $(MAN) $(MANDIR)/$(MAN)
install: $(INSTALLDIR)/$(BIN) $(LIBDIR)/$(DATA) $(MANDIR)/$(MAN)
tar:
	(cd .. ; tar -zcf strobe.tgz strobe)
shar:
	shar > strobe.shar *.c *.1 $(EXTRA) $(DATA)
clean:
	rm -f $(BIN) *.o *.bak *.\~ errlist *.shar *.tgz
