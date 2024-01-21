# Makefile for rpncalc - a rudimentary emulation of a HP28S
#
# $Id: Makefile,v 1.12 1997/02/01 14:30:11 david Exp $

RM      = rm -f
CC      = gcc

LINT	= lclint
LINTOPTS= -exitarg -unrecog -realcompare -predboolint -boolops +boolint\
 -nullassign +charintliteral -mustfree

# Define -Dsunos4 or -Dsunos5 if you're unfortunate enough
# to run SunOs 4.1 or SunOs 5.5 (aka Solaris 2.5).
# Define -Dfreebsd if you're running FreeBSD.
DEFS	= -DVERSION=\"1.15\"

# Define -DHAVE_READLINE if you want readline support (strongly suggested)
# and have the readline libraries installed (most commercial Unices probably
# don't, though; Linux and FreeBSD almost certainly have them installed).
DEFS	+= -DHAVE_READLINE

CFLAGS  = -O2 -Wall -Wstrict-prototypes -Wmissing-prototypes -Wshadow\
  -Wpointer-arith -Wbad-function-cast -Wcast-qual -Wcast-align\
  -Wwrite-strings -Wmissing-declarations -Wnested-externs
LIBS    = -lm

#if defined(HAVE_READLINE)
LIBS   += -lreadline -lncurses
#endif
LDFLAGS =

BIN     = rpncalc
MAN     = $(BIN).1
MANTEMPL= $(MAN).sed
SRCS    = cmds.c stack.c utils.c
OBJS	= cmds.o stack.o utils.o
HEADER  = $(BIN).h cmds.h utils.h stack.h

prefix  = /usr
BINDIR  = $(prefix)/bin
MANDIR  = $(prefix)/man/man1

all:	rpncalc $(MAN)

$(BIN).c:
debian.rules:
	co -kv $@

$(MAN): $(BIN) proto.ed
	if [ \! -r $(MANTEMPL) ]; then co -kk $(MANTEMPL); fi
	sed -e "s/\\\$$Date.*\\\$$/`date +'%B %-d, %Y'`"/g $(MANTEMPL) > $(MAN)
	ed -s - $(MAN) < proto.ed
	chmod 644 $(MAN)

.c.o:	$(HEADER) $(SRCS)
		$(CC) $(CFLAGS) $(DEFS) -c -o $@ $<

rpncalc:	$(HEADER) $(SRCS) $(OBJS) $(BIN).c
	$(CC) $(CFLAGS) $(LDFLAGS) $(DEFS) -o $(BIN) $(BIN).c $(OBJS) $(LIBS)

clean:
	$(RM) *.o *~ *.bak \#*\# errors core* a.out TAGS

distclean:	clean
	$(RM) $(BIN)

realclean:	distclean
	-rcsclean
	-$(RM) cmds.* rpncalc.*

install: 	$(MAN) $(BIN)
	install -d -g root -o root -m 755 $(BINDIR)
	install    -g root -o root -m 755 -s $(BIN) $(BINDIR)
	install -d -g root -o root -m 755 $(MANDIR)
	install    -g root -o root -m 644 $(MAN) $(MANDIR)

lint:	$(SRCS) $(BIN).c
	$(LINT) $(LINTOPTS) $^
