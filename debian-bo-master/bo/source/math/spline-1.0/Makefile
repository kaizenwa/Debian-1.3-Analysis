# Makefile for spline
#
# $Id: Makefile,v 1.3 1996/09/29 21:10:08 david Exp $
# $Log: Makefile,v $
# Revision 1.3  1996/09/29 21:10:08  david
# Removed debian.* dependencies for new debian source format.
#
# Revision 1.2  1996/03/16 15:40:42  david
# Install strips binary when installing.
#
# Revision 1.1  1996/02/11 22:11:44  david
# Initial revision

RM      = rm -f

CC      = gcc
CFLAGS  = -O2 -Wall
LIBS    = -lm
LDFLAGS = 

BIN     = spline
MAN     = $(BIN).1
SRCS    = utils.c
OBJS	= utils.o
HEADER  = utils.h

prefix  = /usr
BINDIR  = $(prefix)/bin
MANDIR  = $(prefix)/man/man1

all:	$(BIN)

$(BIN).c:
	co -kv $@

$(MAN):
	co -kk $(MAN)
	mv $(MAN) $(MAN).sed
	sed "s/\\\$$Date\\\$$/`date +'%B %d, %Y'`/g" $(MAN).sed > $(MAN)
	-$(RM) $(MAN).sed
	chmod 644 $(MAN)

.c.o:	$(HEADER) $(SRCS) $(OBJS)
	$(CC) $(CFLAGS) $(DEFS) -c -o $@ $<

$(BIN):	$(HEADER) $(SRCS) $(OBJS) $(BIN).c $(MAN)
	$(CC) $(CFLAGS) $(LDFLAGS) $(DEFS) -o $(BIN) $(BIN).c $(OBJS) $(LIBS)

clean:
	$(RM) *.o *~ *.bak \#*\# errors core* a.out TAGS

distclean:	clean
	$(RM) $(BIN)

realclean:	distclean
	-rcsclean
	-$(RM) cmds.* $(BIN).*

install: 	$(MAN) $(BIN)
	install -d -g root -o root -m 755 $(BINDIR)
	install -s -g root -o root -m 755 $(BIN) $(BINDIR)
	install -d -g root -o root -m 755 $(MANDIR)
	install    -g root -o root -m 644 $(MAN) $(MANDIR)
