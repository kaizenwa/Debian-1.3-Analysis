# Makefile for uutraf  - an UUCP traffic analyzer
#
# $Id: Makefile,v 1.5 1996/08/03 18:21:53 david Exp david $
# $Log: Makefile,v $
# Revision 1.5  1996/08/03 18:21:53  david
# Added VERSION defines, prefix is now empty
#
# Revision 1.4  1996/05/22 21:37:40  david
# uutraf.cf lives now in /etc/uucp.
#
# Revision 1.4  1996/05/22 21:08:50  david
# moved uutraf.cf from /etc into /etc/uucp.
#
# Revision 1.3  1996/05/11 10:59:48  david
# uutraf.cf lives now in /etc
#
# Revision 1.2  1996/04/13 21:52:58  david
# added debian.* and configuration files into dependency list.
#
# Revision 1.1  1996/03/17 16:15:20  david
# Adapted for ELF distribution
#
# Revision 1.0  1996/01/01 20:30:18  david
# Initial revision
#
RM      = rm -f
CC      = gcc

# If you want to obtain news articles statistics by default, uncomment the
# following line.
DEFS 	= -DDEFAULT_NEWSSTATS
 
# If you do *not* want to have each transfer rounded up to the next kB when
# applying kB fees (ie if you want to apply this fee to the real transfer
# volume), uncomment the following line.
DEFS 	+= -DDEFAULT_PROVFEEPERKBMULT=0
DEFS	+= -DCFGFILE=\"$(LIBDIR)/uutraf.cf\" -DVERSION=\"$(VERSION)\"

CFLAGS  = -O2 -Wall
LIBS    = -lm
LDFLAGS = -s

BIN     = uutraf
MAN1    = $(BIN).1
MAN5    = $(BIN).5
SRCS	= config.c stats.c utils.c
OBJS	= $(SRCS:.c=.o)
HEADER  = $(BIN).h
CFFILE  = uutraf.cf

VERSION = 1.1
prefix  = 
LIBDIR  = $(prefix)/etc/uucp
BINDIR  = $(prefix)/usr/bin
MAN1DIR = $(prefix)/usr/man/man1
MAN5DIR = $(prefix)/usr/man/man5

all:	$(BIN)

source:	$(SRCS) $(HEADER) $(MAN1) $(MAN5) $(CFFILE)
# implicit rules.

$(BIN).c:
	co -kv $@

$(MAN1):
$(MAN5):
	co -kk $(MAN1) $(MAN5)
	mv $(MAN1) $(MAN1).sed
	mv $(MAN5) $(MAN5).sed
	sed "s/\\\$$Date\\\$$/`date +'%B %_d, %Y'`/g" $(MAN1).sed > $(MAN1)
	sed "s/\\\$$Date\\\$$/`date +'%B %_d, %Y'`/g" $(MAN5).sed > $(MAN5)
	-$(RM) $(MAN1).sed $(MAN5).sed
	chmod 644 $(MAN1) $(MAN5)

.c.o:	$(HEADER) $(SRCS) $(OBJS)
	$(CC) $(CFLAGS) $(DEFS) -c -o $@ $<

$(BIN):	$(BIN).c $(HEADER) $(SRCS) $(OBJS) $(MAN)
	$(CC) $(CFLAGS) $(LDFLAGS) $(DEFS) -o $(BIN) $(BIN).c $(OBJS) $(LIBS)

clean:
	$(RM) *.o *~ *.bak \#*\# errors core* a.out TAGS

distclean:	clean
	$(RM) $(BIN)

realclean:	distclean
	-rcsclean
	-$(RM) *.[ch15]

install: 	$(MAN1) $(MAN5) $(BIN) $(CFFILE)
	install -d -g root -o root -m 755 $(BINDIR)
	install -s -g root -o root -m 755 $(BIN)     $(BINDIR)
	install -d -g root -o root -m 644 $(MAN1DIR)
	install    -g root -o root -m 644 $(MAN1)    $(MAN1DIR)
	install -d -g root -o root -m 644 $(MAN5DIR)
	install    -g root -o root -m 644 $(MAN5)    $(MAN5DIR)
	install -d -g root -o root -m 755 $(LIBDIR)
	install    -g root -o root -m 644 $(CFFILE)  $(LIBDIR)/$(CFFILE)
