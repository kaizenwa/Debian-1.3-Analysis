include ../MCONFIG

#DEBUG=		-DFDEBUG -DTDEBUG -DWRITEDEBUG
INC=		-I../compat/include
CFLAGS=		-pipe $(DEFS) $(DEBUG) $(INC) $(GINC) $(OPT)
DEPLIBS=	../compat/lib/libcompat.a
LIBS=		$(GLIBS) -L../compat/lib -lcompat

PROG=		rmt
SRCS=		rmt.c
OBJS=		rmt.o
MAN8=		rmt.8
BINMODE=	0755

all:		$(PROG)

$(PROG):	$(OBJS) $(DEPLIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(PROG) $(OBJS) $(LIBS)

install:	$(PROG)
	install -o $(BINOWN) -g $(BINGRP) -m $(BINMODE) $(PROG) $(BINDIR)
	install -o $(MANOWN) -g $(MANGRP) -m $(MANMODE) $(MAN8) $(MANDIR)

clean:
	rm -f $(PROG) \#* *.s *.o *.a *~ core

realclean:	clean
	rm -f .depend 

dep depend .depend:
	$(CPP) $(CFLAGS) -M *.c >.depend

include .depend
