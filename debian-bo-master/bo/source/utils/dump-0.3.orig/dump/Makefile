include ../MCONFIG

#DEBUG=		-DFDEBUG -DTDEBUG -DWRITEDEBUG
INC=		-I../compat/include
CFLAGS=		-pipe $(DEFS) $(DEBUG) $(INC) $(GINC) $(OPT)
DEPLIBS=	../compat/lib/libcompat.a
LIBS=		$(GLIBS) -L../compat/lib -lcompat

ifdef STATIC_LINK_DUMP
LDFLAGS:=	$(LDFLAGS) -static
endif

PROG=		dump
LINKS=		dump rdump
SRCS=		itime.c main.c optr.c tape.c traverse.c unctime.c
OBJS=		itime.o main.o optr.o tape.o traverse.o unctime.o \
		../common/dumprmt.o
MAN8=		dump.8
MLINKS=		dump.8 rdump.8

all:		$(PROG)

$(PROG):	$(OBJS) $(DEPLIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(PROG) $(OBJS) $(LIBS)

install:	$(PROG)
	install -o $(BINOWN) -g $(BINGRP) -m $(BINMODE) $(PROG) $(BINDIR)
	install -o $(MANOWN) -g $(MANGRP) -m $(MANMODE) $(MAN8) $(MANDIR)
#	@set $(LINKS) $(MLINKS); \
#	while test $$# -ge 2; do \
#		l=$$1; \
#		shift; \
#		t=$$1; \
#		shift; \
#		echo $$t -\> $$l; \
#		rm -f $$t; \
#		ln -s $$l $$t; \
#	done; true

clean:
	rm -f $(PROG) \#* *.s *.o *.a *~ core

realclean:	clean
	rm -f .depend 

dep depend .depend:
	$(CPP) $(CFLAGS) -M *.c >.depend

include .depend
