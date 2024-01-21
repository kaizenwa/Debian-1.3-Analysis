include ../MCONFIG

INC=		-I../compat/include
CFLAGS=		-pipe $(DEFS) $(INC) $(GINC) $(OPT)
DEPLIBS=	../compat/lib/libcompat.a
LIBS=		$(GLIBS) -le2p -L../compat/lib -lcompat

ifdef STATIC_LINK_RESTORE
LDFLAGS:=	$(LDFLAGS) -static
endif

PROG=		restore
LINKS=		restore rrestore
SRCS=		dirs.c interactive.c main.c restore.c symtab.c tape.c \
		utilities.c
OBJS=		dirs.o interactive.o main.o restore.o symtab.o tape.o \
		utilities.o ../common/dumprmt.o
MAN8=		restore.8
MLINKS=		$(MANDIR)/restore.8 $(MANDIR)/rrestore.8

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
