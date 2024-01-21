include ../MCONFIG

INC=		-I../compat/include -I../dump
CFLAGS=		-pipe $(DEFS) $(INC) $(GINC) $(OPT)
SRCS=		dumprmt.c
OBJS=		dumprmt.o

all:		$(OBJS)

clean:
	rm -f \#* *.s *.o *.a *~ core

realclean:	clean
	rm -f .depend 

install::

dep depend .depend:
	$(CPP) $(CFLAGS) -M *.c >.depend

include .depend
