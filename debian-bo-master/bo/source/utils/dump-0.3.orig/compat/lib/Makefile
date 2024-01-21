include ../../MCONFIG

INC=		-I../include # -I../../dump
CFLAGS=	       	-pipe $(DEFS) $(INC) $(GINC) $(OPT)
SRCS=		err.c fstab.c glob.c
OBJS=		err.o fstab.o glob.o
LIB=		libcompat.a

all:		$(LIB)

$(LIB):		$(OBJS)
	$(ARCHIVE) $(LIB) $(OBJS)
	$(RANLIB) $(LIB)

clean:
	rm -f \#* *.s *.o *.a *~ core

realclean:	clean
	rm -f .depend 

install::

dep depend .depend:
	$(CPP) $(CFLAGS) -M *.c >.depend

include .depend
