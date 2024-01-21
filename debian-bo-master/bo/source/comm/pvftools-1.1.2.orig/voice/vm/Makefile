#
# Makefile
#
# This is the Makefile for libvoice.
#
# $Id: Makefile,v 1.6 1997/02/20 06:52:07 marc Exp $
#

VMOBJS=main.o event.o usage.o

VOICELIBS=$(MGETTYLIB) $(LIBVOICE) $(LIBUTIL)

.c.o:
	$(CC) $(CFLAGS) -c $*.c

all: $(VMOBJS) vm

install:

clean:
	@$(RM) -f vm $(VMOBJS)

fullclean: clean

main.o: main.c
	$(CC) $(CFLAGS) -DMAIN -c main.c

vm: $(VMOBJS) $(VOICELIBS)
	$(CC) $(LDFLAGS) -o vm $(VMOBJS) $(VOICELIBS) $(LIBS)

include depend
