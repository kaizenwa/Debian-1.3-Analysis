#include ../Makeconfig
#
#include ../Makerules

CC = gcc -ansi -pedantic # -fbounds-checking

ifndef WFLAGS
WFLAGS = -Wall -Wpointer-arith -Wwrite-strings -Wstrict-prototypes -Wnested-externs
endif

ifndef XCFLAGS
XCFLAGS = -D_POSIX_SOURCE -D_BSD_SOURCE
endif

ifndef OPTFLAGS
OPTFLAGS = -O2 -fno-strength-reduce
endif

INC = -I.

CFLAGS = $(WFLAGS) $(OPTFLAGS) $(INC) $(XCFLAGS)

OBJS = daemon.o errorlog.o lockreg.o \
	locktest.o openmax.o readn.o \
	slave.o writen.o ypbind.o 

PROG = ypbind

all: $(PROG)

ypbind: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

install: $(PROG)
	install -c -o root -g root -m 0755 -s $(PROG) $(USRSBINDIR)
	install -c -o root -g root -m 0644 $(PROG).man ${DEBDIR}/usr/man/man8/$(PROG).8

clean:
	$(RM) -f core *.o *.a *~ $(PROG)

depend: 
	$(CC) -MM $(CFLAGS) *.c > .depend

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
