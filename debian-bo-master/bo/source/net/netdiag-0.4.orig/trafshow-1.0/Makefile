#	Makefile for trafshow program.

DEFS	=

ARCH    = # -bi486-linuxaout  

CC	= gcc
# LD	= ld # Not used
CFLAGS  = -O6  -Wall
LDFLAGS = # -s
LIBS	= -lncurses

ifdef PROF
CFLAGS  = -O6 -pg -Wall
LDFLAGS = -pg
LIBS	= -lncurses -lgcc -lc -lgmon -lgcc
endif

ifdef DEBUG
CFLAGS  = -O6 -fomit-frame-pointer -Wall -g
LDFLAGS = -g
DEFS	= -DDEBUG
endif

PROGS	= trafshow
INCS	= trafshow.h
SRCS 	= trafshow.c csum.c host.c table.c screen.c
OBJS    = trafshow.o csum.o host.o table.o screen.o
DIST    = Makefile $(SRCS) $(INCS)

all:	$(PROGS)

.c.o:   
	$(CC) $(ARCH) $(CFLAGS) $(DEFS) -c $<
	
trafshow: $(OBJS)
	$(CC) $(ARCH) $(LDFLAGS) -o $@ $^ $(LIBS)

tar:	$(DIST)
	tar -cvf - $^ | gzip -9c > packet.tar.gz

co:
	co -l $(DIST)

ci:
	ci $(DIST)

install: all
	install -o root -g root -s -m 4711 trafshow /usr/local/bin

dep depend : $(SRCS) $(INCS) 
	gcc -M $(SRCS) > .depend

clean:
	rm -f $(PROGS) *.o *~ gmon.out

ifeq (.depend,$(wildcard .depend))
include .depend
endif
