CC	= gcc
#CC	= pgcc
AR	= ar
RM	= \rm -f

OBJLIB	= bbitstream.a
OBJECTS	= input.o output.o

DEBUG	= -g
IFLAGS	=
WFLAGS	= -Wstrict-prototypes -Wall
OFLAGS	= -O3 -fomit-frame-pointer -fno-strength-reduce -m486
#OFLAGS	= -O6 -mpentium
CFLAGS	= $(DEBUG) $(IFLAGS) $(WFLAGS) $(OFLAGS)
LIBS	= $(OBJLIB)

all: $(OBJLIB)

clean:
	$(RM) *.o *.a *~

$(OBJLIB): $(OBJECTS)
	$(RM) $(OBJLIB)
	$(AR) rcs $(OBJLIB) $(OBJECTS)

.cc.o :
	$(CC) -c $(CFLAGS) -o $*.o $<

input.o  : bbitstream.h input.cc
output.o : bbitstream.h output.cc
