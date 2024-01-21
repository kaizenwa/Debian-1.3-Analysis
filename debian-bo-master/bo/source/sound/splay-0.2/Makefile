CC	= gcc
#CC	= pgcc
AR	= ar

DEBUG	= -g
IFLAGS	= -I./bbitstream -I./sound
WFLAGS	= -Wstrict-prototypes -Wall
OFLAGS	= -O2 -m486
#OFLAGS	= -O6 -mpentium
TFLAGS	= -D_REENTRANT -DPTHREADEDMPEG
TLIBFLAGS = -lpthread
CFLAGS	= $(OFLAGS) $(WFLAGS) $(IFLAGS) $(DEBUG) $(TFLAGS)
OBJECTS	= splay.o
LIBRARY	= -lm $(TLIBFLAGS) \
	  ./bbitstream/bbitstream.a ./sound/mpegsound.a \
	  ./bbitstream/bbitstream.a ./sound/mpegsound.a

all : splay

clean :
	\rm -f splay *.o *~
	cd bbitstream; make clean
	cd sound;      make clean

splay : bbitstream_lib sound_lib $(OBJECTS)
	$(CC) -o splay $(OBJECTS) $(LIBRARY) 

.cc.o :
	$(CC) -c $(CFLAGS) -o $*.o $<

splay.o		: splay.cc
bbitstream_lib :
	cd bbitstream; make
sound_lib :
	cd sound; make
