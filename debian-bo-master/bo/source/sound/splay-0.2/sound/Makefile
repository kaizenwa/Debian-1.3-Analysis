CC	= gcc
#CC	= pgcc
AR	= ar
RM	= \rm -f

OBJLIB	= mpegsound.a
OBJECTS	= rawplayer.o wavetoraw.o \
	  mpegtable.o huffmantable.o \
	  bitwindow.o huffman.o \
          mpeglayer1.o mpeglayer2.o mpeglayer3.o\
	  filter.o \
	  mpegtoraw.o \
	  fileplayer.o 	

DEBUG	= -g
IFLAGS	= -I../bbitstream/
AFLAGS	= -malign-loops=2 -malign-jumps=2 -malign-functions=2
TFLAGS	= -D_REENTRANT -DPTHREADEDMPEG
TLIBFLAGS = -lpthread
OFLAGS	= -O3 -fomit-frame-pointer -fno-strength-reduce -m486 $(AFLAG)
#OFLAGS   = -O6 -mpentium
WFLAGS	= -Wstrict-prototypes -Wall
CFLAGS	= $(DEBUG) $(IFLAGS) $(OFLAGS) $(WFLAGS) $(TFLAGS)
LIBS	= $(OBJLIB) -lm $(TLIBFLAGS) ../bbitstream/bbitstream.a

all: $(OBJLIB) test

clean:
	$(RM) *.a test *.o *~

check: all
	./test f.mp2

test: $(OBJLIB) test.cc
	$(CC) $(CFLAGS) -o test test.cc $(LIBS)

$(OBJLIB): $(OBJECTS)
	$(RM) $(OBJLIB)
	$(AR) rcs $(OBJLIB) $(OBJECTS)

.cc.o:
	$(CC) -c $(CFLAGS) -o $*.o $<

rawplayer.o	: mpegsound.h rawplayer.cc
wavetoraw.o	: mpegsound.h wavetoraw.cc
fileplayer.o	: mpegsound.h fileplayer.cc

filter.o	: mpegsound.h filter.cc
mpeglayer1.o	: mpegsound.h mpeglayer1.cc
mpeglayer2.o	: mpegsound.h mpeglayer2.cc
mpeglayer3.o	: mpegsound.h mpeglayer3.cc
mpegtable.o	: mpegsound.h mpegtable.cc
mpegtoraw.o	: mpegsound.h mpegtoraw.cc

bitwindow.o	: mpegsound.h bitwindow.cc
huffman.o	: mpegsound.h huffman.cc
huffmantable.o	: mpegsound.h huffmantable.cc