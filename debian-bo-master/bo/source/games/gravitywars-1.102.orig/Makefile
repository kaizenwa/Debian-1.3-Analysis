#
# Makefile for Gravity Wars, by Sami Niemi
#

BINDIR=/usr/local/bin

CFLAGS=-O2 -s  
CC=cc
LIBS=-lm -lvgagl -lvga
TARGET=GravityWars101
OBJECTS=memory.o macros.o misc.o water.o score.o hole.o bullet.o pixel.o \
        blocks.o introutro.o animate.o init.o makepic.o scroll.o moveship.o \
        control.o GravityWars101.o 

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) $(OBJECTS) $(OBJECTS2) -o $(TARGET) $(LIBS)

GravityWars101.o: GravityWars101.c config.h macros.h

memory.o: memory.c config.h macros.h

macros.o: config.h memory.o macros.c macros.h

misc.o: misc.c config.h macros.h

water.o: water.c config.h macros.h

score.o: score.c config.h macros.h

hole.o: hole.c config.h macros.h

bullet.o: bullet.c config.h macros.h

pixel.o: pixel.c config.h macros.h

blocks.o: blocks.c config.h macros.h

introutro.o: introutro.c config.h macros.h

animate.o: animate.c config.h macros.h

init.o: init.c config.h macros.h

makepic.o: makepic.c config.h macros.h

scroll.o: scroll.c config.h macros.h   

moveship.o: moveship.c config.h macros.h

control.o: control.c config.h macros.h


clean:
	rm -f $(OBJECTS)
	rm -f $(TARGET)

install:
	do_install
