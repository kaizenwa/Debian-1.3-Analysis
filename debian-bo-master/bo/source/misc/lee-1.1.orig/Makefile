# Note:	add floating point option if appropriate
#	-lm for the math libraries
#	-O  for optimization
#	-g  for debugging (dbx)
#	-p  for profiling (prof)
CFLAGS = -Ae +O3 +z
LIBS = -L. -llee -lm
CC = cc

LEE = 	globals.o \
	include.o \
	io.o \
	net.o \
	populati.o \
	world.o \
	body.o \
	interact.o \
	stats.o

headers = Makefile defs.h 

all: archive shared main

archive : $(LEE)
	rm -f liblee.a
	ar cr liblee.a $(LEE)

shared : $(LEE)
	rm -f liblee.sl
	ld -b -s -o liblee.sl $(LEE)

main : main.o
	cc $(CFLAGS) -o lee main.o $(LIBS)

table: defs.h table.o
	cc $(CFLAGS) -o table table.o 

config: defs.h config.o
	cc $(CFLAGS) -o config config.o

$(LEE) : $(headers)

clean :
	rm -f *.o liblee.sl liblee.a table config lee
