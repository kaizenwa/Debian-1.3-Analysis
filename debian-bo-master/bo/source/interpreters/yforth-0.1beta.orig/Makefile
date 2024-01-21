OPTIMIZE = -O2
CC = gcc
MATHLIB = -lm

OBJECTS = block.o blocke.o core.o coree.o double.o doublee.o exceptio.o \
	facility.o file.o filee.o float.o floate.o locals.o localse.o \
	memall.o search.o searche.o string.o tools.o toolse.o \
	udio.o vm.o ycore.o yfinit.o yforth.o yfvinit.o  

INCLUDES = block.h blocke.h config.h core.h coree.h defaults.h double.h \
	doublee.h errors.h exceptio.h facility.h file.h filee.h float.h \
	floate.h locals.h localse.h macro.h memall.h search.h searche.h \
	string.h tools.h toolse.h udio.h ver.h ycore.h yforth.h 

all:
	make div
	make yforth

yforth: $(OBJECTS)
	$(CC) $(MATHLIB) -o yforth $(OBJECTS)

div: division.c
	$(CC) -o div division.c
	div > div.h 

.c.o: 	 
	$(CC) -c -o $@ $(OPTIMIZE) $<

clean:
	rm -f *.o yforth div.h div 
