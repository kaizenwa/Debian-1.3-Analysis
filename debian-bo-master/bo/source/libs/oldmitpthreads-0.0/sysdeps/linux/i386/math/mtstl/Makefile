# Unix makefile for math consistency check

CFLAGS += -g -O3 -Wall

MISC = ldrand.o

all: mtstl

mtstl: mtstl.o $(MISC) 
	$(CC) $(OPT) -o mtstl mtstl.o $(MISC) -lc ../*.o

clean:
	rm -f *.o mtstl

