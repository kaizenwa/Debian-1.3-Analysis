# Unix makefile for math consistency check

CFLAGS += -g -O3 -Wall

MISC = const.o drand.o

all: mtst

mtst: mtst.o $(MISC) 
	$(CC) $(OPT) -o mtst mtst.o $(MISC) -lc ../*.o

clean:
	rm -f *.o mtst

