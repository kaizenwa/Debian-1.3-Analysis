CC=gcc
CFLAGS=-O2
LDFLAGS=-s

SPLIT_OBJECTS= floppy_split.o

all: floppy_split
	(cd busybox;make)

clean:
	- rm -f *.o *.bak
	(cd busybox;make clean)

distclean: clean
	- rm -f floppy_split *.o *.bak
	(cd busybox; make distclean)

floppy_split: $(SPLIT_OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $(SPLIT_OBJECTS)

$(SPLIT_OBJECTS): floppy_split.h
