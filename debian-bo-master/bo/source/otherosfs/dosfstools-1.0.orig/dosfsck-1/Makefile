CC=cc
CPP=$(CC) -E
CFLAGS=-Wall -Wno-parentheses -g
LDFLAGS=
OBJS=boot.o check.o common.o dosfsck.o fat.o file.o io.o

dosfsck:	$(OBJS)
		$(CC) -o dosfsck $(LDFLAGS) $(OBJS)

.c.o:
		$(CC) -c $(CFLAGS) $*.c

clean:
		rm -f *.o tmp_make

spotless:	clean
		rm -f dosfsck

dep:
		sed '/\#\#\# Dependencies/q' <Makefile >tmp_make
		$(CPP) $(CFLAGS) -MM *.c >>tmp_make
		mv tmp_make Makefile

### Dependencies
boot.o : boot.c common.h dosfsck.h io.h boot.h 
check.o : check.c common.h dosfsck.h io.h fat.h file.h check.h 
common.o : common.c common.h 
dosfsck.o : dosfsck.c common.h dosfsck.h io.h boot.h fat.h file.h check.h 
fat.o : fat.c common.h dosfsck.h io.h fat.h 
file.o : file.c file.h 
io.o : io.c dosfsck.h common.h io.h 
