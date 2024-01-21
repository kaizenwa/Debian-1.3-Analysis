CC	= gcc
LD	= gcc

# for debug
#CFLAGS	= -Wall -g
#LDFLAGS = -N

# for production code
CFLAGS	= -Wall -O3 -fomit-frame-pointer
LDFLAGS = -s -N

statserial:	statserial.o
	$(LD) $(LDFLAGS) -o statserial statserial.o -lcurses

statserial.o: statserial.c
	$(CC) $(CFLAGS) -c statserial.c

install: statserial
	install -m 555 statserial /usr/local/bin/statserial
	install -m 444 statserial.1 /usr/local/man/man1/statserial.1

clean:
	$(RM) statserial statserial.o core *~

dist:	statserial.c Makefile statserial.1 README
	$(RM) statserial statserial.o core statserial-1.1.tar.gz *~
	cd .. ; tar -czvf statserial-1.1.tar.gz statserial-1.1
