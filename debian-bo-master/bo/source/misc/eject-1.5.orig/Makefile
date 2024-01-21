# Make file for eject

CC	= gcc
LD	= gcc

# for debug
#CFLAGS	= -Wall -g
#LDFLAGS = -g

# for production code
# on a.out systems you might want to add -N when linking
CFLAGS	= -Wall -O3
LDFLAGS = -s

eject:	eject.o
	$(LD) $(LDFLAGS) -o eject eject.o

eject.o: eject.c
	$(CC) $(CFLAGS) -c eject.c

# you may prefer to install program in /usr/bin or /usr/local/bin or
# even /sbin
# you may also want to install it setuid root (see man page)
install: eject
	install -m 555 eject /usr/local/bin/eject
	install -m 444 eject.1 /usr/local/man/man1/eject.1
	@if [ -f /usr/bin/eject ]; then echo "Note: There is another version of eject in /usr/bin"; fi
	@if [ -f /sbin/eject ]; then echo "Note: There is another version of eject in /sbin"; fi

clean:
	$(RM) eject eject.o core *~

dist:	eject.c Makefile eject.1 README
	$(RM) eject eject.o core eject-1.5.tar.gz *~
	cd .. ; tar -czvf eject-1.5.tar.gz eject-1.5
