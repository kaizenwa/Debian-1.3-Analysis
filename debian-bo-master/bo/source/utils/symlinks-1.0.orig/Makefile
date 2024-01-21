# Makefile for symlinks
 
all: symlinks

symlinks: symlinks.c
	gcc -Wall -Wstrict-prototypes -O2 -N -s -o symlinks symlinks.c
 
install: all symlinks.8
	install -m 755 -o root -g root symlinks /usr/local/bin
	install -m 644 -o root -g root symlinks.8 /usr/local/man/man8

clean:
	rm -f symlinks *.o core
