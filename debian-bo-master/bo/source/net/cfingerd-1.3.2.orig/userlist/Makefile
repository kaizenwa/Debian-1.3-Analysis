# Makefile for the custom userlisting display
# version 1.00 by Ken Hollis
#
# Part of the CFINGERD distribution
# Released under the GPL for public/private use

CC= gcc
CFLAGS= -O2 -m486 -Wall
LINK_FLAGS= main.o initialize.o display.o options.o idle.o

userlist:	$(LINK_FLAGS)
		$(CC) -o $@ $^
		strip userlist

clean:
		rm -f *.o userlist
		rm -f config.h

install:	userlist
		chmod 755 userlist
		chown root.root userlist
		chmod +s userlist
		cp -f userlist /bin/userlist
		cp -f userlist.1 /usr/man/man1
