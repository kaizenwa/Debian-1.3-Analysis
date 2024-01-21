CPPFLAGS	= -DLINUX
CFLAGS		= -m486 -O2 -g -Wall
CC		= gcc
RM		= rm
PROGS		= js jscal
BINDIR		= /usr/local/bin
MANDIR		= /usr/man

all: joystick.o $(PROGS)

js: js.o
	$(CC) -o $@ $^

jscal: jscal.o
	$(CC) -o $@ $^

joystick.o: joystick.c /usr/include/linux/joystick.h
	$(CC) -D__KERNEL__ -Wall -DMODULE $(CPPFLAGS) -c -m486 -O6 joystick.c -o joystick.o

clean:
	-$(RM) -f *.o $(PROGS) *~ core

devs:
	-rm /dev/js0 /dev/js1
	mknod /dev/js0 c 15 0
	mknod /dev/js1 c 15 1

install: all devs install.man
	install -d $(BINDIR)
	install $(PROGS) $(BINDIR)
	install -d $(MANDIR)/man4

install.man:
	install -m 644 js.4 $(MANDIR)/man4
	
dist:	clean
	tar czf ../`basename \`pwd\``.tar.gz -C .. `basename \`pwd\``

/usr/include/linux/joystick.h: joystick.h
	install -m 644 joystick.h /usr/include/linux/joystick.h

js.o: js.c /usr/include/linux/joystick.h
jscal.o: jscal.c /usr/include/linux/joystick.h
