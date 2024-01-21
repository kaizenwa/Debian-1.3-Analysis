# setserial.mk - makefile for setserial - rick sladkey
# modified by Michael K. Johnson, johnsonm@stolaf.edu
# debianized by Gordon Russell, g.russell@dcs.napier.ac.uk

CC = gcc
CFLAGS = -O

all: setserial setserial.cat

setserial: setserial.c
	$(CC) $(CFLAGS) setserial.c -o setserial

setserial.cat: setserial.8
	nroff -man setserial.8 > setserial.cat

install: setserial
	install -g root -m 755 -o root -s setserial   $(DESTDIR)/bin
	install -g root -m 644 -o root    setserial.8 $(DESTDIR)/usr/man/man8
	@ARCH=`dpkg --print-architecture`;\
	if [ $${ARCH} == "m68k" ] ;\
        then echo "Building for 680x0 series";\
	  echo install -g root -m 755 -o root    rc.serial.m68k   $(DESTDIR)/etc/rc.boot/0setserial ;\
	  install -g root -m 755 -o root    rc.serial.m68k   $(DESTDIR)/etc/rc.boot/0setserial ;\
        else \
	  echo "Building for non-680x0 series";\
	  echo install -g root -m 755 -o root    rc.serial   $(DESTDIR)/etc/rc.boot/0setserial ;\
	  install -g root -m 755 -o root    rc.serial   $(DESTDIR)/etc/rc.boot/0setserial ;\
        fi

clean:
	rm -f setserial setserial.o setserial.cat *~

