CC = gcc
CFLAGS = -O2 -fomit-frame-pointer  \
	-include /usr/include/bsd/bsd.h -I/usr/include/bsd
LDLIBS = -lbsd
LDFLAGS = -s

comsat: comsat.o

biff: biff.o

install: comsat biff
	install -o root -g root -m 0744 comsat /usr/sbin/in.comsat
	install -o root -g root -m 0755 biff /usr/bin/biff
	
clean:
	rm -f *.o comsat biff

