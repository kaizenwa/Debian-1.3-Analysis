LDFLAGS = -s

all:	lpr

lpr:	lpr.o

install: lpr
	install -o root -g lp -m 6755 lpr ${DEBDIR}/usr/bin
	install -o root -g root -m 0644 lpr.1 ${DEBDIR}/usr/man/man1

clean:
	rm -f lpr *.o

