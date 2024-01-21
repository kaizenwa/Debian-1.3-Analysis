LDFLAGS = -s

all:	lprm

lprm:	lprm.o

install: lprm
	install -o root -g lp -m 6755 lprm ${DEBDIR}/usr/bin
	install -o root -g root -m 0644 lprm.1 ${DEBDIR}/usr/man/man1

clean:
	rm -f lprm *.o

