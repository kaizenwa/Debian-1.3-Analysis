all:	lptest

lptest:	lptest.o

install: lptest
	install -o root -g root -m 0755 lptest ${DEBDIR}/usr/bin
	install -o root -g root -m 0644 lptest.1 ${DEBDIR}/usr/man/man1

clean:
	rm -f lptest *.o

