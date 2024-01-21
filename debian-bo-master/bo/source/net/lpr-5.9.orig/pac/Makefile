all:	pac

pac:	pac.o

install: pac
	install -o root -g root -m 0755 pac ${DEBDIR}/usr/sbin
	install -o root -g root -m 0644 pac.8 ${DEBDIR}/usr/man/man8

clean:
	rm -f *.o pac

