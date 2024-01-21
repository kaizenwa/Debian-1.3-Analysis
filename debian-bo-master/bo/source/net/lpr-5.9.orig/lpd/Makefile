LDFLAGS = -s

all:	lpd

lpd:	lpd.o lpdchar.o printjob.o recvjob.o

install: lpd
	install -o root -g root -m 0755 lpd ${DEBDIR}/usr/sbin
	install -o root -g root -m 0644 lpd.8 ${DEBDIR}/usr/man/man8

clean:
	rm -f lpd *.o

