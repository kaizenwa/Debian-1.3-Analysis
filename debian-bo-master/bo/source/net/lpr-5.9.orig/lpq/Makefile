LDFLAGS = -s

all:	lpq

lpq:	lpq.o

install: lpq
	install -o root -g lp -m 6755 lpq ${DEBDIR}/usr/bin
	install -o root -g root -m 0644 lpq.1 ${DEBDIR}/usr/man/man1

clean:
	@rm -f lpq *.o

