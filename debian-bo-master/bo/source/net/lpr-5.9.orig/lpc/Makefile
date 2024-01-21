LDFLAGS = -s

all:	lpc

lpc:	lpc.o cmds.o cmdtab.o

install: lpc
	install -o root -g lp -m 2755 lpc ${DEBDIR}/usr/sbin
	install -o root -g root -m 0644 lpc.8 ${DEBDIR}/usr/man/man8

clean:
	rm -f lpc *.o

