all:	lpf

lpf:	lpf.o

install: lpf
	install -o root -g root -m 0755 lpf ${DEBDIR}/usr/bin

clean:
	rm -f lpf *.o

