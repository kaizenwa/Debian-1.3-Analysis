CFLAGS	= -Wall -O2 -fomit-frame-pointer
LDFLAGS	= -s -v

PROGS =		systat
MAN1 =		systat.1

all:		$(PROGS)

systat:	systat.o
		$(CC) $(CFLAGS) $(LDFLAGS) -o $@ systat.o

install:	all
		install -o root -g root -m 0755 systat /usr/bin
		install -o root -g root -m 0644 $(MAN1) /usr/man/man1

clean:
		rm -f $(PROGS) core *.o

