PROGS=	cdwrite isosize
LINUX= /usr/src/linux
CFLAGS=	-Wall -O2 -fomit-frame-pointer -ansi -pedantic -I $(LINUX)/include -I $(LINUX)/drivers
LDFLAGS= -s
BINDIR=	/usr/bin

all:	$(PROGS)

install:	$(PROGS)
	install $(PROGS) $(BINDIR)

clean:
	-rm -f $(PROGS) *.o *~
