EXEC= ipxdump ipxparse

CFLAGS= -Wall -O2
OBJECTS= ipxutil.o

all:	$(EXEC)

ipxdump: ipxdump.o $(OBJECTS)
	$(CC) -o $@ ipxdump.o $(CFLAGS) $(OBJECTS)

ipxparse: ipxparse.o $(OBJECTS)
	$(CC) -o $@ ipxparse.o $(CFLAGS) $(OBJECTS)

clean:
	rm -f *.o $(EXEC) *~


modules: ncpfs.o

SRCPATH=$(shell pwd)
SRCDIR=$(shell basename $(SRCPATH))
DISTFILE=$(SRCDIR).tgz

mrproper: clean
	rm -f $(DISTFILE)
	
dist: tgz
	make all

tgz: mrproper
	(cd ..; \
         tar cvf - $(SRCDIR) | \
            gzip -9 > $(DISTFILE); \
         mv $(DISTFILE) $(SRCDIR))

