EXEC= netwatch
CC=gcc
#  You might need to say where the ncurses include files are.
CFLAGS=-O2 -I/usr/include/ncurses
# CFLAGS=
DEFS= curs.h netwatch.h
OBJECTS= curs.o dispdata.o services.o netwatch.o processinetrc.o
LIBS= -lncurses
INSTALLDIR=/usr/local/bin

all:	$(EXEC)

netwatch:	$(OBJECTS) $(DEFS)
	$(CC) -o $(EXEC) $(CFLAGS) $(OBJECTS) $(LIBS)

install:	$(EXEC)
	chown root.root $(EXEC)
	chmod a+rx $(EXEC)
	chmod a-w  $(EXEC)
	chmod u+s  $(EXEC)
	mv $(EXEC) $(INSTALLDIR)/$(EXEC)

tar:
	tar -C .. -czf ../netwatch0.5.src.tar.gz netwatch

clean:
	rm -f *.o $(EXEC) 

curs.o:		curs.c curs.h
dispdata.o:	dispdata.c curs.h netwatch.h
services.o:	services.c netwatch.h
netwatch.o: 	netwatch.c netwatch.h curs.h
processinetrc.o:	processinetrc.c