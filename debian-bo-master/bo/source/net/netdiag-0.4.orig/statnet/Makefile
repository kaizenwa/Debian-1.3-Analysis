EXEC= statnet

#  You might need to say where the ncurses include files are.
CFLAGS=-I/usr/include/ncurses
# CFLAGS=
DEFS= curs.h stat.h
OBJECTS= curs.o if_getstats.o itstime.o set_null.o services.o stat.o
LIBS= -lncurses
INSTALLDIR=/usr/local/bin

all:	$(EXEC)

statnet:	$(OBJECTS) $(DEFS)
		$(CC) -o $(EXEC) $(CFLAGS) $(OBJECTS) $(LIBS)

install:	$(EXEC) $(INSTALLDIR)/$(EXEC)
	chown root.root $(EXEC)
	chmod a+rx $(EXEC)
	chmod a-w  $(EXEC)
	chmod u+s  $(EXEC)
	mv $(EXEC) $(INSTALLDIR)/$(EXEC)

shar:
	shar README CHANGES INSTALL Makefile *.h *.c *.lsm >statnet.shar

tar:
	tar cvf statnet2.1.tar README CHANGES INSTALL Makefile *.h *.c *.lsm
	gzip -v statnet2.1.tar

clean:
	rm -f *.o $(EXEC) statnet.shar statnet.tar statnet.tar.gz

curs.o:		curs.c curs.h
if_getstats.o:	if_getstats.c
itstime.o:	itstime.c curs.h stat.h
services.o:	services.c stat.h
set_null.o:	set_null.c stat.h
stat.o: 	stat.c stat.h curs.h
