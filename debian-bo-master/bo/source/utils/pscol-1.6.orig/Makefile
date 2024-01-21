VERMAJOR = 1
VERMINOR = 6
CC = gcc
OBJ  =  parsevar.o convname.o convuids.o main.o allnum.o buildvar.o \
	buildpsfile.o readfile.o convplus.o convtype.o convcolor.o \
	strarraycmp.o isansi.o convansi.o
FILES = Makefile allnum.c buildpsfile.c buildvar.c config.h convansi.c \
	convcolor.c convname.c convplus.c convtype.c convuids.c isansi.c \
	main.c parsevar.c pscol.doc pscol.h pscolor.1 pscolor.2 readfile.c \
	strarraycmp.c
TAR = tar
DISTDIR = pscol-$(VERMAJOR).$(VERMINOR)
TARFILE = $(DISTDIR).tar.gz

all:	pscol

pscol:	$(OBJ)
	$(CC) $(OBJ) -o $@
	strip $@

objs:	$(OBJ)

noobjs: pscol
	rm *.o

main.o: main.c
	$(CC) -c $< -DPS_COL_VER=\"$(VERMAJOR).$(VERMINOR)\"

.c.o:
	$(CC) -c $<

sortaclean:
	rm -f *.o *~

clean:
	rm -f *.o pscol *~

distclean: clean
	rm -f $(TARFILE)
	rm -rf $(DISTDIR)

dist:	distclean
	mkdir $(DISTDIR)
	cp -a $(FILES) $(DISTDIR)
	$(TAR) zcf $(TARFILE) $(DISTDIR)
	rm -rf $(DISTDIR)

redo:	clean all
