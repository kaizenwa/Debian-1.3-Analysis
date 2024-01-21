CC = gcc
INC = pal.h palfiler.h palpriv.h config.h
BINS = lxdir lxcopy lxdel lxmkdir lxrmdir
all : $(BINS)

clean:
	/bin/rm -f *.o core

reallyclean:
	/bin/rm -f *.o core $(BINS)
	
lxdir : lxdir.o filer.o filercb.o asdfiler.o gtdfiler.o synfiler.o
	$(CC) -o lxdir lxdir.o filer.o filercb.o asdfiler.o gtdfiler.o synfiler.o 

lxdel : lxdel.o filer.o filercb.o asdfiler.o gtdfiler.o synfiler.o delfiler.o
	$(CC) -o lxdel lxdel.o filer.o filercb.o asdfiler.o gtdfiler.o synfiler.o delfiler.o

lxrmdir : lxrmdir.o filer.o filercb.o asdfiler.o gtdfiler.o synfiler.o rmdfiler.o
	$(CC) -o lxrmdir lxrmdir.o filer.o filercb.o asdfiler.o gtdfiler.o synfiler.o rmdfiler.o

lxmkdir : lxmkdir.o filer.o filercb.o asdfiler.o gtdfiler.o synfiler.o mkdfiler.o
	$(CC) -o lxmkdir lxmkdir.o filer.o filercb.o asdfiler.o gtdfiler.o synfiler.o mkdfiler.o

lxcopy : lxcopy.o filer.o filercb.o asdfiler.o gtdfiler.o synfiler.o getfiler.o sndfiler.o mkdfiler.o
	$(CC) -o lxcopy lxcopy.o filer.o filercb.o asdfiler.o gtdfiler.o synfiler.o getfiler.o sndfiler.o mkdfiler.o

lxdir.o : lxdir.c $(INC)
	$(CC) -c lxdir.c

lxcopy.o : lxcopy.c $(INC)
	$(CC) -c lxcopy.c

filer.o : filer.c $(INC)
	$(CC) -c filer.c

filercb.o : filercb.c $(INC)
	$(CC) -c filercb.c

asdfiler.o : asdfiler.c $(INC)
	$(CC) -c asdfiler.c

gtdfiler.o : gtdfiler.c $(INC)
	$(CC) -c gtdfiler.c

synfiler.o : synfiler.c $(INC)
	$(CC) -c synfiler.c

getfiler.o : getfiler.c $(INC)
	$(CC) -c getfiler.c

sndfiler.o : sndfiler.c $(INC)
	$(CC) -c sndfiler.c

delfiler.o : delfiler.c $(INC)
	$(CC) -c delfiler.c

mkdfiler.o : mkdfiler.c $(INC)
	$(CC) -c mkdfiler.c

rmdfiler.o : rmdfiler.c $(INC)
	$(CC) -c rmdfiler.c

