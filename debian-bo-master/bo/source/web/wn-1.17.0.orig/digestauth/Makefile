
# Uncomment these two lines to use the DBM features of wndigest
# You will likely have to change DBMLIB to whatever library contains
# the DBM functions dbminit(), fetch() and dbmclose() on your system.

#DBMLIB = -ldbm
#DBMFLAG =  -DDBM_AUTH


CFLAGS = -I../wn/ -O -DMD=5 $(DBMFLAG)

OBJS = wndigest.o md5c.o

all:	md5 wndigest

wndigest: $(OBJS)
	$(CC) $(CFLAGS) -o wndigest $(OBJS) $(LIBS) $(DBMLIB)

md5: mddriver.o md5c.o
	$(CC) $(CFLAGS) -o md5 mddriver.o md5c.o $(LIBS)

install:	wndigest md5
	$(INSTALL) wndigest $(BINDIR)

clean:
	rm -f *.o  *~ wndigest md5

wndigest.o:	md5 wndigest.c wndigest.h global.h md5.h ../config.h
	@echo "Calculating random seed"
	perl ./rand > random.h
	$(CC) $(CFLAGS) -c wndigest.c -o wndigest.o
	rm -f random.h

md5c.o: md5c.c global.h md5.h
mddriver.o: mddriver.c md5c.c global.h md5.h
wndigest.o: md5c.o wndigest.c wndigest.h



