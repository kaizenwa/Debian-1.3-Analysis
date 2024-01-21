#CC = gcc

#Uncomment these two lines to use the DBM features of authwn
#You will likely have to change DBMLIB to whatever library contains
# the DBM functions dbminit(), fetch() and dbmclose() on your system.
#DBMLIB = -ldbm
#DBMFLAG =  -DDBM_AUTH


CFLAGS = -I../wn/ -O $(DBMFLAG)

OBJS = authwn.o

all:	authwn

authwn: $(OBJS)
	$(CC) $(CFLAGS) -o authwn $(OBJS) $(LIBS) $(DBMLIB)

install:	authwn
	@echo "Installing authwn in $(BINDIR)"
	@cp authwn $(BINDIR)
	@strip $(BINDIR)/authwn

clean:
	@rm -f *.o  *~ authwn

authwn.o: 		authwn.h  ../config.h

