#
# Makefile for nntpxfer
#
SRVRSRC = nntpxfer.c ../xmit/get_tcp_conn.c ../server/fakesyslog.c 

SRVROBJ = nntpxfer.o get_tcp_conn.o fakesyslog.o 

# set CFLAGS to be -Ipath/to/special/includes if needed
CFLAGS= -O
# Uncomment the following for System V Release 4 Systems
#NETLIBS   = -lsocket -lnsl
# set DBLIBS to be -lneededlibrary if needed
#DBLIBS	= -ldbz
#DBLIBS	= /usr/local/lib/dbz.o #/usr/local/lib/dbzdbm.o
#DBLIBS	= -ldbm

LIBS = $(DBLIBS) $(NETLIBS)

nntpxfer: ${SRVROBJ}
	${CC} ${LDFLAGS} ${SRVROBJ} -o nntpxfer ${LIBS}

get_tcp_conn.o: ../xmit/get_tcp_conn.c ../xmit/get_tcp_conn.h
	${CC} ${CFLAGS} ${DEFS}	-c ../xmit/get_tcp_conn.c

fakesyslog.o: ../server/fakesyslog.c ../server/fakesyslog.h
	${CC} ${CFLAGS} ${DEFS} -c ../server/fakesyslog.c

nntpxfer.o: nntpxfer.c ../conf.h ../server/fakesyslog.h

install: nntpxfer
	cp nntpxfer ${BINDIR}

clean:
	-rm -f *.o nntpxfer make*.out *.BAK *.CKP *~

distrib: clean
	rm -rf SCCS

check:;
