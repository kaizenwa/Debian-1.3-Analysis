#
#	Makefile for RADIUS - 
#		Remote Authentication Dial In User Service
#
#
# Add -DNOSHADOW to CFLAGS if you don't have Shadow Passwords
#
# aix:	add -Daix to CFLAGS
# SCO:	add LIBS= -lsocket
# Solaris:
#	CFLAGS= -O -Dsys5
#	CC=	gcc -traditional
#	LIBS= -lnsl -lsocket
# unixware: add -Dunixware to CFLAGS, use LIBS= -lucb -lnsl -lsocket
#
SRCDIR=.
CFLAGS= -O -DNOSHADOW
LDFLAGS=
CC=	cc
LIBS=
SERVER_OBJS=radiusd.o dict.o users.o util.o md5.o attrprint.o acct.o version.o
SERVERDBM_OBJS=radiusd.o dict.o usersdbm.o util.o md5.o attrprint.o acct.o versiondbm.o
SERVER_SRCS=radiusd.c dict.c users.c util.c md5.c attrprint.c acct.c version.c
INCLUDES=$(SRCDIR)/radius.h $(SRCDIR)/conf.h

ALL:	radiusd radpass

dbm:	radiusd.dbm builddbm

radiusd: $(SERVER_OBJS)
	$(CC) $(CFLAGS) -o radiusd $(SERVER_OBJS) $(LIBS)

radiusd.dbm: $(SERVERDBM_OBJS)
	$(CC) $(CFLAGS) -o radiusd.dbm $(SERVERDBM_OBJS) -ldbm $(LIBS)

radiusd.o: $(SRCDIR)/radiusd.c $(INCLUDES)
	$(CC) $(CFLAGS) -c $(SRCDIR)/radiusd.c

acct.o: $(SRCDIR)/acct.c $(INCLUDES)
	$(CC) $(CFLAGS) -c $(SRCDIR)/acct.c

attrprint.o: $(SRCDIR)/attrprint.c $(INCLUDES)
	$(CC) $(CFLAGS) -c $(SRCDIR)/attrprint.c

dict.o: $(SRCDIR)/dict.c $(INCLUDES)
	$(CC) $(CFLAGS) -c $(SRCDIR)/dict.c

users.o: $(SRCDIR)/users.c $(INCLUDES)
	$(CC) $(CFLAGS) -c $(SRCDIR)/users.c

usersdbm.o: $(SRCDIR)/users.c $(INCLUDES)
	$(CC) $(CFLAGS) -DDBM -o usersdbm.o -c $(SRCDIR)/users.c

util.o: $(SRCDIR)/util.c $(INCLUDES)
	$(CC) $(CFLAGS) -c $(SRCDIR)/util.c

version.o: $(SRCDIR)/version.c $(INCLUDES)
	$(CC) $(CFLAGS) -o version.o -c $(SRCDIR)/version.c

versiondbm.o: $(SRCDIR)/version.c $(INCLUDES)
	$(CC) $(CFLAGS) -DDBM -o versiondbm.o -c $(SRCDIR)/version.c

radpass: radpass.o md5.o util.o
	$(CC) $(CFLAGS) -o radpass radpass.o md5.o util.o $(LIBS)

radpass.o: $(SRCDIR)/radpass.c $(INCLUDES)
	$(CC) $(CFLAGS) -c $(SRCDIR)/radpass.c

md5.o: $(SRCDIR)/md5.c $(SRCDIR)/md5.h
	$(CC) $(CFLAGS) -c $(SRCDIR)/md5.c

builddbm: builddbm.o
	$(CC) $(CFLAGS) -o builddbm builddbm.o -ldbm $(LIBS)

builddbm.o: $(SRCDIR)/builddbm.c
	$(CC) $(CFLAGS) -c $(SRCDIR)/builddbm.c

lint:
	-lint -hbacvx -DLINT $(SERVER_SRCS)
	-lint -hbacvx -DLINT ../radpass.c ../md5.c ../util.c

clean:
	rm -f *.o radiusd radpass builddbm radiusd.dbm
