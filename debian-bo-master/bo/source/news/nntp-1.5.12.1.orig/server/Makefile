# $Id: Makefile,v 1.7 1994/12/04 00:18:40 sob Exp sob $
# Makefile for NNTP server
#

SRVROBJ = main.o serve.o access.o access_inet.o access_dnet.o active.o \
	ahbs.o globals.o group.o help.o ihave.o list.o misc.o netaux.o \
	newgroups.o newnews.o nextlast.o ngmatch.o post.o parsit.o scandir.o \
	slave.o spawn.o strcasecmp.o subnet.o date.o xhdr.o fakesyslog.o \
	batch.o xauth.o timer.o msgid.o xover.o xthread.o xindex.o wildmat.o \
	version.o

SRVRSRC = main.c serve.c access.c access_inet.c access_dnet.c active.c \
	ahbs.c globals.c group.c help.c ihave.c list.c misc.c netaux.c \
	newgroups.c newnews.c nextlast.c ngmatch.c post.c parsit.c scandir.c \
	slave.c spawn.c strcasecmp.c subnet.c date.c xhdr.c fakesyslog.c \
	batch.c xauth.c timer.c msgid.c xover.c xthread.c xindex.c wildmat.c \
	version.c

SRVRINC = common.h ../conf.h nntp.h timer.h

SRCS	= ${SRVRSRC}

# set CFLAGS to be -Ipath/to/special/includes if needed
#CFLAGS	= -O
# Uncomment the following for System V Release 4 Systems
#NETLIBS   = -lsocket -lnsl
# set DBLIBS to be -lneededlibrary if needed
#DBLIBS	= -ldbz
#DBLIBS	= /usr/local/lib/dbz.o #/usr/local/lib/dbzdbm.o
#DBLIBS	= -ldbm

LIBS = $(DBLIBS) $(NETLIBS)

# Where nntpd is going to live
#DESTDIR = /etc

all:	nntpd msgidd

nntpd: ${SRVROBJ} ${SRVRINC}
	${CC} ${CFLAGS} -o nntpd ${SRVROBJ} ${LIBS}

${SRVROBJ}: ${SRVRINC}

msgidd: msgidd.c
	${CC} ${CFLAGS} -o msgidd msgidd.c ${LIBS}

install: nntpd
	-mv -f ${ETCDIR}/nntpd ${ETCDIR}/nntpd.old
	cp nntpd ${ETCDIR}/nntpd
	chmod 711 ${ETCDIR}/nntpd

install_msgidd: msgidd
	-mv -f ${ETCDIR}/msgidd ${ETCDIR}/msgidd.old
	cp msgidd ${ETCDIR}/msgidd
	chmod 711 ${ETCDIR}/msgidd

testsubnet: subnet.c
	${CC} ${CFLAGS} -DTESTSUBNET -o testsubnet subnet.c ${LIBS}

lint:
	lint ${SRVRSRC}

clean:
	-rm -f *.o nntpd msgidd testsubnet make*.out a.out *.BAK *.CKP *~ 

distrib: clean
	rm -rf SCCS save tags

check:
	sccs check
	sccs prt -y *.[ch] > /dev/null

tags:	${SRVRSRC} ${SRVRINC}
	ctags -wt ${SRVRSRC} ${SRVRINC}

# Dependencies
