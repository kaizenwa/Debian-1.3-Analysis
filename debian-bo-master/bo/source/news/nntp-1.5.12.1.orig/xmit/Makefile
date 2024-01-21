#
# Makefile for nntpxmit
#
# $Id: Makefile,v 1.6 1994/11/17 16:43:05 sob Exp sob $
#
SRVRSRC = nntpxmit.c remote.c llist.c get_tcp_conn.c xmitauth.c ../server/fakesyslog.c ../server/strcasecmp.c

SRVROBJ = nntpxmit.o remote.o llist.o get_tcp_conn.o xmitauth.o fakesyslog.o strcasecmp.o

SRVRINC = ../conf.h ../server/nntp.h nntpxmit.h get_tcp_conn.h \
	llist.h sysexits.h ../server/fakesyslog.h

SRCS	= ${SRVRSRC}

LIBS	=

CFLAGS	= -O 

all:	nntpxmit shlock

nntpxmit: ${SRVROBJ} ${SRVRINC}
	${CC} ${CFLAGS} -o nntpxmit ${SRVROBJ} ${LIBS}

shlock: shlock.c
	cc ${CFLAGS} -DNNTPSRC -o shlock shlock.c

${SRVROBJ}: ${SRVRINC}

install: nntpxmit shlock
	cp nntpxmit ${BINDIR}/nntpxmit
	cp shlock ${BINDIR}/shlock
	chmod 755 ${BINDIR}/nntpxmit
	chmod 755 ${BINDIR}/shlock

lint:
	lint ${SRVRSRC}

clean:
	-rm -f *.o nntpxmit shlock make*.out a.out *.BAK *.CKP *~

distrib: clean
	rm -rf RCS save tags

check:
	ci -l *.[ch] Makefile

tags:	${SRVRSRC} ${SRVRINC}
	ctags ${SRVRSRC} ${SRVRINC}

fakesyslog.o: ../server/fakesyslog.c ../server/fakesyslog.h
	${CC} ${CFLAGS} -c ../server/fakesyslog.c

strcasecmp.o: ../server/strcasecmp.c 
	${CC} ${CFLAGS} -c ../server/strcasecmp.c

nntpxmit.o: nntpxmit.c nntpxmit.h llist.h ../conf.h

get_tcp_conn.o: get_tcp_conn.c get_tcp_conn.h ../conf.h

remote.o: remote.c nntpxmit.h get_tcp_conn.h ../conf.h

llist.o: llist.c llist.h

