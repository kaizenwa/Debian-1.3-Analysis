# $Id: Makefile,v 8.1 1994/12/15 06:23:45 vixie Exp $

.PATH:	${.CURDIR}/../BIND/tools \
	${.CURDIR}/../BIND/tools/nslookup \
	${.CURDIR}/../BIND/man

PROG=	dig
SRCS=	dig.c list.c subr.c debug.c send.c

BINDIR=	/usr/bin

.include <bsd.prog.mk>
.include "${.CURDIR}/../Makefile.maninc"
