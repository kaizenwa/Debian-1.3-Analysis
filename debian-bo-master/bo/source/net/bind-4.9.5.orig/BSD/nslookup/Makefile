# $Id: Makefile,v 8.2 1996/08/05 08:31:22 vixie Exp $

.PATH:	${.CURDIR}/../BIND/tools/nslookup \
	${.CURDIR}/../BIND/man

PROG=	nslookup
SRCS=	main.c getinfo.c debug.c send.c skip.c list.c subr.c
OBJS+=	commands.o
DPADD=	${LIBL}
LDADD=	-ll
BINDIR=	/usr/bin
LINKS=	${BINDIR}/nslookup ${DESTSBIN}/nslookup
CLEANFILES+= commands.c lex.yy.c lex.yy.o

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 \
		${.CURDIR}/../BIND/tools/nslookup/nslookup.help \
		${DESTDIR}/usr/share/misc/nslookup.help

.include <bsd.prog.mk>
.include "${.CURDIR}/../Makefile.maninc"
