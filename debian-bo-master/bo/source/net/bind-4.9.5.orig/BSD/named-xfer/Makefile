# $Id: Makefile,v 8.3 1996/01/09 20:23:48 vixie Exp $

.PATH:	${.CURDIR}/../BIND/named \
	${.CURDIR}/../named/obj \
	${.CURDIR}/../BIND/man

PROG=	named-xfer
SRCS=	named-xfer.c db_glue.c storage.c dmalloc.c version.c
#LDADD+=	-lutil
#DPADD+=	${LIBUTIL}

BINDIR=	/usr/libexec

.include <bsd.prog.mk>
.include "${.CURDIR}/../Makefile.maninc"
