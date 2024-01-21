#
# Makefile for NNTP server support programs
#

OBJS	=	acttimes.o

SRCS	=	acttimes.c

HFILES	=	../conf.h

CFLAGS= -O

# Where these support programs will live

#DESTDIR	= /usr/lib/news

all: acttimes

acttimes: acttimes.o
	$(CC) ${CFLAGS} -o acttimes acttimes.o

install: acttimes
	cp acttimes ${BINDIR}/acttimes
	chmod 755 ${BINDIR}/acttimes

lint:
	lint acttimes.c

clean:
	-rm -f *.o acttimes *.BAK *.CKP *~

distrib: clean
	rm -rf SCCS old

check:
	sccs check
	sccs prt -y *.[ch] > /dev/null

tags:	${SRCS} ${HFILES}
	ctags ${SRCS} ${HFILES}

${OBJS}:	${HFILES}
