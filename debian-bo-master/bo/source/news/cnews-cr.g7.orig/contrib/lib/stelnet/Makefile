PROG = stelnet
OBJS = stelnet.o
SRCS = ${OBJS:.o=.c}
PROG1 = getpeername
OBJS1 = getpeername.o
SRCS1 = ${OBJS1:.o=.c}

#CC = gcc -traditional
BINDIR = /local/bin
LIBDIR = /local/lib
SHAREDIR = /local/share
MANEXT = 1
MANDIR = /local/man/man${MANEXT}
CDEBUGFLAGS = -g # -O
DEFINES=
INCLUDES=
LIBS=

CFLAGS = ${CDEBUGFLAGS} ${INCLUDES} ${DEFINES}
LINTFLAGS = ${INCLUDES} ${DEFINES}
LDFLAGS = ${CDEBUGFLAGS} ${LIBS}

${PROG}: ${OBJS}
	/bin/rm -f ${PROG}
	${CC} -o ${PROG} ${OBJS} ${LDFLAGS}

${PROG1}: ${OBJS1}
	/bin/rm -f ${PROG1}
	${CC} -o ${PROG1} ${OBJS1} ${LDFLAGS}

install: ${PROG} ${PROG1}
	-mv -f ${BINDIR}/${PROG} ${BINDIR}/${PROG}-
	install -c -m 755 ${PROG} ${BINDIR}
	-mv -f ${BINDIR}/${PROG1} ${BINDIR}/${PROG1}-
	install -c -m 755 ${PROG1} ${BINDIR}
	install -c -m 755 runonpty.sh ${BINDIR}/runonpty
#	install -c -m 444 ${PROG}.man ${MANDIR}/${PROG}.${MANEXT}

clean:
	rm -f *.o core make.out \#* *~ ${PROG} ${PROG1}

lint:
	lint $(LINTOPTS) ${DEFINES} ${INCLUDES} ${SRCS}
	lint $(LINTOPTS) ${DEFINES} ${INCLUDES} ${SRCS1}

depend:
	mkdep ${DEFINES} ${INCLUDES} ${SRCS} ${SRCS1}

stelnet.o: telnet.h
