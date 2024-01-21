SHELL	= /bin/sh

CFLAGS	= -O
LFLAGS	= -8
YFLAGS	= -d

OBJS	= mknumeric.o parse.o lexer.o

LIBS	= -lfl

mknumeric:	${OBJS}
	${CC} ${CFLAGS} -o $@ ${OBJS} ${LIBS}

lexer.o:	lexer.l parse.o

clean:
	rm -f *.output *.tab.c *.tab.h *.o core nohup.out LC_*

run:
	mknumeric example

install:	mknumeric
	install -c -m 555 mknumeric /usr/bin
	install -c -m 444 mknumeric.1 /usr/man/man1
