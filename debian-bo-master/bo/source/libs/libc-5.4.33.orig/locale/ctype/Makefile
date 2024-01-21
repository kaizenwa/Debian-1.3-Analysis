SHELL	= /bin/sh

CFLAGS	= -O
LFLAGS	= -8
YFLAGS	= -d

OBJS	= mkctype.o parse.o lexer.o

LIBS	= -lfl

mkctype:	${OBJS}
	${CC} ${CFLAGS} -o $@ ${OBJS} ${LIBS}

lexer.o:	lexer.l parse.o

clean:
	rm -f *.output *.tab.c *.tab.h *.o core nohup.out LC_*

run:
	mkctype example

install:	mkctype
	install -c -m 555 mkctype /usr/bin
	install -c -m 444 mkctype.1 /usr/man/man1
