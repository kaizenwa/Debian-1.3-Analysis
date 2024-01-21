SHELL	= /bin/sh

CFLAGS	= -O
LFLAGS	= -8
YFLAGS	= -d

OBJS	= mkmonetary.o parse.o lexer.o

LIBS	= -lfl

mkmonetary:	${OBJS}
	${CC} ${CFLAGS} -o $@ ${OBJS} ${LIBS}

lexer.o:	lexer.l parse.o

clean:
	rm -f *.output *.tab.c *.tab.h *.o core nohup.out LC_*

run:
	mkmonetary example

install:	mkmonetary
	install -c -m 555 mkmonetary /usr/bin
	install -c -m 444 mkmonetary.1 /usr/man/man1
