SHELL	= /bin/sh

CFLAGS	= -O
LFLAGS	= -8
YFLAGS	= -d

OBJS	= mkcollate.o parse.o lexer.o

LIBS	= -lfl

mkcollate:	${OBJS}
	${CC} ${CFLAGS} -o $@ ${OBJS} ${LIBS}

lexer.o:	lexer.l parse.o

clean:
	rm -f *.output *.tab.c *.tab.h *.o core nohup.out LC_*

run:
	mkcollate example

install:	mkcollate
	install -c -m 555 mkcollate /usr/bin
	install -c -m 444 mkcollate.1 /usr/man/man1
