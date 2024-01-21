SHELL	= /bin/sh

CFLAGS	= -O

OBJS	= mktime.o

mktime:	${OBJS}
	${CC} ${CFLAGS} -o $@ ${OBJS} ${LIBS}

clean:
	rm -f *.output *.tab.c *.tab.h *.o core nohup.out LC_*

run:
	mktime example

install:	mktime
	install -c -m 555 mktime /usr/bin
	install -c -m 444 mktime.1 /usr/man/man1
