SHELL	= /bin/sh

CFLAGS	= -O

OBJS	= mkresponse.o

mkresponse:	${OBJS}
	${CC} ${CFLAGS} -o $@ ${OBJS} ${LIBS}

clean:
	rm -f *.output *.tab.c *.tab.h *.o core nohup.out LC_*

run:
	mkresponse example

install:	mkresponse
	install -c -m 555 mkresponse /usr/bin
	install -c -m 444 mkresponse.1 /usr/man/man1
