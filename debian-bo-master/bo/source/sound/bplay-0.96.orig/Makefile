PROG = bplay
OBJS = bplay.o sndfunc.o shmbuf.o
SRCS = bplay.c sndfunc.o shmbuf.c

CC = gcc
CFLAGS = -Wall -O2 -m486 -DUSEBUFFLOCK -DDEBUG
LDFLAGS = 
LIBS =

$(PROG): $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o $(PROG) $(LIBS)
	ln -sf bplay brec

clean:
	rm -f bplay brec *.o
