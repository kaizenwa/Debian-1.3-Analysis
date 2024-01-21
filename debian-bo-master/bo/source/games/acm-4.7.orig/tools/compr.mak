CC=gcc
CFLAGS=-I/usr/X11R5/include -I../V/lib -O -g

compr: compr.o
	$(CC) $(CFLAGS) -o $@ compr.o -L/usr/X11R5/lib -L../V/lib -lV -lX11 -lm
