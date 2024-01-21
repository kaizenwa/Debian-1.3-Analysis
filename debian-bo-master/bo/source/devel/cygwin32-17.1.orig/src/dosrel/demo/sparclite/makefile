.SUFFIXES : .x .o .c .s
CC=gcc
AS=as
LD=ld
LDFLAGS= -L../lib -Tex93x.ld

all: hello.x
	nm hello.x
	objdump -d -j .text hello.o
	objdump -s -j .rodata hello.o
	size hello.x

hello.s: hello.c
	$(CC) -O -g -S hello.c

hello.o: hello.s
	$(AS) -ahld -o hello.o hello.s

hello.x: hello.o
	$(LD)  -o hello.x $(LDFLAGS) ../lib/crt0.o hello.o -lc -lgcc

clean:
	rm -f *.[osx]
