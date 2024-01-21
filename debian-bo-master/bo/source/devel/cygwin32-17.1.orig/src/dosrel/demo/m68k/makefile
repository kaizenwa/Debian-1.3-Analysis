.SUFFIXES : .x .o .c .s
CC=gcc
AS=as
LD=ld
IDP_LDFLAGS= -L../lib -Tidp.ld
MVME_LDFLAGS= -L../lib -Tmvme135.ld

all: hi-idp.x hi-mvme.x
	nm hi-idp.x
	nm hi-mvme.x
	objdump -d -j .text hello.o
	objdump -s -j .rodata hello.o
	size hi-idp.x
	size hi-mvme.x

hello.s : hello.c
	$(CC) -O -g -S hello.c

hello.o : hello.s
	$(AS) -ahld -o hello.o hello.s

hi-idp.x: hello.o
	$(LD)  -o hi-idp.x $(IDP_LDFLAGS) ../lib/crt0.o hello.o -lc -lgcc

hi-mvme.x: hello.c
	$(LD)  -o hi-mvme.x $(MVME_LDFLAGS) ../lib/crt0.o hello.o -lc -lgcc

clean:
	rm -f *.[osx]
