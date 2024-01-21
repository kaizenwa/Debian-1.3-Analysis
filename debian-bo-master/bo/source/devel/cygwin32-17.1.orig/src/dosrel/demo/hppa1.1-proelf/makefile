.SUFFIXES : .x .o .c .s
CC=gcc -msoft-float
AS=as
LD=ld

OKI_LDFLAGS= -L../lib -Top50n.ld -Ttext 40000
WEC_LDFLAGS= -L../lib -Tw89k.ld  -Ttext 100000
HP_LDFLAGS=  -L../lib -Thp74x.ld -N -R 10000 -a archive
LIBS= ../lib/soft-flo/libc.a ../lib/soft-flo/libgcc.a ../lib/soft-flo/libc.a

all: hi-oki.x hi-wec.x
	del *.nm
	nm hi-wec.x > hi-wec.nm
	nm hi-oki.x > hi-oki.nm
	nm hi-wec.x
	nm hi-oki.x

hello.s: hello.c
	$(CC) $(CFLAGS) -S hello.c

hello.o: hello.s
	$(AS) -ahld -o hello.o hello.s

hi-oki.x: hello.o
	$(LD)  -o hi-oki.x hello.o $(OKI_LDFLAGS) $(LIBS)

hi-wec.x: hello.c
	$(LD)  -o hi-wec.x hello.o $(WEC_LDFLAGS) $(LIBS)

clean:
	rm -f *.[osx] *.nm *.dis *.sre
