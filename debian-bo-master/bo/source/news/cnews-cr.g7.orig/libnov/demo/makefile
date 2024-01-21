DEFINES=-I../include
CFLAGS=$(DEFINES) -O # -g
LINTFLAGS=-ha $(DEFINES)

all: thread

thread: thread.o ../libnov/libnov.a ../libc/libuseful.a
	$(CC) $(CFLAGS) -o $@ thread.o ../libnov/libnov.a ../libc/libuseful.a

lint: thread.c lib.c
	lint $(LINTFLAGS) thread.c lib.c

clean:
	rm -f gmon.out a.out core *.o thread
