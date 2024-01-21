CFLAGS = -Wall -fdollars-in-identifiers -c -g
OBJS = stubs-stdio.o stubs-string.o stubs-malloc.o stubs-stdlib.o \
 stubs-time.o stubs-sys_timeb.o stubs-unistd.o stubs-fcntl.o stubs-getopt.o \
 stubs-pwd.o stubs-grp.o stubs-sys_stat.o stubs-setjmp.o stubs-signal.o \
 stubs-sys_resource.o stubs-sys_wait.o stubs-sys_utsname.o stubs-dirent.o \
 stubs-utime.o stubs-ioctl.o stubs-fnmatch.o stubs-curses.o \
 stubs-netinet_in.o stubs-sys_time.o stubs-sys_socket.o stubs-arpa_inet.o \
 stubs-netdb.o stubs-termios.o stubs-sys_uio.o \
 check-printf.o

all: libstubs.a

$(OBJS):
	$(CC) $(CFLAGS) $(subst .o,.c,$@) -o $@

libstubs.a: $(OBJS)
	$(AR) rcv libstubs.a $(OBJS)

mk-stubs.o: mk-stubs.c
	$(CC) -g -c -o $@ mk-stubs.c

mk-stubs: mk-stubs.o
	$(CC) -o mk-stubs mk-stubs.o

clean:
	rm -f *.o mk-stubs libstubs.a

stubs-stdio.o: stubs-stdio.c
stubs-string.o: stubs-string.c
stubs-malloc.o: stubs-malloc.c
stubs-stdlib.o: stubs-stdlib.c
stubs-time.o: stubs-time.c
stubs-sys_timeb.o: stubs-sys_timeb.c
stubs-unistd.o: stubs-unistd.c
stubs-fcntl.o: stubs-fcntl.c
stubs-getopt.o: stubs-getopt.c
stubs-pwd.o: stubs-pwd.c
stubs-grp.o: stubs-grp.c
stubs-sys_stat.o: stubs-sys_stat.c
stubs-setjmp.o: stubs-setjmp.c
stubs-signal.o: stubs-signal.c
stubs-sys_resource.o: stubs-sys_resource.c
stubs-sys_wait.o: stubs-sys_wait.c
stubs-sys_utsname.o: stubs-sys_utsname.c
stubs-dirent.o: stubs-dirent.c
stubs-utime.o: stubs-utime.c
stubs-ioctl.o: stubs-ioctl.c
stubs-fnmatch.o: stubs-fnmatch.c
stubs-curses.o: stubs-curses.c
stubs-netinet_in.o: stubs-netinet_in.c
stubs-sys_time.o: stubs-sys_time.c
stubs-sys_socket.o: stubs-sys_socket.c
stubs-arpa_inet.o: stubs-arpa_inet.c
stubs-netdb.o: stubs-netdb.c
stubs-termios.o: stubs-termios.c
stubs-sys_uio.o: stubs-sys_uio.c

check-printf.o: check-printf.c check-printf.h
