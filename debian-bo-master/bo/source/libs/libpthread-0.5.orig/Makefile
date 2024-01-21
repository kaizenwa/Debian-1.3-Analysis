#### Configuration section

# Where to install

INCLUDEDIR=/usr/include
LIBDIR=/usr/lib
SHAREDLIBDIR=/lib
MANDIR=/usr/man/man3

# Compilation options

CC=gcc

CFLAGS=-pipe -O2 -Wall
# CFLAGS+=-g -DDEBUG # for debugging

PICCFLAGS=-fpic
PICLDFLAGS=-shared -Wl,-soname,$(shell echo $@ | sed 's/\.[^.]$$//')

# Define this as "yes" if you're using H.J.Lu's libc 5.2.18, 5.3.12, or 5.4.x
# (standard on most Linux distributions for Intel processors).
# Define this as "no" if you're using a different C library, e.g. glibc
# on the Alpha.

REENTRANT_LIBC_SUPPORT=yes

#### End of configuration section

# Determine architecture

ARCH:=$(shell uname -m | sed -e 's/i.86/i386/')

ifeq ($(ARCH),i386)
CFLAGS+=-m486
endif

CFLAGS+=-D__BUILDING_LINUXTHREADS -Isysdeps/$(ARCH)

# Contents of the library
OBJS=pthread.o manager.o attr.o join.o mutex.o condvar.o specific.o cancel.o \
  signals.o lockfile.o errno.o fork.o sleep.o semaphore.o

ifneq ($(wildcard sysdeps/$(ARCH)/clone.[cS]),)
OBJS+=clone.o
endif
ifneq ($(wildcard sysdeps/$(ARCH)/syscalls.[cS]),)
OBJS+=syscalls.o
endif

vpath %.c sysdeps/$(ARCH)
vpath %.S sysdeps/$(ARCH)

# The reentrant libc code (taken from libc-5.3.9)
ifeq ($(REENTRANT_LIBC_SUPPORT),yes)
vpath %.h libc_r
vpath %.c libc_r
CFLAGS+=-Ilibc_r -D_POSIX_THREADS
OBJS+=stdio.o getnetby_r.o getprotoby_r.o getservby_r.o \
       gethostby_r.o getpw_r.o malloc.o dirent.o
endif

LIB=libpthread.a
SHOBJS=$(OBJS:%.o=%.pic)
SHLIB=libpthread.so.0.5
SHLIB0=libpthread.so

all: $(LIB) $(SHLIB)
	cd man; $(MAKE) all

$(LIB): $(OBJS)
	ar rc $(LIB) $(OBJS)

$(SHLIB): $(SHOBJS)
	$(CC) $(PICLDFLAGS) -o $@ $(SHOBJS)

clean:
	rm -f $(LIB) $(SHLIB) *.o *.pic *~ libc_r/*~ sysdeps/*/*~
	cd man; $(MAKE) clean

install:
	install pthread.h $(INCLUDEDIR)/pthread.h
	install semaphore.h $(INCLUDEDIR)/semaphore.h
ifeq ($(REENTRANT_LIBC_SUPPORT),yes)
	test -f /usr/include/sched.h || install sched.h $(INCLUDEDIR)/sched.h
endif
	install $(LIB) $(LIBDIR)/$(LIB)
	install $(SHLIB) $(SHAREDLIBDIR)/$(SHLIB)
	rm -f $(LIBDIR)/$(SHLIB0)
	ln -s $(SHAREDLIBDIR)/$(SHLIB) $(LIBDIR)/$(SHLIB0)
	ldconfig -n $(SHAREDLIBDIR)
	cd man; $(MAKE) MANDIR=$(MANDIR) install

.SUFFIXES: .pic

%.pic: %.c
	$(CC) $(CFLAGS) $(PICCFLAGS) -c $< -o $@

%.pic: %.S
	$(CC) $(CFLAGS) $(PICCFLAGS) -c $< -o $@

depend:
	$(CC) $(CFLAGS) -MM *.c libc_r/*.c | \
        sed -e 's/^\(.*\)\.o:/\1.o \1.pic:/' \
            -e 's/sysdeps\/$(ARCH)/sysdeps\/$$(ARCH)/' > .depend

include .depend

