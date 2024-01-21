# Where to get named resolver routines, if not in libc.
LIBRESOLV=-lresolv

# For my debugging malloc.  If you don't have it, comment MALLOC= out.
MYLIB=$(HOME)/lib.$(HOSTTYPE)
#MALLOC=$(MYLIB)/libmalloc_d.a

#CC = gcc -traditional
BINDIR = /usr/bin
LIBDIR = /usr/lib
SHAREDIR = /usr/share
MANEXT = 1
MANDIR = /usr/man/man${MANEXT}
CDEBUGFLAGS =-g
DEFINES=
INCLUDES= -I../../support -I../include -I../../include 
LIBS=$(LIBCNEWS) $(MALLOC) $(LIBRESOLV)

CFLAGS = ${CDEBUGFLAGS} ${INCLUDES} ${DEFINES}
LINTFLAGS = ${INCLUDES} ${DEFINES} -u
LDFLAGS = ${CDEBUGFLAGS} ${LIBS}

# Location of the C News library and include for this machine.
#LIBCNEWS=/usr/src/news/c-news/libcnews.a
#CNEWSINCLUDE=/usr/src/news/c-news/include

LIBOBJS= bicat.o disconnect.o ipc.o nserrs.o passfd.43.o tcpopen.o unixopen.o
LIBSRCS=
LIBMISC=../lib.a

all: $(LIBMISC)

$(LIBMISC): ${LIBOBJS}
	ar ruv ${LIBMISC} ${LIBOBJS}
	-ranlib ${LIBMISC}

install: ${PROG}
	rm -f ${BINDIR}/${PROG}
	install -s -m 755 ${PROG} ${BINDIR}
	install -c -m 444 ${PROG}.man ${MANDIR}/${PROG}.${MANEXT}

test:
	./dotest < testbatch

clean:
	rm -f *.o core make.out make.log a.out \#* *~ ${PROG} ${PROG2} ${PROG3} ${PROG4}
	rm -f test/spool/in.coming/*

wc:
	-@echo Library; wc ${LIBSRCS}; echo ""
	-@echo ${PROG}; wc ${SRCS}; echo ""
	-@echo ${PROG2}; wc ${SRCS2}; echo ""
	-@echo ${PROG3}; wc ${SRCS3}; echo ""
	-@echo ${PROG4}; wc ${SRCS4}; echo ""

depend:
	mkdep ${DEFINES} ${INCLUDES} ${LIBSRCS}
