#
#       @(#)Makefile.linux
#
# Makefile for rpc.pcnfsd for Linux
# Based on original 'Makefile.bsd' provided by Sun MicroSystems
#
CC=gcc

all: linux/rpc.pcnfsd linux/clnt.pcnfsd

LINUX_OBJS=  \
	linux/pcnfsd_svc.o \
	linux/pcnfsd_xdr.o \
	linux/pcnfsd_v1.o \
	linux/pcnfsd_v2.o \
	linux/pcnfsd_misc.o \
	linux/pcnfsd_cache.o \
	linux/pcnfsd_print.o

LINUX_SRCS=  \
	pcnfsd_svc.c \
	pcnfsd_xdr.c \
	pcnfsd_v1.c \
	pcnfsd_v2.c \
	pcnfsd_misc.c \
	pcnfsd_cache.c \
	pcnfsd_print.c

TST_OBJS= \
	linux/pcnfsd_clnt.o \
	linux/pcnfsd_test.o \
	linux/pcnfsd_xdr.o

TST_SRCS= \
	pcnfsd_clnt.c \
	pcnfsd_test.c \
	pcnfsd_xdr.c

HDRS= pcnfsd.h common.h

LINTFLAGS= -hbax
# uncomment as appropriate for debugging
CFLAGS = -DOSVER_BSD386 
#CFLAGS = -g -DDEBUG -DOSVER_BSD386
LIBS= #-lrpc
LFLAGS = 

linux/rpc.pcnfsd: $(LINUX_OBJS) $(HDRS)
	$(CC)  -o $@ $(CFLAGS) $(LFLAGS) $(LINUX_OBJS) $(LIBS)

linux/pcnfsd_svc.o: $(HDRS) pcnfsd_svc.c
	$(CC)  -c $(CFLAGS) -o $@ pcnfsd_svc.c

linux/pcnfsd_xdr.o: $(HDRS) pcnfsd_xdr.c
	$(CC)  -c $(CFLAGS) -o $@ pcnfsd_xdr.c

linux/pcnfsd_v1.o: $(HDRS) pcnfsd_v1.c
	$(CC)  -c $(CFLAGS) -o $@ pcnfsd_v1.c

linux/pcnfsd_v2.o: $(HDRS) pcnfsd_v2.c
	$(CC)  -c $(CFLAGS) -o $@ pcnfsd_v2.c

linux/pcnfsd_misc.o: $(HDRS) pcnfsd_misc.c
	$(CC)  -c $(CFLAGS) -o $@ pcnfsd_misc.c

linux/pcnfsd_cache.o: $(HDRS) pcnfsd_cache.c
	$(CC)  -c $(CFLAGS) -o $@ pcnfsd_cache.c

linux/pcnfsd_print.o: $(HDRS) pcnfsd_print.c
	$(CC)  -c $(CFLAGS) -o $@ pcnfsd_print.c


linux/clnt.pcnfsd: $(TST_OBJS) $(HDRS)
	$(CC)  -o $@ $(CFLAGS) $(LFLAGS) $(TST_OBJS) $(LIBS)

linux/pcnfsd_clnt.o: $(HDRS) pcnfsd_clnt.c
	$(CC)  -c $(CFLAGS) -o $@ pcnfsd_clnt.c

linux/pcnfsd_test.o: $(HDRS) pcnfsd_test.c
	$(CC)  -c $(CFLAGS) -o $@ pcnfsd_test.c


lint: $(SRCS)
	lint $(CFLAGS) $(LINTFLAGS) $(LINUX_SRCS)
	lint $(CFLAGS) $(LINTFLAGS) $(TST_SRCS)

clean:
	rm -rf linux

install:
	cp linux/rpc.pcnfsd	/etc
	cp linux/clnt.pcnfsd	/etc
	cp pcnfsd.8c		/usr/man/man8/
	echo	' '
	echo	'You shoul add '/etc/rpc.pcnfsd' to your '/etc/rc.net'
	echo	' '
