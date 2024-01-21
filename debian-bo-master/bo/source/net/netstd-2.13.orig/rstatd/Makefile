# Uncomment your choice of destination
DEST = /sbin
#DEST = /etc

OBJS = rstat_svc.o rstat_xdr.o rstat_prog.o
CFLAGS = -m486 -O2 -fomit-frame-pointer

# Uncomment this unless RPC functions are in libc
#RPCLIB = -lrpclib

all:rstatd librpcsvc.a

install:all
	cp librpcsvc.a /usr/lib
	cp rstat.h /usr/include
	cp rstatd $(DEST)/rpc.rstatd

rstatd:$(OBJS)
	$(CC) -o $@ $(OBJS) $(RPCLIB)

librpcsvc.a:rstat.o rstat_xdr.o
	ar crv librpcsvc.a rstat.o rstat_xdr.o

rstat.h rstat_clnt.c rstat_svc.c rstat_xdr.c:rstat.x
	rpcgen rstat.x

clean:
	-rm -f rstatd librpcsvc.a *.o
	-rm -f rstat.h rstat_clnt.c rstat_svc.c rstat_xdr.c
