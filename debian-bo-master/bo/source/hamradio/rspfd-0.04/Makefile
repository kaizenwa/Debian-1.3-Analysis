#Makefile for RSPFd
CFLAGS=-Wall -ggdb -DFUNC_DEBUG
# Add -lefence if you have Electric Fence and want to debug.
# LDFLAGS=-lefence
RSPFD_CONF=/etc/rspfd.conf
OBJECTS= rspfd.o queue.o deltaq.o rspfif.o rspfax25.o rspftcp.o rspf_out.o \
	rspfroute.o

all:	rspfd

.c.o:
	$(CC) $(CFLAGS) -c $<

rspfd: $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -o rspfd

clean:
	rm -f *.o rspfd rspfc
	
install:
	install -g root -m 0755 rspfd /usr/sbin/rspfd
	install -o root -m 0755 rspfd.conf $(RSPFD_CONF)
