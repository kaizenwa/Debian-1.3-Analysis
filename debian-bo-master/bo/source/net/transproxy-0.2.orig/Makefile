#
#	Makefile for tproxy.
#
#
# Currently supported OPTIONS flags are
#	DEBUG			Print some stuff to stdout.
#	DNS_LOOKUPS		Look up the hostnames via DNS to translate
#				the HTTP request.
#	USELESS_DNS_LOOKUPS	Look up the hostnames only needed for the log
#				files. Normally turned off.
#	PAY_THE_PENALTY		Slow down if they don't use proxy directly.
#

OPTIONS=-DDNS_LOOKUPS #-DUSELESS_DNS_LOOKUPS

CC=gcc
CFLAGS=-O2 -m486 -Wall $(OPTIONS)
LDFLAGS=-s
LIBS= #-lresolv

RM=rm -f
INSTALL=install

TARGETNAME=in.tproxyd

tproxy:		tproxy.o
	$(CC) $(LDFLAGS) tproxy.o -o $@ $(LIBS)

tproxy.o:	tproxy.c
	$(CC) $(CFLAGS) -c tproxy.c

clean:
	$(RM) tproxy.o

dist-clean:	clean
	$(RM) tproxy

install:	tproxy
	$(INSTALL) -o bin -g bin -m 555 tproxy /usr/sbin/$(TARGETNAME)

