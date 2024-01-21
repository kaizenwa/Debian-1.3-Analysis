CFLAGS = -O2 -g -Wall
UTILS = ipx_configure ipx_interface ipx_internal_net ipx_route

SBINDIR = $(prefix)/usr/sbin
MANDIR = $(prefix)/usr/man
CONFDIR = $(prefix)/etc
INITDIR = $(prefix)/etc/init.d

all: $(UTILS)
	
clean:
	rm -f $(UTILS) *.o rip sap ipxrcv ipxsend

install: $(UTILS)
	for i in $(UTILS); \
	do \
		install --strip $$i $(SBINDIR); \
		install $$i.8 $(MANDIR)/man8; \
	done
	install -m 0755 init.ipx $(INITDIR)/ipx
	install -m 0644 config.ipx $(CONFDIR)/ipx.conf

