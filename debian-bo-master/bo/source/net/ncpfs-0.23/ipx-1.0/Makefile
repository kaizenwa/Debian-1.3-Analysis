CFLAGS = -O2 -Wall
UTILS = $(INTERM_BINDIR)/ipx_configure $(INTERM_BINDIR)/ipx_interface \
	$(INTERM_BINDIR)/ipx_internal_net $(INTERM_BINDIR)/ipx_route

MAN8 = ipx_configure ipx_interface ipx_internal_net ipx_route

all: $(UTILS)

$(INTERM_BINDIR)/ipx_configure: ipx_configure.o
	$(CC) -o $(INTERM_BINDIR)/ipx_configure ipx_configure.o

$(INTERM_BINDIR)/ipx_interface: ipx_interface.o
	$(CC) -o $(INTERM_BINDIR)/ipx_interface ipx_interface.o

$(INTERM_BINDIR)/ipx_internal_net: ipx_internal_net.o
	$(CC) -o $(INTERM_BINDIR)/ipx_internal_net ipx_internal_net.o

$(INTERM_BINDIR)/ipx_route: ipx_route.o
	$(CC) -o $(INTERM_BINDIR)/ipx_route ipx_route.o

dep:
	$(CPP) -M $(INCLUDES) *.c > .depend

clean:
	rm -f $(UTILS) *.o rip sap ipxrcv ipxsend

install: $(UTILS)
	for i in $(UTILS); \
	do \
		install $$i $(SBINDIR); \
	done

	for i in $(MAN8); do install ../man/$$i.8 -m 755 $(MANDIR)/man8; done
