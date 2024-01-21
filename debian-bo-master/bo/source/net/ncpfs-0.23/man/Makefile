MAN1= slist nprint pqlist nsend pserver ncopy
MAN5= nwclient
MAN8= ncpmount ncpumount nwmsg



all:

dep:

install:
	for i in $(MAN1); do install $$i.1 -m 644 $(MANDIR)/man1; done
	for i in $(MAN5); do install $$i.5 -m 644 $(MANDIR)/man5; done
	for i in $(MAN8); do install $$i.8 -m 644 $(MANDIR)/man8; done

clean:
