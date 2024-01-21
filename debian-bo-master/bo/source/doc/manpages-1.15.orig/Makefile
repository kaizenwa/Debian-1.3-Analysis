# Do "make install" to copy the pages to their destination.
# Do "make gz" before "make install" if you use compressed source pages.
# We now unconditionally do "make remove".

MANDIR=/usr/man

# make this "gzip -9" to squeeze out the last byte
GZIP=gzip

example: remove gz install

remove:
	for i in man?; do for j in $$i/*; do \
		rm -f $(MANDIR)/$$j $(MANDIR)/$$j.gz; done; done
	touch remove

gz: remove
	for i in man?; do $(GZIP) $$i/*; done

install: remove
	for i in man?; do \
		install -d -m 755 $(MANDIR)/$$i; \
		install -m 644 $$i/* $(MANDIR)/$$i; \
	done
# someone might also want to look at /var/catman/cat2 or so ...
