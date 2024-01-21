# "make install" copia le pagine alla loro destinazione.
# fate "make gz" prima di "make install" se usate i sorgenti compressi.
# fate "make remove" prima di "make gz" se avete sorgenti non compressi
# di man page in giro.

MANDIR=/usr/man/it

example: remove gz install

remove:
	for i in man?; do for j in $$i/*; do rm -f $(MANDIR)/$$i/$$j; done; done

gz:
	for i in man?; do gzip $$i/*; done

install:
	for i in man?; do \
		install -d -m 755 $(MANDIR)/$$i; \
		install -m 644 $$i/* $(MANDIR)/$$i; \
	done
