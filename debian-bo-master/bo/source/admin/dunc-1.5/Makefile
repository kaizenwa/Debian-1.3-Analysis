all: dunc

bindir=$(DESTDIR)/usr/bin
mandir=$(DESTDIR)/usr/man
#docdir=$(DESTDIR)/usr/doc/dunc
version=1.5

clean:
	rm -rf *~

install:
	install -g root -o root -m 755 dunc $(bindir)
	perl -p -i -e 's/=V/$(version)/g' dunc.8
dunc: install
