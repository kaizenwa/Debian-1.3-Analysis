
SCRIPTS = dpkg-cross dpkg-buildpackage dpkg-shlibdeps
MAN1 = dpkg-cross.1
MAN5 = cross-compile.5
DESTDIR = /

all: $(SCRIPTS)

clean:
	rm -f $(SCRIPTS)

install:
	install -m755 -o root -g root $(SCRIPTS) $(DESTDIR)/usr/bin
	install -m644 -o root -g root $(MAN1) $(DESTDIR)/usr/man/man1
	install -m644 -o root -g root $(MAN5) $(DESTDIR)/usr/man/man5

%: %.in
	sed -f conf.sed < $^ > $@

