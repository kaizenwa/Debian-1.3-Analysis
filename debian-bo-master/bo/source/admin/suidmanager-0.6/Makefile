all:	suidexec

#suidexec:	suidexec.c
#	gcc -O5 -o suidexec suidexec.c

install:
	install -m644 suid.conf $(DESTDIR)/etc/suid.conf
	install suidregister suidunregister $(DESTDIR)/usr/sbin
#	install -m4755 suidexec $(DESTDIR)/usr/sbin

clean:
#	rm suidexec
