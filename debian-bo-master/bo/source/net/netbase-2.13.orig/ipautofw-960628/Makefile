CC	= gcc
CFLAGS	= -Wall -O2
SBIN	= /usr/local/sbin

all:		ipautofw

ipautofw:
	$(CC) $(CFLAGS) -o ipautofw ipautofw-2.0.0.c

install:	ipautofw
	-mv $(SBIN)/ipautofw $(SBIN)/ipautofw.old
	cp ipautofw $(SBIN)/ipautofw

install-kernel:
	cp ipautofw-2.0.0.diff /usr/src
	cd /usr/src/linux
	patch -p1 < ../ipautofw-2.0.0.diff

archive:
	cd ../.. && tar -cf - ipautofw/ | gzip -9 > /root/scootdir/rlynch.ftp/ipautofw-`date +%y%m%d`.tar.gz

