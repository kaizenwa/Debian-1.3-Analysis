CFLAGS=-O2 -fomit-frame-pointer -Wall
LDFLAGS=-s -N

BINDIR=/usr/lib/scsi
WISHEXECS=tk/cache tk/control tk/disconnect tk/error tk/format\
	tk/inquiry tk/notch tk/peripheral tk/rigid tk/save-changes tk/verify\
	tk/save-file tk/overview

scsiinfo: scsiinfo.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o scsiinfo scsiinfo.c

install: scsiinfo $(WISHEXECS) scsi-config
	cp scsiinfo /usr/bin
	if [ ! -d $(BINDIR) ]; then mkdir -p $(BINDIR); fi
	install-wish $(BINDIR) $(WISHEXECS)
	install-wish /usr/bin scsi-config
	cp tk/generic $(BINDIR)

dist:
	(mydir=`basename \`pwd\``;\
	cd .. && tar cvvf - $$mydir | gzip -9 > $${mydir}.tar.gz)
