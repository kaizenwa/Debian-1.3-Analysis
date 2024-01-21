BINDIR=/usr/local/bin
MANDIR=/usr/local/man
CFLAGS=-g # -DDEBUG

scsidev: scsidev.c
	${CC} ${CFLAGS} -o scsidev scsidev.c

install: scsidev
	cp scsidev $(BINDIR)
	cp scsidev.8 $(MANDIR)
	if [ ! -d /dev/scsi ]; then mkdir /dev/scsi; fi

dist:
	(mydir=`basename \`pwd\``;\
	cd .. && tar cvvf - $$mydir | gzip -9 > $${mydir}.tar.gz)

clean:
	rm -f *~ *.o scsidev
