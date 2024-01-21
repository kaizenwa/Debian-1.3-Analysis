CFLAGS=-O2 -I/usr/src/linux/include -I/usr/src/linux/drivers -fomit-frame-pointer

all: checkcd getcdtoc isiso isodetect isosize mycmp mydd readcdda playcdda \
	 mycdwrite resetcd isoinfo scsiinq getpartsize fdisk3.04 mymkisofs

checkcd: checkcd.c
	gcc $(CFLAGS) checkcd.c -o checkcd

getcdtoc: getcdtoc.c 
	gcc $(CFLAGS) getcdtoc.c -o getcdtoc 

isiso: isiso.c
	gcc $(CFLAGS) isiso.c -o isiso

isodetect: isodetect.c
	gcc $(CFLAGS) isodetect.c -o isodetect

isosize: isosize.c
	gcc $(CFLAGS) isosize.c -o isosize

mycmp: mycmp.c
	gcc $(CFLAGS) mycmp.c -o mycmp

mydd: mydd.c
	gcc $(CFLAGS) mydd.c -o mydd

readcdda: readcdda.c
	gcc $(CFLAGS) readcdda.c -o readcdda

playcdda: playcdda.c 
	gcc $(CFLAGS) playcdda.c -o playcdda

mycdwrite: mycdwrite.c
	gcc $(CFLAGS) mycdwrite.c -o mycdwrite

resetcd: resetcd.c
	gcc $(CFLAGS) resetcd.c -o resetcd

isoinfo: isoinfo.c
	gcc $(CFLAGS) isoinfo.c -o isoinfo

scsiinq: scsiinq.c
	gcc $(CFLAGS) scsiinq.c -o scsiinq 

getpartsize: getpartsize.c
	gcc $(CFLAGS) getpartsize.c -o getpartsize 

fdisk3.04: fdisk3.04.c
	gcc $(CFLAGS) fdisk3.04.c -o fdisk3.04

mymkisofs:
	cd mkisofs-1.10; configure; make

install:
	mv checkcd getcdtoc isiso isodetect isosize mycmp mydd readcdda \
	playcdda mycdwrite resetcd isoinfo scsiinq getpartsize fdisk3.04 ../bin
	mv mkisofs-1.10/mymkisofs ../bin
	
clean:
	rm -f *.o core checkcd getcdtoc isiso isodetect isosize mycmp mydd \
	readcdda playcdda mycdwrite resetcd isoinfo scsiinq getpartsize fdisk3.04
	cd mkisofs-1.10; make clean
