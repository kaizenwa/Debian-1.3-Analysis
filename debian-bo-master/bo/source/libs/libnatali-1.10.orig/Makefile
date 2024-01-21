#
# Build the NetAtalk ALI library
#
# Last modified 19 February 1996.
#

CC=gcc -O -fomit-frame-pointer -Wall

libnatali.a: natali.o nbp_lookup.o nbp_parse.o nbp_register.o \
		nbp_remove.o pap_close.o pap_look.o pap_open.o \
		pap_read.o pap_sync.o pap_write.o paps_get_next.o \
		paps_open.o paps_status.o natali_pap.o
	rm -f libnatali.a
	ar -crs libnatali.a natali.o nbp_lookup.o nbp_parse.o \
		nbp_register.o nbp_remove.o pap_close.o pap_look.o \
		pap_open.o pap_read.o pap_sync.o pap_write.o \
		paps_get_next.o paps_open.o paps_status.o natali_pap.o

install: /usr/local/lib/libnatali.a /usr/local/include/at/appletalk.h \
		/usr/local/include/at/nbp.h /usr/local/include/at/pap.h

/usr/local/lib/libnatali.a: libnatali.a
	cp libnatali.a /usr/local/lib

/usr/local/include/at/appletalk.h: appletalk.h
	cp appletalk.h /usr/local/include/at

/usr/local/include/at/nbp.h: nbp.h
	cp nbp.h /usr/local/include/at

/usr/local/include/at/pap.h: pap.h
	cp pap.h /usr/local/include/at

natali.o: natali.c natali.h appletalk.h nbp.h pap.h
	$(CC) -c natali.c

natali_pap.o: natali_pap.c natali.h appletalk.h pap.h
	$(CC) -c natali_pap.c

nbp_lookup.o: nbp_lookup.c natali.h appletalk.h nbp.h
	$(CC) -c nbp_lookup.c

nbp_parse.o: nbp_parse.c natali.h appletalk.h nbp.h
	$(CC) -c nbp_parse.c

nbp_register.o: nbp_register.c natali.h appletalk.h nbp.h
	$(CC) -c nbp_register.c

nbp_remove.o: nbp_remove.c natali.h appletalk.h nbp.h
	$(CC) -c nbp_remove.c

pap_close.o: pap_close.c natali.h appletalk.h pap.h
	$(CC) -c pap_close.c

pap_look.o: pap_look.c natali.h appletalk.h pap.h
	$(CC) -c pap_look.c

pap_open.o: pap_open.c natali.h appletalk.h pap.h nbp.h
	$(CC) -c pap_open.c

pap_read.o: pap_read.c natali.h appletalk.h pap.h
	$(CC) -c pap_read.c

pap_sync.o: pap_sync.c natali.h appletalk.h pap.h
	$(CC) -c pap_sync.c

pap_write.o: pap_write.c natali.h appletalk.h pap.h
	$(CC) -c pap_write.c

paps_get_next.o: paps_get_next.c natali.h appletalk.h pap.h
	$(CC) -c paps_get_next.c

paps_open.o: paps_open.c natali.h appletalk.h pap.h
	$(CC) -c paps_open.c

paps_status.o: paps_status.c natali.h appletalk.h pap.h
	$(CC) -c paps_status.c

clean: 
	rm -f *.o *~ *.a

archive: clean
	cd ..; tar zcf natali-1.10.tar.gz natali-1.10/*

# end of file
