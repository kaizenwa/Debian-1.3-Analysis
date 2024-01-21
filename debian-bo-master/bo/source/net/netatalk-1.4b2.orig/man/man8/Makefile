SRC=	afpd.8 atalkd.8 papd.8 psf.8

INCPATH=
CFLAGS=
TAGSFILE=
CC=
INSTALL=	install

LINKS=

all :

install : all
	-mkdir ${MANDIR}/man8
	for i in ${SRC} ; do \
	    rm -f ${MANDIR}/man8/$$i; \
	    sed -e s@:DESTDIR:@${DESTDIR}@ -e s@:SBINDIR:@${SBINDIR}@ \
		-e s@:BINDIR:@${BINDIR}@ -e s@:RESDIR:@${RESDIR}@ \
		-e s@:ETCDIR:@${ETCDIR}@ -e s@:LIBDIR:@${LIBDIR}@ \
		-e s@:INCDIR:@${INCDIR}@ \
		< $$i > ${MANDIR}/man8/$$i; \
	done

clean :

tags : ${SRC}

depend :

# DO NOT DELETE THIS LINE

