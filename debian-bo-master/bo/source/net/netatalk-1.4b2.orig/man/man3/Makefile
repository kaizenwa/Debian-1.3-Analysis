SRC=	atalk_aton.3 nbp_name.3

INCPATH=
CFLAGS=
TAGSFILE=
CC=
INSTALL=	install

LINKS=

all :

install : all
	-mkdir ${MANDIR}/man3
	for i in ${SRC} ; do \
	    rm -f ${MANDIR}/man3/$$i; \
	    sed -e s@:DESTDIR:@${DESTDIR}@ -e s@:SBINDIR:@${SBINDIR}@ \
		-e s@:BINDIR:@${BINDIR}@ -e s@:RESDIR:@${RESDIR}@ \
		-e s@:ETCDIR:@${ETCDIR}@ -e s@:LIBDIR:@${LIBDIR}@ \
		-e s@:INCDIR:@${INCDIR}@ \
		< $$i > ${MANDIR}/man3/$$i; \
	done

clean :

tags : ${SRC}

depend :

# DO NOT DELETE THIS LINE

