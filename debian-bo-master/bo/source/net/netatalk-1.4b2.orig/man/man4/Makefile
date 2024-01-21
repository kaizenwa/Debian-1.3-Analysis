SRC=	atalk.4
INCPATH=
CFLAGS=
TAGSFILE=
CC=
INSTALL=	install

LINKS=

all :

install : all
	-mkdir ${MANDIR}/man4
	for i in ${SRC} ; do \
	    rm -f ${MANDIR}/man4/$$i; \
	    sed -e s@:DESTDIR:@${DESTDIR}@ -e s@:SBINDIR:@${SBINDIR}@ \
		    -e s@:BINDIR:@${BINDIR}@ -e s@:RESDIR:@${RESDIR}@ \
		    -e s@:ETCDIR:@${ETCDIR}@ -e s@:LIBDIR:@${LIBDIR}@ \
		    -e s@:INCDIR:@${INCDIR}@ \
		    < $$i > ${MANDIR}/man4/$$i; \
	done

clean :

tags : ${SRC}

depend :

# DO NOT DELETE THIS LINE

