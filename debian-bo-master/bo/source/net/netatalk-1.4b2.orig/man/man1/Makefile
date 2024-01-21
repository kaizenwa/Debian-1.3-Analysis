SRC=	aecho.1 getzones.1 nbp.1 nbplkup.1 nbprgstr.1 nbplkup.1 \
	pap.1 papstatus.1 psorder.1 megatron.1 unhex.1 unbin.1 \
	unsingle.1 macbinary.1 hqx2bin.1 single2bin.1

INCPATH=
CFLAGS=
TAGSFILE=
CC=
INSTALL=	install

LINKS=

all :

install : all
	-mkdir ${MANDIR}/man1
	for i in ${SRC} ; do \
	    rm -f ${MANDIR}/man1/$$i; \
	    sed -e s@:DESTDIR:@${DESTDIR}@ -e s@:SBINDIR:@${SBINDIR}@ \
		-e s@:BINDIR:@${BINDIR}@ -e s@:RESDIR:@${RESDIR}@ \
		-e s@:ETCDIR:@${ETCDIR}@ -e s@:LIBDIR:@${LIBDIR}@ \
		-e s@:INCDIR:@${INCDIR}@ \
		< $$i > ${MANDIR}/man1/$$i; \
	done

clean :

tags : ${SRC}

depend :

# DO NOT DELETE THIS LINE

