
##########################################################################

SRC = unix.c afs.c kuam.c send_to_kdc.c lifetime.c ofork.c \
	main.c switch.c auth.c volume.c directory.c file.c \
	enumerate.c desktop.c filedir.c fork.c appl.c gettok.c bprint.c
OBJ = unix.o afs.o kuam.o send_to_kdc.o lifetime.o ofork.o \
	main.o switch.o auth.o volume.o directory.o file.o \
	enumerate.o desktop.o filedir.o fork.o appl.o gettok.o bprint.o

INCPATH=	-I../../include ${AFSINCPATH} ${KRBINCPATH}
CFLAGS=	${DEFS} ${AFSDEFS} ${KRBDEFS} ${OPTOPTS} ${INCPATH} \
	-DAPPLCNAME -DCRLF # -DDOWNCASE
LIBS=	-latalk ${AFSLIBS} ${KRBLIBS} ${ADDLIBS}
LIBDIRS=	-L../../libatalk ${AFSLIBDIRS} ${KRBLIBDIRS}
TAGSFILE=	tags
CC=	cc
INSTALL=	install

all :
	if [ x"${KRBDIR}" != x ]; then \
	    KRBLIBS="-lkrb -ldes"; \
	    KRBLIBDIRS="-L${KRBDIR}/lib"; \
	    KRBINCPATH="-I${KRBDIR}/include"; \
	    KRBDEFS="-DKRB"; \
	fi; \
	if [ x"${AFSDIR}" != x ]; then \
	    AFSLIBS="-lkauth -lprot -lubik -lauth -lsys -lrxkad -lrx -laudit \
		-llwp -lcmd -lcom_err ${AFSDIR}/lib/afs/util.a -ldes"; \
	    AFSLIBDIRS="-L${AFSDIR}/lib -L${AFSDIR}/lib/afs"; \
	    AFSINCPATH="-I${AFSDIR}/include"; \
	    AFSDEFS="-DAFS"; \
	fi; \
	${MAKE} ${MFLAGS} CC="${CC}" ADDLIBS="${ADDLIBS}" DEFS="${DEFS}" \
	    OPTOPTS="${OPTOPTS}" DESTDIR="${DESTDIR}" \
	    SBINDIR="${SBINDIR}" BINDIR="${BINDIR}" RESDIR="${RESDIR}" \
	    ETCDIR="${ETCDIR}" LIBDIR="${LIBDIR}" INCDIR="${INCDIR}" \
	    KRBLIBS="$${KRBLIBS}" KRBLIBDIRS="$${KRBLIBDIRS}" \
	    KRBINCPATH="$${KRBINCPATH}" KRBDEFS="$${KRBDEFS}" \
	    AFSLIBS="$${AFSLIBS}" AFSLIBDIRS="$${AFSLIBDIRS}" \
	    AFSINCPATH="$${AFSINCPATH}" AFSDEFS="$${AFSDEFS}" \
	    afpd

afpd : ${OBJ} ../../libatalk/libatalk.a
	${CC} ${CFLAGS} ${LDFLAGS} -o afpd ${OBJ} ${LIBDIRS} ${LIBS}

main.o : main.c
	${CC} ${CFLAGS} \
	    -D_PATH_AFPDDEFVOL=\"${ETCDIR}/AppleVolumes.default\" \
	    -D_PATH_AFPDSYSVOL=\"${ETCDIR}/AppleVolumes.system\" \
	    -DVERSION=\"`cat ../../VERSION`\" \
	    ${CPPFLAGS} -c main.c

install : all
	${INSTALL} -c afpd ${SBINDIR}

clean :
	rm -f a.out core* *.o *.bak *[Ee]rrs tags
	rm -f afpd

tags : ${SRC}
	cwd=`pwd`; \
	for i in ${SRC}; do \
	    ctags -t -a -f ${TAGSFILE} $$cwd/$$i; \
	done

depend :
	for i in ${SRC} ; do \
	    ${CC} -M ${DEFS} ${INCPATH} $$i | \
	    awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec } ' >> makedep; done
	sed -n '1,/^# DO NOT DELETE THIS LINE/p' Makefile > Makefile.tmp
	cat makedep >> Makefile.tmp
	rm makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile.tmp
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile.tmp
	echo '# see make depend above' >> Makefile.tmp
	rm -f Makefile.bak
	cp Makefile Makefile.bak
	mv Makefile.tmp Makefile

# DO NOT DELETE THIS LINE

