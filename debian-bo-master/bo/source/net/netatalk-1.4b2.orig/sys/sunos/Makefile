# Sun specific defines, passed to subdirectories.
DEFS=
OPTOPTS=	-O
CC=	cc
INSTALL=	install
ADDLIBS=

# source for kernel module
SRC=	at_sun.c aarp.c at_control.c at_proto.c ddp_input.c ddp_output.c \
	ddp_usrreq.c
OBJ=	at_sun.o aarp.o at_control.o at_proto.o ddp_input.o ddp_output.o \
	ddp_usrreq.o

INCPATH = -I../../include -I../netatalk
CFLAGS=	${DEFS} ${OPTOPTS} ${INCPATH}

ALL=	../../libatalk ../../include ../../bin ../../etc ../../man

all:	kernel ${ALL}

kernel : netatalk.o

netatalk.o:	${OBJ}
	${LD} -r -o netatalk.o ${OBJ}

at_sun.o:	at_sun.c
	${CC} ${CFLAGS} -DKERNEL -D`arch -k` -c at_sun.c

aarp.o:	../netatalk/aarp.c
	${CC} ${CFLAGS} -DKERNEL -D`arch -k` -c ../netatalk/aarp.c

at_control.o:	../netatalk/at_control.c
	${CC} ${CFLAGS} -DKERNEL -D`arch -k` -c ../netatalk/at_control.c

at_proto.o:	../netatalk/at_proto.c
	${CC} ${CFLAGS} -DKERNEL -D`arch -k` -c ../netatalk/at_proto.c

ddp_input.o:	../netatalk/ddp_input.c
	${CC} ${CFLAGS} -DKERNEL -D`arch -k` -c ../netatalk/ddp_input.c

ddp_output.o:	../netatalk/ddp_output.c
	${CC} ${CFLAGS} -DKERNEL -D`arch -k` -c ../netatalk/ddp_output.c

ddp_usrreq.o:	../netatalk/ddp_usrreq.c
	${CC} ${CFLAGS} -DKERNEL -D`arch -k` -c ../netatalk/ddp_usrreq.c

../../bin ../../etc:	../../libatalk

${ALL}:	FRC
	cd $@; ${MAKE} ${MFLAGS} CC="${CC}" \
	    ADDLIBS="${ADDLIBS}" DEFS="${DEFS}" OPTOPTS="${OPTOPTS}" \
	    SBINDIR="${SBINDIR}" BINDIR="${BINDIR}" RESDIR="${RESDIR}" \
	    ETCDIR="${ETCDIR}" LIBDIR="${LIBDIR}" INCDIR="${INCDIR}" \
	    DESTDIR="${DESTDIR}" AFSDIR="${AFSDIR}" KRBDIR="${KRBDIR}" all

FRC:

kinstall :	kernel
	-mkdir ${DESTDIR}
	-mkdir ${ETCDIR}
	${INSTALL} -c netatalk.o ${ETCDIR}

install :	kinstall
	-mkdir ${DESTDIR}
	-mkdir ${SBINDIR}
	-mkdir ${BINDIR}
	-mkdir ${ETCDIR}
	-mkdir ${LIBDIR}
	for i in ${ALL}; \
	    do (cd $$i; ${MAKE} ${MFLAGS} CC="${CC}" \
		ADDLIBS="${ADDLIBS}" DEFS="${DEFS}" OPTOPTS="${OPTOPTS}" \
	        SBINDIR="${SBINDIR}" BINDIR="${BINDIR}" RESDIR="${RESDIR}" \
	        ETCDIR="${ETCDIR}" LIBDIR="${LIBDIR}" INCDIR="${INCDIR}" \
		DESTDIR="${DESTDIR}" MANDIR="${MANDIR}" \
		AFSDIR="${AFSDIR}" KRBDIR="${KRBDIR}" \
		INSTALL="${INSTALL}" $@); \
	done
	rm -f ${ETCDIR}/rc.atalk
	sed -e s@:DESTDIR:@${DESTDIR}@ -e s@:SBINDIR:@${SBINDIR}@ \
		-e s@:BINDIR:@${BINDIR}@ -e s@:RESDIR:@${RESDIR}@ \
		-e s@:ETCDIR:@${ETCDIR}@ -e s@:LIBDIR:@${LIBDIR}@ \
		-e s@:INCDIR:@${INCDIR}@ -e 's@^##@@' \
	    < ../../rc.atalk.bsd > ${ETCDIR}/rc.atalk
	@echo
	@echo "Install is done.  Don't forget to add lines from"
	@echo "services.atalk to /etc/services and to call rc.atalk"
	@echo "in /etc/rc.  See README and README.SUN for more"
	@echo "information."

clean : sysclean
	for i in ${ALL}; \
	    do (cd $$i; ${MAKE} ${MFLAGS} clean); \
	done

sysclean :
	rm -f a.out core* *.o *.bak *[Ee]rrs tags
	rm -f netatalk.o

depend :
	for i in ${ALL}; \
	    do (cd $$i; ${MAKE} ${MFLAGS} DEFS=${DEFS} depend); \
	done
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

