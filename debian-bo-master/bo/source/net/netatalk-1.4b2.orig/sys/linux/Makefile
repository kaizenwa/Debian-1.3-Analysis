# Linux specific defines, passed to subdirectories.
DEFS=
OPTOPTS=	-O2 -fsigned-char
CC=	gcc
INSTALL=	install
ADDLIBS=

ALL=	../../libatalk ../../include ../../bin ../../etc ../../man

all:	${ALL}

../../bin ../../etc:	../../libatalk

${ALL}:	FRC
	cd $@; ${MAKE} ${MFLAGS} CC="${CC}" \
	    ADDLIBS="${ADDLIBS}" DEFS="${DEFS}" OPTOPTS="${OPTOPTS}" \
	    SBINDIR="${SBINDIR}" BINDIR="${BINDIR}" RESDIR="${RESDIR}" \
	    ETCDIR="${ETCDIR}" LIBDIR="${LIBDIR}" INCDIR="${INCDIR}" \
	    DESTDIR="${DESTDIR}" AFSDIR="${AFSDIR}" KRBDIR="${KRBDIR}" all

FRC:

install :
	-mkdir ${DESTDIR} ${SBINDIR} ${BINDIR} ${ETCDIR} ${LIBDIR}
	for i in ${ALL}; \
	    do (cd $$i; ${MAKE} ${MFLAGS} CC="${CC}" \
		ADDLIBS="${ADDLIBS}" DEFS="${DEFS}" OPTOPTS="${OPTOPTS}" \
	        SBINDIR="${SBINDIR}" BINDIR="${BINDIR}" RESDIR="${RESDIR}" \
	        ETCDIR="${ETCDIR}" LIBDIR="${LIBDIR}" INCDIR="${INCDIR}" \
		AFSDIR="${AFSDIR}" KRBDIR="${KRBDIR}" \
		INSTALL="${INSTALL}" $@); \
	done
	rm -f ${ETCDIR}/rc.atalk
	sed -e s@:DESTDIR:@${DESTDIR}@ -e s@:SBINDIR:@${SBINDIR}@ \
		-e s@:BINDIR:@${BINDIR}@ -e s@:RESDIR:@${RESDIR}@ \
		-e s@:ETCDIR:@${ETCDIR}@ -e s@:LIBDIR:@${LIBDIR}@ \
		-e s@:INCDIR:@${INCDIR}@ \
	    < ../../rc.atalk.bsd > ${ETCDIR}/rc.atalk
	chmod +x ${ETCDIR}/rc.atalk
	@echo
	@echo "Install is done.  Don't forget to add lines from"
	@echo "services.atalk to /etc/services and to call rc.atalk"
	@echo "in /etc/rc.  See README and README.LINUX for more"
	@echo "information."

clean :
	for i in ${ALL}; \
	    do (cd $$i; ${MAKE} ${MFLAGS} clean); \
	done

depend :
	for i in ${ALL}; \
	    do (cd $$i; ${MAKE} ${MFLAGS} DEFS=${DEFS} depend); \
	done

# DO NOT DELETE THIS LINE

