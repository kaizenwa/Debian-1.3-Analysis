ALL=	afpd atalkd papd psf
TAGSFILE=tags
CC=cc
INSTALL=	install

all:	${ALL}

${ALL}: FRC
	cd $@; ${MAKE} ${MFLAGS} CC="${CC}" \
	    ADDLIBS="${ADDLIBS}" DEFS="${DEFS}" OPTOPTS="${OPTOPTS}" \
	    SBINDIR="${SBINDIR}" BINDIR="${BINDIR}" RESDIR="${RESDIR}" \
	    ETCDIR="${ETCDIR}" LIBDIR="${LIBDIR}" INCDIR="${INCDIR}" \
	    DESTDIR="${DESTDIR}" AFSDIR="${AFSDIR}" KRBDIR="${KRBDIR}"

FRC:

tags:
	for i in ${ALL}; do \
	    (cd $$i; ${MAKE} ${MFLAGS} DEFS="${DEFS}" \
		TAGSFILE=../${TAGSFILE} tags); \
	done

install:
	-mkdir ${ETCDIR}
	for i in ${ALL}; \
	    do (cd $$i; ${MAKE} ${MFLAGS} CC="${CC}" \
		ADDLIBS="${ADDLIBS}" DEFS="${DEFS}" OPTOPTS="${OPTOPTS}" \
	        SBINDIR="${SBINDIR}" BINDIR="${BINDIR}" RESDIR="${RESDIR}" \
	        ETCDIR="${ETCDIR}" LIBDIR="${LIBDIR}" INCDIR="${INCDIR}" \
		DESTDIR="${DESTDIR}" AFSDIR="${AFSDIR}" KRBDIR="${KRBDIR}" \
		INSTALL="${INSTALL}" install); \
	done

clean:
	for i in ${ALL}; \
	    do (cd $$i; ${MAKE} ${MFLAGS} clean); \
	done

depend:
	for i in ${ALL}; \
	    do (cd $$i; ${MAKE} ${MFLAGS} DEFS=${DEFS} depend); \
	done
