ALL=	asp atp nbp adouble util compat
LIBS=	asp/asplib atp/atplib nbp/nbplib adouble/adoublelib \
	util/utillib compat/compatlib
PLIBS=	asp/asplib_p atp/atplib_p nbp/nbplib_p adouble/adoublelib_p \
	util/utillib_p compat/compatlib_p
TAGSFILE=tags
CC=cc
INSTALL=install

all:	libatalk.a

libatalk.a: ${LIBS}
	rm -rf tmp
	-mkdir tmp
	for i in ${ALL}; do (cd tmp; ar x ../$$i/$${i}lib); done
	(cd tmp; ar cr libatalk.a  *.o; ranlib libatalk.a)
	mv tmp/libatalk.a libatalk.a
	ranlib libatalk.a
	rm -rf tmp tmp_p
	-mkdir tmp_p
	for i in ${ALL}; do (cd tmp_p; ar x ../$$i/$${i}lib_p); done
	(cd tmp_p; ar cr libatalk_p.a  *.o; ranlib libatalk_p.a)
	mv tmp_p/libatalk_p.a libatalk_p.a
	ranlib libatalk_p.a
	rm -rf tmp_p

${ALL}: FRC
	cd $@; ${MAKE} ${MFLAGS} CC="${CC}" DEFS="${DEFS}" \
	    OPTOPTS="${OPTOPTS}"

asp/asplib:	asp
atp/atplib:	atp
nbp/nbplib:	nbp
adouble/adoublelib:	adouble
util/utillib:	util
compat/compatlib:	compat

FRC:

tags:
	for i in ${ALL}; do \
	    (cd $$i; ${MAKE} ${MFLAGS} DEFS="${DEFS}" \
		TAGSFILE=../${TAGSFILE} tags); \
	done

install: all
	-mkdir ${LIBDIR}
	${INSTALL} -c libatalk.a ${LIBDIR}/libatalk.a
	(cd ${LIBDIR}; ranlib ${LIBDIR}/libatalk.a)
	${INSTALL} -c libatalk_p.a ${LIBDIR}/libatalk_p.a
	(cd ${LIBDIR}; ranlib ${LIBDIR}/libatalk_p.a)

clean:
	for i in ${ALL}; \
	    do (cd $$i; ${MAKE} ${MFLAGS} clean); \
	done
	rm -f *[Ee]rrs libatalk.a libatalk_p.a
	rm -rf tmp tmp_p

depend:
	for i in ${ALL}; \
	    do (cd $$i; ${MAKE} ${MFLAGS} DEFS=${DEFS} depend); \
	done
