#ident "@(#) Makefile,v 1.2 1990/10/24 05:20:10 tron Exp"
#
# Makefile for building smail router maps

# standard locations
#
SHELL=/bin/sh
LIB_DIR=/usr/lib/smail
UTIL_BIN_DIR=/usr/lib/smail
MAPDIR=.#${LIB_DIR}/maps
PATH=/usr/lib/smail:/bin:/usr/bin:/usr/amdahl/bin
UNSHAR_MAP_DIR=/usr/spool/uumaps
TMPDIR=${UNSHAR_MAP_DIR}/tmp
SORT=sort -T ${TMPDIR}
# on HDB UUCP, the file is .../Systems
# on old UUCP, the file is .../L.sys
LSYS=/usr/lib/uucp/Systems
#LSYS=/usr/lib/uucp/L.sys

# what we need to build
TARGETS=force.path domain.path world.path uuname.path world.dir world.pag
TESTTARGETS=T.force.path T.domain.path T.world.path T.uuname.path \
	    T.world.pag T.world.dir

# what is used to build these files
FORCEFILES=${MAPDIR}/force ${LSYS}
DOMAINFILES=${MAPDIR}/domain.conf ${MAPDIR}/domain.map /etc/hosts
WORLDFILES=${MAPDIR}/world.conf ${UNSHAR_MAP_DIR}/work/getmap.rebuild \
	${MAPDIR}/world.map ${MAPDIR}/private.map ${MAPDIR}/tweak.map
UUNAMEFILES=${LSYS} ${MAPDIR}/uuname.sed
SRCFILES=${FORCEFILES} ${DOMAINFILES} ${WORLDFILES} ${UUNAMEFILES}

# everything we need to make
#
all: Makefile ${TARGETS}
test: Makefile ${TESTTARGETS}

# build each path file
#
force.path: ${FORCEFILES} Makefile
	-rm -f ${MAPDIR}/N.force.path
	( sed -n -e 's/[	 ]*#.*//' -e 's/^[ 	][ 	]*//' \
	    -e '/./p' ${MAPDIR}/force; \
	    uuname | sed -e 's/\(.*\)/\1.uucp.foo.com	\1!%s/' ) \
	    | sort > ${MAPDIR}/N.force.path
	chmod 0444 ${MAPDIR}/N.force.path
	-rm -f ${MAPDIR}/O.force.path
	-touch ${MAPDIR}/force.path
	ln ${MAPDIR}/force.path ${MAPDIR}/O.force.path
	mv -f ${MAPDIR}/N.force.path ${MAPDIR}/force.path
domain.path: ${DOMAINFILES} Makefile
	-rm -f ${MAPDIR}/N.domain.path ${MAPDIR}/domain.err
	mkpath ${MAPDIR}/domain.conf > ${MAPDIR}/N.domain.path \
	    2> ${MAPDIR}/domain.err
	chmod 0444 ${MAPDIR}/N.domain.path
	-rm -f ${MAPDIR}/O.domain.path
	-touch ${MAPDIR}/domain.path
	ln ${MAPDIR}/domain.path ${MAPDIR}/O.domain.path
	mv -f ${MAPDIR}/N.domain.path ${MAPDIR}/domain.path
	@-if [ -s ${MAPDIR}/domain.err ]; then \
	    echo WARNING: domain.path did not build cleanly; \
	else \
	    echo "	"rm -f ${MAPDIR}/domain.err; \
	    rm -f ${MAPDIR}/domain.err; \
	fi
world.path: ${WORLDFILES} Makefile
	-rm -f ${MAPDIR}/N.world.path ${MAPDIR}/world.err
	mkpath ${MAPDIR}/world.conf > ${MAPDIR}/N.world.path \
	    2> ${MAPDIR}/world.err
	chmod 0444 ${MAPDIR}/N.world.path
	-rm -f ${MAPDIR}/O.world.path
	-touch ${MAPDIR}/world.path
	ln ${MAPDIR}/world.path ${MAPDIR}/O.world.path
	mv -f ${MAPDIR}/N.world.path ${MAPDIR}/world.path
	@-if [ -s ${MAPDIR}/world.err ]; then \
	    echo WARNING: world.path did not build cleanly; \
	else \
	    echo "	"rm -f ${MAPDIR}/world.err; \
	    rm -f ${MAPDIR}/world.err; \
	fi
world.dir world.pag: world.path
	mkdbm -o world world.path
	chmod 0444 world.dir world.pag
uuname.path: ${UUNAMEFILES} Makefile
	-rm -f ${MAPDIR}/N.uuname.path
	uuname | if [ -f ${MAPDIR}/uuname.sed ]; then \
			sed -f ${MAPDIR}/uuname.sed; \
		 else \
			cat; \
		 fi | sort | \
		sed -e 's/\(.*\)/\1	\1!%s/' > ${MAPDIR}/N.uuname.path
	chmod 0444 ${MAPDIR}/N.uuname.path
	-rm -f ${MAPDIR}/O.uuname.path
	-touch ${MAPDIR}/uuname.path
	ln ${MAPDIR}/uuname.path ${MAPDIR}/O.uuname.path
	mv -f ${MAPDIR}/N.uuname.path ${MAPDIR}/uuname.path

# test builds

# build each path file
#
T.force.path: ${FORCEFILES} Makefile
	-rm -f ${MAPDIR}/T.force.path
	( sed -n -e 's/[	 ]*#.*//' -e 's/^[ 	][ 	]*//' \
	    -e '/./p' ${MAPDIR}/force; \
	    uuname | sed -e 's/\(.*\)/\1.uucp.foo.com	\1!%s/' ) \
	    | sort > ${MAPDIR}/T.force.path
T.domain.path: ${DOMAINFILES} Makefile
	-rm -f ${MAPDIR}/T.domain.path
	mkpath ${MAPDIR}/domain.conf > ${MAPDIR}/T.domain.path \
	    2> ${MAPDIR}/domain.err
T.world.path: ${WORLDFILES} Makefile
	-rm -f ${MAPDIR}/T.world.path
	mkpath ${MAPDIR}/world.conf > ${MAPDIR}/T.world.path \
	    2> ${MAPDIR}/world.err
T.world.dir T.world.pag: T.world.path
	mkdbm -o T.world T.world.path
T.uuname.path: ${UUNAMEFILES} Makefile
	-rm -f ${MAPDIR}/T.uuname.path
	uuname | if [ -f ${MAPDIR}/uuname.sed ]; then \
			sed -f ${MAPDIR}/uuname.sed; \
		 else \
			cat; \
		 fi | sort | \
		sed -e 's/\(.*\)/\1	\1!%s/' > ${MAPDIR}/T.uuname.path

# utilities
install: all Makefile
clean:
	-rm -f ${MAPDIR}/N.uuname.path
	-rm -f ${MAPDIR}/N.world.path
	-rm -f ${MAPDIR}/N.domain.path
	-rm -f ${MAPDIR}/N.force.path
	-rm -f ${MAPDIR}/domain.err
	-rm -f ${MAPDIR}/world.err
	-rm -f ${TESTTARGETS}
clobber: clean
	-rm -f ${MAPDIR}/O.uuname.path
	-rm -f ${MAPDIR}/O.world.path
	-rm -f ${MAPDIR}/O.domain.path
	-rm -f ${MAPDIR}/O.force.path
nuke: clobber
	-rm -f ${TARGETS}
