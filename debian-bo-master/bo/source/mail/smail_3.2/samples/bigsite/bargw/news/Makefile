#ident "@(#) Makefile,v 1.2 1990/10/24 05:20:34 tron Exp"
#
# update the map system
#
# Copy this makefile into /usr/lib/smail, for use with the daily cron
# job also given in this samples directory.

# standard locations
#
SHELL=/bin/sh
LIB_DIR=/usr/lib/smail
UTIL_BIN_DIR=/usr/lib/smail
MAPDIR=${LIB_DIR}/maps
PATH=/usr/lib/smail:/bin:/usr/bin:/usr/amdahl/bin
UNSHAR_MAP_DIR=/usr/spool/uumaps
TMPDIR=${UNSHAR_MAP_DIR}/tmp
WORKDIR=${UNSHAR_MAP_DIR}/work
BATCH=${WORKDIR}/batch
SORT=sort -T ${TMPDIR}

# what we need to build
UUWHOTARGETS=${LIB_DIR}/uuwho.dir ${LIB_DIR}/uuwho.pag
REBUILD=${WORKDIR}/getmap.rebuild
TARGETS=${REBUILD} ${UUWHOTARGETS} mkpath.target

# build it all
all: prep ${TARGETS} Makefile

# do some prep cleanup work
prep:
	@find ${LIB_DIR} -name 'dbm*' -mtime +1 -print | xargs /bin/rm -f

# build it in parts
map: ${REBUILD}
uuwho: ${UUWHOTARGETS}
path: mkpath.target

# get the maps
${REBUILD}: ${BATCH} Makefile
	@-if [ -s "${BATCH}" ]; then \
	    echo "	getmap -m ${UNSHAR_MAP_DIR} -w ${WORKDIR} -b ${BATCH}";\
	    getmap -m ${UNSHAR_MAP_DIR} -w ${WORKDIR} -b ${BATCH}; \
	    echo "	"touch ${BATCH}; \
	    touch ${BATCH}; \
	    chmod 0664 ${BATCH}; \
	    chgrp news ${BATCH}; \
	    echo "	"touch ${REBUILD}; \
	    touch ${REBUILD}; \
	else \
	    echo "	"touch ${REBUILD}; \
	    touch ${REBUILD}; \
	fi
${BATCH}: Makefile
	@-if [ ! -f ${BATCH} ]; then \
	    echo "	"touch ${BATCH}; \
	    touch ${BATCH}; \
	    chmod 0664 ${BATCH}; \
	    chgrp news ${BATCH}; \
	fi
${UUWHOTARGETS}: ${REBUILD} ${MAPDIR}/world.conf Makefile
	-rm -f ${LIB_DIR}/O.uuwho.dir ${LIB_DIR}/O.uuwho.pag
	touch ${LIB_DIR}/uuwho.dir ${LIB_DIR}/uuwho.pag
	ln ${LIB_DIR}/uuwho.dir ${LIB_DIR}/O.uuwho.dir
	ln ${LIB_DIR}/uuwho.pag ${LIB_DIR}/O.uuwho.pag
	mkuuwho -u ${LIB_DIR}/uuwho ${MAPDIR}/world.conf
mkpath.target: Makefile
	@cd ${MAPDIR}; $(MAKE) all

# utilities
clean:
	cd ${MAPDIR}; $(MAKE) clean
clobber: clean
	cd ${MAPDIR}; $(MAKE) clobber
	touch ${REBUILD}
nuke:
	cd ${MAPDIR}; $(MAKE) nuke
	-rm -f ${UUWHOTARGETS}
install: all Makefile
