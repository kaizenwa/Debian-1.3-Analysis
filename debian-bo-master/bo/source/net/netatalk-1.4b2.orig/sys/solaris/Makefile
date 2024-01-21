# Solaris specific defines, passed to subdirectories.
# To use Sun CC, uncomment the CC and KFLAGS variables.

# use gcc
CC=		gcc
KCFLAGS=	-D_KERNEL -mno-app-regs -munaligned-doubles -fpcc-struct-return

# use Sun CC
#CC=		cc
#KCFLAGS=	-D_KERNEL

OPTOPTS=	-O
DEFS=	-D__svr4__ -DSOLARIS
INSTALL=	/usr/ucb/install
ADDLIBS=	-lsocket -lnsl

# Local build stuff.

SRC= aarp.c ddp.c ddptp_rdq.c ddptp_utls.c dlpi.c tpi.c utils.c
OBJ= aarp.o ddp.o ddptp_rdq.o ddptp_utls.o dlpi.o tpi.o utils.o

INCPATH=	-I../../include -I../netatalk
CFLAGS=	${KCFLAGS} ${DEFS} ${OPTOPTS} ${INCPATH}

ALL=	../../libatalk ../../include ../../bin ../../etc ../../man

all :	kernel ${ALL}

kernel :	ddp

ddp :	${OBJ}
	ld -r -o ddp ${OBJ}

../../bin ../../etc:	../../libatalk

${ALL}:	FRC
	cd $@; ${MAKE} ${MFLAGS} CC="${CC}" \
	    ADDLIBS="${ADDLIBS}" DEFS="${DEFS}" OPTOPTS="${OPTOPTS}" \
	    SBINDIR="${SBINDIR}" BINDIR="${BINDIR}" RESDIR="${RESDIR}" \
	    ETCDIR="${ETCDIR}" LIBDIR="${LIBDIR}" INCDIR="${INCDIR}" \
	    DESTDIR="${DESTDIR}" AFSDIR="${AFSDIR}" KRBDIR="${KRBDIR}" all

FRC:

kinstall :
	-rem_drv ddp
	${INSTALL} -c ddp /usr/kernel/drv/ddp
	${INSTALL} -c ddp.conf /usr/kernel/drv/ddp.conf
	rm -f /usr/kernel/strmod/ddp
	ln /usr/kernel/drv/ddp /usr/kernel/strmod/ddp
	add_drv ddp
	rm -f /etc/init.d/atalk /etc/rc2.d/S79atalk /etc/rc0.d/K79atalk
	sed -e s@:DESTDIR:@${DESTDIR}@ -e s@:SBINDIR:@${SBINDIR}@ \
		-e s@:BINDIR:@${BINDIR}@ -e s@:RESDIR:@${RESDIR}@ \
		-e s@:ETCDIR:@${ETCDIR}@ -e s@:LIBDIR:@${LIBDIR}@ \
		-e s@:INCDIR:@${INCDIR}@ \
	    < ../../rc.atalk.sysv > /etc/init.d/atalk
	ln -s ../init.d/atalk /etc/rc2.d/S79atalk
	ln -s ../init.d/atalk /etc/rc0.d/K79atalk
	sync;sync;sync

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
	@echo
	@echo "Install is done.  Don't forget to add lines from"
	@echo "services.atalk to /etc/services and to call rc.atalk"
	@echo "in /etc/rc.  See README and README.SOLARIS for more"
	@echo "information."

clean : sysclean
	for i in ${ALL}; \
	    do (cd $$i; ${MAKE} ${MFLAGS} clean); \
	done

sysclean :
	rm -f a.out core* *.o *.bak *[Ee]rrs tags
	rm -f ddp

depend :
	for i in ${ALL}; \
	    do (cd $$i; ${MAKE} ${MFLAGS} DEFS=${DEFS} depend); \
	done

# DO NOT DELETE THIS LINE

