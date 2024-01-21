# Root of installation. Subdirectories will be ${DESTDIR}/etc,
# ${DESTDIR}/bin, and ${DESTDIR}/lib.
DESTDIR=/usr/local/atalk

# for system-level binaries
SBINDIR=$(DESTDIR)/etc
# for user-level binaries
BINDIR=$(DESTDIR)/bin
# for program libraries (*.a)
LIBDIR=$(DESTDIR)/lib
# for machine-independent resources (pagecount.ps, etc.)
RESDIR=$(DESTDIR)/etc
# for configuration files (AppleVolumes.system, etc.)
ETCDIR=$(DESTDIR)/etc
# for include files
INCDIR=$(DESTDIR)/include
# Root of man pages.  Subdirectories will be
# ${MANDIR}/man1, ${MANDIR}/man4, and ${MANDIR}/man8.
MANDIR=$(DESTDIR)/man

#SBINDIR=/usr/sbin
#BINDIR=/usr/bin
#LIBDIR=/usr/lib
#RESDIR=/usr/lib/atalk
#ETCDIR=/etc
#INCDIR=/usr/include
#MANDIR=/usr/man

# Location of the AFS and Kerberos libraries and include files.  Uncomment
# and edit these if you want to include AFS or Kerberos support in afpd
# or Kerberos support in papd.
#AFSDIR=/usr/local/afs
#KRBDIR=/usr/local/kerberos

##########################################################################

all install depend clean tags kernel kinstall kpatch:	FRC
	@case `uname -rs` in \
	    "SunOS 4"*) ARCH=sunos \
		;; \
	    "SunOS 5"*) ARCH=solaris \
		;; \
	    ULTRIX*) ARCH=ultrix \
		;; \
	    Linux*) ARCH=linux \
		;; \
	    FreeBSD*) ARCH=freebsd \
		;; \
	    *) echo "Unknown machine type..."; exit 1 \
		;; \
	esac; \
	echo "Making $@ for $$ARCH..."; \
	cd sys/$$ARCH && ${MAKE} ${MFLAGS} \
	    SBINDIR="${SBINDIR}" BINDIR="${BINDIR}" RESDIR="${RESDIR}"\
	    ETCDIR="${ETCDIR}" LIBDIR="${LIBDIR}" INCDIR="${INCDIR}" \
	    DESTDIR="${DESTDIR}" MANDIR="${MANDIR}" \
	    AFSDIR="${AFSDIR}" KRBDIR="${KRBDIR}" $@

FRC:

SYS=sunos ultrix solaris
VERSION=dist
DISTDIR=../netatalk-${VERSION}

sysclean : FRC
	for i in ${SYS}; \
	    do (cd sys/$$i; ${MAKE} ${MFLAGS} sysclean); \
	done

dist : sysclean clean
	mkdir ${DISTDIR}
	tar cfFFX - EXCLUDE . | (cd ${DISTDIR}; tar xvf - )
	chmod +w ${DISTDIR}/Makefile ${DISTDIR}/include/atalk/paths.h \
		${DISTDIR}/sys/solaris/Makefile
	echo ${VERSION} > ${DISTDIR}/VERSION
