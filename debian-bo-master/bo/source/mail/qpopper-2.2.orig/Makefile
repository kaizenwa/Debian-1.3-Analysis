#@(#)@(#)Makefile	2.5  2.5 4/3/91

DOCS		=	README pop3.rfc1081 pop3e.rfc1082 popper.8

INCLUDES	=	popper.h version.h

SRCS		=	${CSRCS} ${INCLUDES}

SCCS		=	/usr/ucb/sccs

REL		=

MAKEFILE	=	Makefile

TARGET		=	popper

TAR		=	${TARGET}.tar

INSTALLDIR	=	/tmp

MANPAGE		=	popper.8

CATPAGE		=	popper.0

MANDIR		=	/usr/local/man/cat8



all:
	@echo ""
	@echo "Enter make <your system> here"
	@echo "below are the currently supported system."
	@echo "Enter the one appropriate to your site."
	@echo ""
	@echo "    make sunos (4.x berkeley ish)"
	@echo "    make solaris2 (SVR4 w/Solaris 2.x specific mods)"
	@echo "    make 44bsd"
	@echo "    make bsdi"
	@echo "    make next"
	@echo "    make isc"
	@echo "    make aix"
	@echo "    make ultrix"
	@echo "    make sysv"
	@echo "    make hpux"
	@echo "    make hpux10 (OS version 10.x)"
	@echo "    make aux"
	@echo "    make unixware (NCR)"
	@echo "    make sco"
	@echo "    make scor5 (SCO Open Server 5)"
	@echo "    make ptx"
	@echo "    make irix"
	@echo "    make pyramid"
	@echo "    make linux"
	@echo "    make next"
	@echo "    make osf1"
	@echo ""

sunos:
	make -f make.sunos

solaris2:
	make -f make.solaris2

isc:
	make -f make.isc

aix:
	make -f make.aix

ultrix:
	make -f make.ultrix

sysv:
	make -f make.sysv

hpux:
	make -f make.hpux

hpux10:
	make -f make.hpux10

aux:
	make -f make.aux

irix:
	make -f make.irix

pyramid:
	make -f make.pyramid

sco:
	make -f make.sco

scor5:
	make -f make.scor5

ptx:
	make -f make.ptx

unixware:
	make -f make.unixware

linux:
	make -f make.linux

osf1:
	make -f make.osf1

44bsd:
	make -f make.44bsd

bsdi:
	make -f make.bsdi

next:
	make -f make.next

tar: ${SRCS} ${DOCS} ${MAKEFILE}
	rm -f ${TAR} *.Z*
	tar -cvf ${TAR} ${SRCS} ${DOCS} ${MAKEFILE}
	compress ${TAR}
	uuencode ${TAR}.Z ${TAR}.Z > ${TAR}.Z.uuencoded
	split -300 ${TAR}.Z.uuencoded
	mv xaa ${TAR}.Z.uuencoded.xaa
	mv xab ${TAR}.Z.uuencoded.xab
	mv xac ${TAR}.Z.uuencoded.xac
	mv xad ${TAR}.Z.uuencoded.xad
	mv xae ${TAR}.Z.uuencoded.xae

clean:
	rm -f core *.o *.Z*

installman:
	rm -f ${CATPAGE}
	nroff -man ${MANPAGE} > ${CATPAGE}
	install -c -m 644 -o root -g staff ${CATPAGE} ${MANDIR}

sources: ${SRCS}

${SRCS}:
	${SCCS} get ${REL} $@ -p | expand -4 > $@

${DOCS}:
	${SCCS} get README -p | expand -4 > README
	${SCCS} get popper.8 -p | expand -4 > popper.8

