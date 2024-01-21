#! /bin/make -f
#
#ident	"@(#)smail/pd/binmail:RELEASE-3_2:Makefile,v 1.9 1996/02/26 15:37:42 woods Exp"
#
# Makefile for binmail
#

SHELL=/bin/sh
MAKE=make
AR=ar
LINT=lint
SRC_PREFIX=

MKDEPEND=${ROOT}/conf/lib/mkdepend.sh
MKDEFS=${ROOT}/conf/lib/mkdefs.sh
CHECKDEFS=${ROOT}/conf/lib/checkdefs.sh
MKDIRS=${ROOT}/conf/lib/mkdefs.sh
INST=${ROOT}/conf/lib/inst.sh
INSTM=${ROOT}/conf/lib/instm.sh
XEXEC=${SHELL} ${ROOT}/conf/lib/xexec.sh
DEFS_SH=defs.sh
DEFS_H=defs.h
DEFS_SED=defs.sed

ROOT=../..
SMAILSRC=${ROOT}/src
# WARNING: only one target allowed here!
TARGETS=binmail
CSRC=binmail.c
MISCSRC=Makefile
SRC=${CSRC} ${MISCSRC}
OBJ=binmail.o

all:	${TARGETS}

binmail: binmail.o ${DEFS_SH}
	@. ./${DEFS_SH}; \
	   case "$$LMAIL" in \
	   '')	echo "LMAIL not defined in EDITME, not building binmail..."; \
		exit 0;; \
	   esac; \
	   ${XEXEC} $$CC $$CFLAGS $$LDFLAGS binmail.o -o binmail $$LIBS

binmail.o: binmail.c ${DEFS_SH}
	@. ./${DEFS_SH}; \
	   case "$$LMAIL" in \
	   '') exit 0;; \
	   esac; \
	   ${XEXEC} $$CC $$CFLAGS $$INCLUDES -c binmail.c

mkdefs ${DEFS_H} ${DEFS_SH} ${DEFS_SED}: ${ROOT}/conf/EDITME
	ROOT=${ROOT} ${SHELL} ${MKDEFS}

${ROOT}/conf/EDITME: # cannot depend on anything!
	cd ${ROOT}/conf; ${MAKE} EDITME

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

depend local_depend: ${SRC} check_defs
	. ./${DEFS_SH}; \
		${XEXEC} ${SHELL} ${MKDEPEND} $$CPPFLAGS $$INCLUDES \
		Makefile ${CSRC}
	. ./${DEFS_SH}; echo "$$DEFS_DEPEND" >> Makefile
	chmod -w Makefile

check_defs:
	SHELL=${SHELL} ROOT=${ROOT} ${SHELL} ${CHECKDEFS}

# install with care!
#
# NOTE: the original /bin/mail (actually, we use the pathname to
# $LMAIL to see were to install binmail) is renamed to /bin/mail.SAV,
# and then linked to /bin/`basename $LMAIL`, and binmail is installed
# to replace /bin/mail.
#
install: all ${DEFS_SH}
	@. ./${DEFS_SH}; \
	   case "$$DONT_INSTALL" in \
	   ?*)	echo Testing ... install ignored; exit 0;;\
	   esac; \
	   case "$$LMAIL" in \
	   '')	echo LMAIL not defined in EDITME, binmail not installed; \
		exit 0;;\
	   esac; \
	   ${SHELL} -c "if [ ! -f $$LMAIL -a -f `dirname $$LMAIL`/mail -a ! -f `dirname $$LMAIL`/mail.SAV ] ; then \
	     ${XEXEC} ln `dirname $$LMAIL`/mail `dirname $$LMAIL`/mail.SAV; \
	     ${XEXEC} mv `dirname $$LMAIL`/mail $$LMAIL; \
	     ${XEXEC} ${SHELL} ${INST} -m 0555 ${TARGETS} `dirname $$LMAIL`/mail; \
	   elif [ -f $$LMAIL -a -f `dirname $$LMAIL`/mail -a -f `dirname $$LMAIL`/mail.SAV ] ; then \
	     ${XEXEC} ${SHELL} ${INST} -m 0555 ${TARGETS} `dirname $$LMAIL`/mail; \
	   else \
	     echo Cannot find $$LMAIL and `dirname $$LMAIL`/mail.SAV, or just `dirname $$LMAIL`/mail; binmail not installed.; \
	     exit 0; \
	   fi"

clean:
	rm -f ${DEFS_SH} ${DEFS_H} ${DEFS_SED}
	rm -f ${OBJ} core a.out

clobber: clean
	rm -f ${TARGETS}

# DO NOT REMOVE THIS LINE, OR "make depend" WILL NOT WORK
