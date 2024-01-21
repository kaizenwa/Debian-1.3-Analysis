#! /bin/make -f
#
#ident	"@(#)smail/util:RELEASE-3_2:Makefile,v 1.24 1996/06/21 21:39:37 woods Exp"
#
# Makefile for the smail utility tools
#
#    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
#    Copyright (C) 1992  Ronald S. Karr
# 
# See the file COPYING, distributed with smail, for restriction
# and warranty information.

SHELL=/bin/sh
MAKE=make
AR=ar
LINT=lint
SRC_PREFIX=

MKDEPEND=${ROOT}/conf/lib/mkdepend.sh
MKVERSION=${ROOT}/conf/lib/mkversion.sh
MKDEFS=${ROOT}/conf/lib/mkdefs.sh
CHECKDEFS=${ROOT}/conf/lib/checkdefs.sh
MKDIRS=${ROOT}/conf/lib/mkdirs.sh
INSTM=${ROOT}/conf/lib/instm.sh
INST=${ROOT}/conf/lib/inst.sh
XEXEC=${SHELL} ${ROOT}/conf/lib/xexec.sh
DEFS_SH=defs.sh
DEFS_H=defs.h
DEFS_SED=defs.sed
VERSION_SH=version.sh
VERSION_H=version.h
VERSION_SED=version.sed

ROOT=..
SMAILSRC=${ROOT}/src
UTILSRC=${ROOT}/util
INCLUDES=-I${SMAILSRC} $$DBM_INCLUDES $$INCLUDES
REMOTE_SRC=${SMAILSRC}/field.c ${SMAILSRC}/parse.c \
	   ${SMAILSRC}/ascii.c ${SMAILSRC}/string.c
LOCAL_SRC=field.c parse.c ascii.c string.c
LOCAL_OBJ=field.o parse.o ascii.o string.o
UTIL_TARGETS=mkline mksort dcasehost mkdbm mkpath \
	     pathmerge checkerr getmap gleem logsumm unsharmap savelog \
	     mkuuwho mkhpath smailbug
# this may not always be sane or necessary....
BIN_TARGETS=mkaliases
TARGETS=${UTIL_TARGETS} ${BIN_TARGETS}
STRIP_TARGETS=mkline mksort dcasehost mkdbm pathmerge gleem unsharmap
OBJ=mkline.o mksort.o dcasehost.o mkdbm.o pathmerge.o \
    gleem.o unsharmap.o ${LOCAL_OBJ}
CSRC=mkline.c mksort.c dcasehost.c mkdbm.c pathmerge.c gleem.c unsharmap.c
HSRC=dbm_compat.h
SHSRC=mkpath.sh checkerr.sh getmap.sh logsumm.sh mkaliases.sh \
      savelog.sh mkuuwho.sh mkhpath.sh smailbug.sh
AWK_SRC=mkpath.awk mkuuwho.awk
MISCSRC=Makefile
SRC=${CSRC} ${HSRC} ${SHSRC} ${AWK_SRC} ${MISCSRC}
COMPAT_LIB_DIR=${ROOT}/compat
COMPAT_LIB=${COMPAT_LIB_DIR}/compat.a

.c.o:
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS ${INCLUDES} -c $*.c

all: ${TARGETS}

mkdefs ${DEFS_H} ${DEFS_SH} ${DEFS_SED}: ${ROOT}/conf/EDITME
	ROOT=${ROOT} ${SHELL} ${MKDEFS}

${VERSION_H} ${VERSION_SH} ${VERSION_SED}:
	ROOT=${ROOT} ${SHELL} ${MKVERSION}

${ROOT}/conf/EDITME: # cannot depend on anything!
	cd ${ROOT}/conf; ${MAKE} EDITME

${COMPAT_LIB}:
	cd ${COMPAT_LIB_DIR}; ${MAKE}

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

install: all
	@. ./${DEFS_SH}; \
	   case "$$DONT_INSTALL" in \
	   ?*)	echo Testing ... install ignored; exit 0;; \
	   esac; \
	   l_flag=; \
	   case "$$USE_SYMLINKS" in \
	   ?*)	l_flag=-l;; \
	   esac; \
	   ${XEXEC} ${SHELL} ${MKDIRS} -m 0755 $$UTIL_BIN_DIR $$SMAIL_BIN_DIR \
				     $$LIB_DIR $$LIB_DIR/maps $$MAIN_SPOOL_DIR \
				     $$SPOOL_DIRS; \
	   case "$$UNSHAR_MAP_DIR" in \
	   ?*)	${XEXEC} ${SHELL} ${MKDIRS} -m 0755 $$UNSHAR_MAP_DIR \
					   $$UNSHAR_MAP_DIR/work;; \
	   esac; \
	   ${XEXEC} ${SHELL} ${INSTM} -m 0555 $$UTIL_BIN_DIR ${UTIL_TARGETS}; \
	   case $$UTIL_BIN_DIR in \
	   "$$SMAIL_BIN_DIR") x=;; \
	   *)	xx=$$SMAIL_BIN_DIR/mkaliases;; \
	   esac; \
	   ${XEXEC} ${SHELL} ${INST} -m 0555 $$l_flag -d $$UTIL_BIN_DIR \
		mkaliases mkaliases $$xx; \
	   ${XEXEC} ${SHELL} ${INSTM} -m 0444 $$UTIL_BIN_DIR ${AWK_SRC}; \
	   case "$$STRIP" in \
	   ?*)	for i in ${STRIP_TARGETS}; do \
			${XEXEC} strip $$UTIL_BIN_DIR/$$i; \
			if [ -x /usr/bin/mcs ]; then \
				${XEXEC} mcs -d $$UTIL_BIN_DIR/$$i; \
			fi; \
		done; \
	   esac

depend local_depend: check_defs
	-@for i in ${LOCAL_SRC}; do \
		if cmp -s $$i ${SMAILSRC}/$$i; then \
			: ; \
		else \
			${XEXEC} rm -f $$i; cp ${SMAILSRC}/$$i $$i; \
		fi; \
	done
	@. ./${DEFS_SH}; ${XEXEC} ${SHELL} ${MKDEPEND} $$CPPFLAGS ${INCLUDES} \
		Makefile ${CSRC} ${LOCAL_SRC}
	. ./${DEFS_SH}; echo "$$DEFS_DEPEND" >> Makefile
	chmod -w Makefile

check_defs:
	SHELL=${SHELL} ROOT=${ROOT} ${SHELL} ${CHECKDEFS}

mkline:	mkline.o string.o field.o parse.o ascii.o ${DEFS_SH} ${COMPAT_LIB}
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS $$LDFLAGS -o mkline mkline.o \
		string.o field.o parse.o ascii.o \
		$$LIBS ${COMPAT_LIB}

mksort: mksort.o ascii.o ${DEFS_SH} ${COMPAT_LIB}
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS $$LDFLAGS -o mksort mksort.o \
		ascii.o $$LIBS ${COMPAT_LIB}

dcasehost: dcasehost.o ascii.o ${DEFS_SH} ${COMPAT_LIB}
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS $$LDFLAGS -o dcasehost \
		dcasehost.o ascii.o $$LIBS ${COMPAT_LIB}

mkdbm: mkdbm.o ascii.o ${DEFS_SH} ${COMPAT_LIB}
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS $$LDFLAGS -o mkdbm mkdbm.o \
		ascii.o $$LIBS $$DBM_LIB ${COMPAT_LIB}

mkpath: mkpath.sh ${DEFS_SED}
	@-rm -f $@
	sed -f ${DEFS_SED} $@.sh > $@
	chmod +x $@

checkerr: checkerr.sh ${DEFS_SED}
	@-rm -f $@
	sed -f ${DEFS_SED} $@.sh > $@
	chmod +x $@

mkaliases: mkaliases.sh ${DEFS_SED}
	@-rm -f $@
	sed -f ${DEFS_SED} $@.sh > $@
	chmod +x $@

pathmerge: pathmerge.o ascii.o ${DEFS_SH} ${COMPAT_LIB}
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS $$LDFLAGS -o pathmerge \
		pathmerge.o ascii.o $$LIBS ${COMPAT_LIB}

getmap: getmap.sh ${DEFS_SED}
	@-rm -f $@
	sed -f ${DEFS_SED} $@.sh > $@
	chmod +x $@

logsumm: logsumm.sh ${DEFS_SED}
	@-rm -f $@
	sed -f ${DEFS_SED} $@.sh > $@
	chmod +x $@

mkuuwho: mkuuwho.sh ${DEFS_SED}
	@-rm -f $@
	sed -f ${DEFS_SED} $@.sh > $@
	chmod +x $@

savelog: savelog.sh ${DEFS_SED}
	@-rm -f $@
	sed -f ${DEFS_SED} $@.sh > $@
	chmod +x $@

smailbug: smailbug.sh ${DEFS_SED} ${VERSION_SED}
	@-rm -f $@
	sed -f ${DEFS_SED} $@.sh | sed -f ${VERSION_SED} > $@
	chmod +x $@

mkhpath: mkhpath.sh ${DEFS_SED}
	@-rm -f $@
	sed -f ${DEFS_SED} $@.sh > $@
	chmod +x $@

gleem: gleem.o ${DEFS_SH} ${COMPAT_LIB}
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS $$LDFLAGS -o gleem gleem.o \
		$$LIBS ${COMPAT_LIB}

unsharmap: unsharmap.o string.o ascii.o ${DEFS_SH} ${COMPAT_LIB}
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS $$LDFLAGS -o unsharmap \
		unsharmap.o string.o ascii.o $$LIBS ${COMPAT_LIB}

field.c: ${SMAILSRC}/field.c
	-rm -f $@
	cp $? $@

parse.c: ${SMAILSRC}/parse.c
	-rm -f $@
	cp $? $@

ascii.c: ${SMAILSRC}/ascii.c
	-rm -f $@
	cp $? $@

string.c: ${SMAILSRC}/string.c
	-rm -f $@
	cp $? $@

clean:
	rm -f ${OBJ} core a.out
	rm -f ${VERSION_SH} ${VERSION_H} ${VERSION_SED}
	rm -f ${DEFS_SH} ${DEFS_H} ${DEFS_SED} ${LOCAL_SRC} XMakefile

clobber: clean
	rm -f ${TARGETS}
	rm -f ${VERSION_SH} ${VERSION_H} ${VERSION_SED}
	rm -f .${DEFS_SH} .${DEFS_H} .${DEFS_SED} .Makefile

${OBJ}:	${DEFS_SH} ${DEFS_H}

# DO NOT REMOVE THIS LINE, OR "make depend" WILL NOT WORK
