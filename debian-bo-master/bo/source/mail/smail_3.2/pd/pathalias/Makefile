#! /bin/make -f
#
#ident	"@(#)smail/pd/pathalias:RELEASE-3_2:Makefile,v 1.16 1996/02/26 15:38:15 woods Exp"
#
# pathalias -- by steve bellovin, as told to peter honeyman

# NOTE: this does not build arpatxt, to avoid requiring regular
#	expressions for generic builds.

SHELL=/bin/sh
MAKE=make
AR=ar
LINT=lint
SRC_PREFIX=

MKDEPEND=${ROOT}/conf/lib/mkdepend.sh
MKDEFS=${ROOT}/conf/lib/mkdefs.sh
CHECKDEFS=${ROOT}/conf/lib/checkdefs.sh
MKDIRS=${ROOT}/conf/lib/mkdirs.sh
INST=${ROOT}/conf/lib/inst.sh
INSTM=${ROOT}/conf/lib/instm.sh
XEXEC=${SHELL} ${ROOT}/conf/lib/xexec.sh
DEFS_SH=defs.sh
DEFS_H=defs.h
DEFS_SED=defs.sed

ROOT=../..
UTILSRC=${ROOT}/util
INCLUDES=-I${UTILSRC} $$DBM_INCLUDES $$INCLUDES
CFLAGS = -DSTATIC=static $$CFLAGS
YFLAGS = -d
OBJ = addlink.o addnode.o local.o main.o mapit.o mapaux.o mem.o parse.o printit.o
HDRS = def.h config.h
CSRC = addlink.c addnode.c local.c main.c mapit.c mapaux.c mem.c printit.c
MORE_OBJ=makedb.o arpatxt.o
MORE_CSRC=makedb.c arpatxt.c
MISCSRC=Makefile CHANGES README arpa-privates
YSRC=parse.y
SRC = $(CSRC) $(HDRS) $(MISCSRC) $(YSRC) $(MORE_CSRC)
LSRC = $(CSRC) parse.c
TARGETS = pathalias makedb # arpatxt
COMPAT_LIB_DIR=${ROOT}/compat
COMPAT_LIB=${COMPAT_LIB_DIR}/compat.a

all: ${TARGETS}

pathalias: $(OBJ) ${COMPAT_LIB}
	@. ./${DEFS_SH}; ${XEXEC} $$CC ${CFLAGS} $$LDFLAGS ${OBJ} ${GETOPT} \
		-o pathalias $$LIBS $$DBM_LIB ${COMPAT_LIB}

${COMPAT_LIB}:
	cd ${COMPAT_LIB_DIR}; ${MAKE}

mkdefs ${DEFS_H} ${DEFS_SH} ${DEFS_SED}: ${ROOT}/conf/EDITME
	ROOT=${ROOT} ${SHELL} ${MKDEFS}

${ROOT}/conf/EDITME: # cannot depend on anything!
	cd ${ROOT}/conf; ${MAKE} EDITME

.c.o:
	@. ./${DEFS_SH}; ${XEXEC} $$CC ${CFLAGS} ${INCLUDES} -c $*.c

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

${OBJ} ${MORE_OBJ}: ${DEFS_SH}
	@. ./${DEFS_SH}; ${XEXEC} $$CC ${CFLAGS} ${INCLUDES} -c $*.c

depend local_depend: ${SRC} parse.c check_defs
	@. ./${DEFS_SH}; ${XEXEC} ${SHELL} ${MKDEPEND} $$CPPFLAGS ${INCLUDES} \
		Makefile ${CSRC} ${MORE_CSRC} parse.c
	. ./${DEFS_SH}; echo "$$DEFS_DEPEND" >> Makefile
	chmod -w Makefile

check_defs:
	SHELL=${SHELL} ROOT=${ROOT} ${SHELL} ${CHECKDEFS}

install: all ${DEFS_SH}
	@. ./${DEFS_SH}; \
	   case "$$DONT_INSTALL" in \
	   ?*)	echo Testing ... install ignored; exit 0;; \
	   esac; \
	   ${XEXEC} ${SHELL} ${MKDIRS} -m 0755 $$UTIL_BIN_DIR; \
	   s_flag=; \
	   case "$$STRIP" in \
	   ?*)	s_flag=-s;;\
	   esac; \
	   ${XEXEC} ${SHELL} ${INSTM} $$s_flag -m 0555 $$UTIL_BIN_DIR ${TARGETS}

parse.o: parse.c

parse.c: parse.y $(HDRS)
	$(YACC) $(YFLAGS) parse.y
	sed 's/y\.tab\.c/parse.c/' < y.tab.c > $@
	rm -f y.tab.c

makedb: makedb.o ${DEFS_SH} ${COMPAT_LIB}
	@. ./${DEFS_SH}; ${XEXEC} $$CC ${CFLAGS} $$LDFLAGS makedb.o ${GETOPT} \
		-o makedb $$LIBS $$DBM_LIB ${COMPAT_LIB}

makedb.o: config.h

arpatxt: arpatxt.o ${DEFS_SH} ${COMPAT_LIB}
	@. ./${DEFS_SH}; ${XEXEC} $$CC ${CFLAGS} $$LDFLAGS arpatxt.o ${GETOPT} \
		-o arpatxt $$LIBS ${COMPAT_LIB}

clean:
	rm -f ${DEFS_SH} ${DEFS_H} ${DEFS_SED}
	rm -f *.o y.tab.? parse.c

clobber: clean
	rm -f pathalias makedb arpatxt

tags: $(CSRC) $(HSRC)
	ctags -w $(CSRC) $(HSRC)

TAGS: $(CSRC) $(HSRC)
	etags -w $(CSRC) $(HSRC)

bundle:
	@bundle ${SRC}

lint:	$(LSRC)
	${LINT} ${LINTFLAGS} $(CFLAGS) $(LSRC)
	${LINT} ${LINTFLAGS} makedb.c
	${LINT} ${LINTFLAGS} arpatxt.c

# DO NOT REMOVE THIS LINE, OR "make depend" WILL NOT WORK
