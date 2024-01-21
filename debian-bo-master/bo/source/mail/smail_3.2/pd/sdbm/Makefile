#! /bin/make -f
#
#ident	"@(#)smail/pd/sdbm:RELEASE-3_2:Makefile,v 1.13 1996/02/26 15:38:32 woods Exp"
#
# Makefile for the sdbm library as distributed with smail.
#
# See the file COPYING, distributed with smail, for restriction
# and warranty information.

SHELL=/bin/sh
MAKE=make
AR=ar
LINT=lint
SRC_PREFIX=

MKDEPEND=${ROOT}/conf/lib/mkdepend.sh
MKDEFS=${ROOT}/conf/lib/mkdefs.sh
CHECKDEFS=${ROOT}/conf/lib/checkdefs.sh
XEXEC=${SHELL} ${ROOT}/conf/lib/xexec.sh
DEFS_SH=defs.sh
DEFS_H=defs.h
DEFS_SED=defs.sed

ROOT=../..
MISCSRC=Makefile

OBJ = sdbm.o pair.o hash.o
TEST = dba dbd dbu
TEST_OBJ = dba.o dbd.o dbu.o util.o
CSRC = sdbm.c pair.c hash.c dba.c dbd.c dbe.c dbm.c dbu.c util.c
HSRC = dbm.h pair.h sdbm.h tune.h
MISCSRC = Makefile grind
SRC = $(MISCSRC) $(CSRC) $(HSRC)
CHDR = tune.h sdbm.h pair.h

.c.o:
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS $$INCLUDES -c $*.c

all:	sdbm.a $(TEST)

sdbm.a: ${DEFS_SH} ${OBJ}
	@. ./${DEFS_SH}; \
	   ${XEXEC} ${AR} rc sdbm.a ${OBJ}; \
	   ${XEXEC} $$RANLIB sdbm.a

dba: dba.o util.o sdbm.a
	@. ./${DEFS_SH}; \
	   ${XEXEC} $$CC $$CFLAGS $$LDFLAGS -o $@ $@.o util.o sdbm.a $$LIBS
dbd: dbd.o util.o sdbm.a
	@. ./${DEFS_SH}; \
	   ${XEXEC} $$CC $$CFLAGS $$LDFLAGS -o $@ $@.o util.o sdbm.a $$LIBS
dbu: dbu.o util.o sdbm.a
	@. ./${DEFS_SH}; \
	   ${XEXEC} $$CC $$CFLAGS $$LDFLAGS -o $@ $@.o util.o sdbm.a $$LIBS

mkdefs ${DEFS_SH}: ${ROOT}/conf/EDITME
	ROOT=${ROOT} ${SHELL} ${MKDEFS}

${ROOT}/conf/EDITME: # cannot depend on anything!
	cd ${ROOT}/conf; ${MAKE} EDITME

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done

depend local_depend: check_defs
	@. ./${DEFS_SH}; ${XEXEC} ${SHELL} ${MKDEPEND} $$CPPFLAGS $$INCLUDES \
		Makefile ${CSRC}
	@. ./${DEFS_SH}; \
	   echo "echo \"defs.sh: \$${ROOT}/conf/os/$$OS_TYPE\" >> Makefile"; \
	   echo "defs.sh: \$${ROOT}/conf/os/$$OS_TYPE" >> Makefile; \
	chmod -w Makefile

check_defs:
	SHELL=${SHELL} ROOT=${ROOT} ${SHELL} ${CHECKDEFS}

install:

clean:
	rm -f ${DEFS_SH} ${DEFS_H} ${DEFS_SED}
	rm -f .${DEFS_SH} .${DEFS_H} .${DEFS_SED} .Makefile
	rm -f ${OBJ} ${TEST_OBJ}
	rm -f a.out core
	rm -f sdbm.a

clobber: clean
	rm -f ${TEST}

# DO NOT REMOVE THIS LINE, OR "make depend" WILL NOT WORK
