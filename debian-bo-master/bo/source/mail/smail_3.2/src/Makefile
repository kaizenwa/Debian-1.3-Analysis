#! /bin/make -f
#
#ident	"@(#)smail/src:RELEASE-3_2:Makefile,v 1.28 1996/06/21 19:23:01 woods Exp"
#
# Makefile for the smail program
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
MKDIRS=${ROOT}/conf/lib/mkdirs.sh
CHECKDEFS=${ROOT}/conf/lib/checkdefs.sh
INST=${ROOT}/conf/lib/inst.sh
INSTM=${ROOT}/conf/lib/instm.sh
XEXEC=${SHELL} ${ROOT}/conf/lib/xexec.sh
DEFS_SH=defs.sh
DEFS_H=defs.h
DEFS_SED=defs.sed
VERSION_SH=version.sh
VERSION_H=version.h
VERSION_SED=version.sed

ROOT=..
INCLUDES=$$DBM_INCLUDES $$INCLUDES
CSRC=addr.c alloc.c ascii.c bindlib.c config.c copyright.c default.c direct.c \
     expand.c field.c hash.c header.c log.c lookup.c main.c modes.c notify.c \
     parse.c pathto.c pwcache.c qualify.c queue.c resolve.c retry.c route.c \
     smailconf.c smtprecv.c spool.c string.c sysdep.c transport.c verify.c \
     version.c
LASTSRC=silly.c
HSRC=addr.h alloc.h bindlib.h bindsmtpth.h child.h config.h debug.h direct.h \
     dys.h error.h exitcodes.h extern.h field.h hash.h jump.h lookup.h log.h \
     main.h parse.h route.h smail.h smailconf.h spool.h transport.h
MISCSRC=Makefile mkdriv.awk COPYING
SHSRC=mkdrivtab.sh bump_cnt.sh
STANDSRC=standalone.c
SRC=${CSRC} ${HSRC} ${SHSRC} ${MISCSRC} ${LASTSRC} ${STANDSRC}
FIRSTOBJ=copyright.o
OBJ=addr.o alloc.o ascii.o bindlib.o config.o default.o direct.o drivertab.o \
    expand.o field.o hash.o header.o log.o lookup.o main.o modes.o notify.o \
    parse.o pathto.o pwcache.o qualify.o queue.o resolve.o retry.o route.o \
    smailconf.o smtprecv.o spool.o string.o sysdep.o transport.o verify.o \
    version.o
LASTOBJ=silly.o
STANDOBJ=stand_hash.o stand_silly.o
STAND=testfield stand_field.c stand_field.o \
      testaddr stand_addr.c stand_addr.o
DRIVLIBS=directors/ddrivlib.a routers/rdrivlib.a transports/tdrivlib.a
DIRECTORS_HSRC=aliasfile.h altuser.h fwdfile.h include.h smartuser.h user.h
DIRECTORS_CSRC=aliasfile.c altuser.c fwdfile.c include.c smartuser.c user.c
DIRECTORS_SRC=${DIRECTORS_HSRC} ${DIRECTORS_CSRC}
ROUTERS_HSRC=bind.h rtlib.h gethost.h pathalias.h smarthost.h uuname.h \
	     queryprog.h reroute.h
ROUTERS_CSRC=bind.c rtlib.c gethost.c pathalias.c smarthost.c uuname.c \
	     queryprog.c reroute.c
ROUTERS_SRC=${ROUTERS_HSRC} ${ROUTERS_CSRC}
TRANSPORTS_HSRC=appendfile.h pipe.h tcpsmtp.h smtplib.h
TRANSPORTS_CSRC=appendfile.c pipe.c tcpsmtp.c smtplib.c
TRANSPORTS_SRC=${TRANSPORTS_HSRC} ${TRANSPORTS_CSRC}
COMPAT_LIB_DIR=${ROOT}/compat
COMPAT_LIB=${COMPAT_LIB_DIR}/compat.a

# Smail will be copied to BIN_DIR with links for all of these names
BIN_NAMES=smail mailq pathto uupath runq rsmtp smtpd

.c.o:
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS ${INCLUDES} -c $*.c

all:	ddrivlib rdrivlib tdrivlib smail

smail:	${DEFS_SH} ${FIRSTOBJ} ${OBJ} ${DRIVLIBS} ${LASTOBJ} \
		${COMPAT_LIB} bump_cnt.sh
	@. ./${DEFS_SH}; ${XEXEC} ${SHELL} bump_cnt.sh
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS ${INCLUDES} -c ldinfo.c
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS $$LDFLAGS -o smail \
		${FIRSTOBJ} ${OBJ} ${DRIVLIBS} ${LASTOBJ} \
		${COMPAT_LIB} $$DBM_LIB $$LIBS ldinfo.o

${FIRSTOBJ} ${OBJ} ${LASTOBJ}: ${DEFS_SH} ${DEFS_H}

${COMPAT_LIB}:
	cd ${COMPAT_LIB_DIR}; ${MAKE}

lint:	drivertab.c ${CSRC} ${DEFS_SH} ${VERSION_H}
	@. ./${DEFS_SH}; \
	   files="drivertab.c ${CSRC}"; \
	   echo "Get C files under ${SRC_PREFIX}directors ..."; \
	   for i in `(cd directors; ${MAKE} csrc)`; do \
		files="$$files directors/$$i"; \
	   done; \
	   echo "Get C files under ${SRC_PREFIX}routers ..."; \
	   for i in `(cd routers; ${MAKE} csrc)`; do \
		files="$$files routers/$$i"; \
	   done; \
	   echo "Get C files under ${SRC_PREFIX}transports ..."; \
	   for i in `(cd transports; ${MAKE} csrc)`; do \
		files="$$files transports/$$i"; \
	   done; \
	   ${XEXEC} ${LINT} ${LINTFLAGS} $$CPPFLAGS ${INCLUDES} $$files ${LASTSRC}

tags:	drivertab.c ${SRC} ${DEFS_SH}
	@. ./${DEFS_SH}; \
	   files="drivertab.c ${SRC}"; \
	   for i in $(DIRECTORS_SRC) ; do \
		files="$$files directors/$$i"; \
	   done; \
	   for i in $(ROUTERS_SRC) ; do \
		files="$$files routers/$$i"; \
	   done; \
	   for i in $(TRANSPORTS_SRC) ; do \
		files="$$files transports/$$i"; \
	   done; \
	   ${XEXEC} ctags $(CTAGFLAGS) $$files

TAGS:	drivertab.c ${SRC} ${DEFS_SH}
	@. ./${DEFS_SH}; \
	   files="drivertab.c ${SRC}"; \
	   for i in $(DIRECTORS_SRC) ; do \
		files="$$files directors/$$i"; \
	   done; \
	   for i in $(ROUTERS_SRC) ; do \
		files="$$files routers/$$i"; \
	   done; \
	   for i in $(TRANSPORTS_SRC) ; do \
		files="$$files transports/$$i"; \
	   done; \
	   ${XEXEC} etags $(ETAGFLAGS) $$files

names:
	@for i in ${SRC}; do echo ${SRC_PREFIX}$$i; done
	@for i in ${DIRECTORS_SRC}; do echo ${SRC_PREFIX}directors/$$i; done
	@for i in ${ROUTERS_SRC}; do echo ${SRC_PREFIX}routers/$$i; done
	@for i in ${TRANSPORTS_SRC}; do echo ${SRC_PREFIX}transports/$$i; done

ddrivlib: ${HSRC} ${DEFS_H}
directors/ddrivlib.a ddrivlib: directors/Makefile
	@echo "Build default targets under ${SRC_PREFIX}directors ..."
	@cd directors; ${MAKE}

rdrivlib: ${HSRC} ${DEFS_H}
routers/rdrivlib.a rdrivlib: routers/Makefile
	@echo "Build default targets under ${SRC_PREFIX}routers ..."
	@cd routers; ${MAKE}

tdrivlib: ${HSRC} ${DEFS_H}
transports/tdrivlib.a tdrivlib: transports/Makefile
	@echo "Build default targets under ${SRC_PREFIX}transports ..."
	@cd transports; ${MAKE}

drivertab.c directors/Makefile transports/Makefile routers/Makefile: \
		mkdrivtab.sh mkdriv.awk ${DEFS_SH}
	@. ./${DEFS_SH}; \
	   ${XEXEC} ${SHELL} mkdrivtab.sh $$DRIVER_CONFIGURATION
	@for i in directors routers transports; do \
		echo "Building dependencies under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ depend); \
	 done

iobpeek.h: ${DEFS_SH}
	@rm -f iobpeek.h
	@. ./${DEFS_SH}; ${XEXEC} ${SHELL} geniobpeek.sh $$CC "$$CFLAGS"

depend: ${SRC} check_defs subdir_depend local_depend

subdir_depend: remove_driver_makefiles \
		directors/Makefile routers/Makefile transports/Makefile

local_depend: ${VERSION_H}
	@. ./${DEFS_SH}; ${XEXEC} ${SHELL} ${MKDEPEND} $$CPPFLAGS ${INCLUDES} \
		Makefile ${CSRC}
	. ./${DEFS_SH}; echo "$$DEFS_DEPEND" >> Makefile; \
	  echo "drivertab.c: $$DRIVER_CONFIGURATION" >> Makefile; \
	  echo "directors/Makefile: $$DRIVER_CONFIGURATION" >> Makefile; \
	  echo "routers/Makefile: $$DRIVER_CONFIGURATION" >> Makefile; \
	  echo "transports/Makefile: $$DRIVER_CONFIGURATION" >> Makefile; \
	  echo chmod -w Makefile; \
	  chmod -w Makefile

${VERSION_H} ${VERSION_SH} ${VERSION_SED}:
	ROOT=${ROOT} ${SHELL} ${MKVERSION}

check_defs:
	SHELL=${SHELL} ROOT=${ROOT} ${SHELL} ${CHECKDEFS}

mkdefs: ${DEFS_H} ${DEFS_SH} ${DEFS_SED}
	@. ./${DEFS_SH}; \
	   ${XEXEC} ${SHELL} mkdrivtab.sh $$DRIVER_CONFIGURATION
	@for i in directors routers transports; do \
		echo "Running $@ under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ $@); \
	 done

${DEFS_H} ${DEFS_SH} ${DEFS_SED}: ${ROOT}/conf/EDITME
	ROOT=${ROOT} ${SHELL} ${MKDEFS}

${ROOT}/conf/EDITME:
	cd ${ROOT}/conf; ${MAKE} EDITME

remove_driver_makefiles:
	rm -f directors/Makefile routers/Makefile transports/Makefile

clean:
	rm -f ${DEFS_SH} ${DEFS_H} ${DEFS_SED}
	rm -f ${VERSION_SH} ${VERSION_H} ${VERSION_SED}
	rm -f ${FIRSTOBJ} ${OBJ} ${LASTOBJ} ${STANDOBJ} core ldinfo.o
	-@for i in directors routers transports; do \
	    if [ -f $$i/Makefile ]; then \
		echo "Clean under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ clean); \
	    else \
		echo "Ignoring ${SRC_PREFIX}$$i -- no Makefile"; \
	    fi; \
	  done

clobber:
	rm -f ${DEFS_SH} ${DEFS_H} ${DEFS_SED}
	rm -f ${VERSION_SH} ${VERSION_H} ${VERSION_SED}
	rm -f ${FIRSTOBJ} ${OBJ} ${LASTOBJ} ${STANDOBJ} core ldinfo.o
	rm -f smail ${STAND}
	rm -f drivertab.c iobpeek.h ldinfo.c
	rm -f .${DEFS_SH} .${DEFS_H} .${DEFS_SED} .Makefile .drivertab.c
	-@for i in directors routers transports; do \
	    if [ -f $$i/Makefile ]; then \
		echo "Clobber under ${SRC_PREFIX}$$i ..."; \
		(cd $$i; ${MAKE} SRC_PREFIX=${SRC_PREFIX}$$i/ clobber); \
	    else \
		echo "Ignoring ${SRC_PREFIX}$$i -- no Makefile"; \
	    fi; \
	  done

install: ${DEFS_SH} all install-dirs install-bins install-libs

install-bins:
	@. ./${DEFS_SH} ;\
	   case "$$DONT_INSTALL" in \
	   ?*)	echo Testing ... install-bins ignored; exit 0;; \
	   esac; \
	   l_flag=; \
	   case "$$USE_SYMLINKS" in \
	   ?*)	l_flag=-l;; \
	   esac; \
	   s_flag=; \
	   case "$$STRIP" in \
	   ?*)	s_flag=-s;; \
	   esac; \
	   ${XEXEC} ${SHELL} ${INST} $$s_flag $$l_flag -u root -m 4555 \
			    -d $$SMAIL_BIN_DIR \
			    smail ${BIN_NAMES} -k $$NEWALIASES \
			    $$OTHER_SMAIL_NAMES

install-libs:
	@. ./${DEFS_SH}; \
	   case "$$DONT_INSTALL" in \
	   ?*)	echo Testing ... install-libs ignored; exit 0;; \
	   esac; \
	   ${XEXEC} ${SHELL} ${INSTM} -r -m 0444 $$LIB_DIR COPYING

install-dirs:
	@. ./${DEFS_SH}; \
	   case "$$DONT_INSTALL" in \
	   ?*)	echo Testing ... install-dirs ignored; exit 0;; \
	   esac; \
	   ${XEXEC} ${SHELL} ${MKDIRS} -m 0755 $$LIB_DIR $$LIB_DIR/methods \
				      $$SMAIL_BIN_DIR

# build standalone versions to test various subsystems
#
# NOTE:  these are not checked on a regular basis and are thus not
#	 guarranteed to be compilable.  Some of these rules also leave
#	 object files which are not suitable for loading into the final
#	 smail binary and thus must be removed.
#
# stand_silly.o is loaded as the last module to overcome some
# gdb problems on one of our testing machines

TESTFIELD_OBJ=stand_field.o string.o alloc.o qualify.o addr.o \
	      ascii.o smailconf.o config.o parse.o version.o standalone.o
testfield: $(TESTFIELD_OBJ)
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS $$LDFLAGS ${INCLUDES} \
		-DSTANDALONE -o testfield $(TESTFIELD_OBJ)
stand_field.o: field.c field.h defs.h
	cp field.c stand_field.c
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS ${INCLUDES} -DSTANDALONE \
		-c stand_field.c

TESTADDR_OBJ=stand_addr.o string.o alloc.o ascii.o config.o standalone.o
testaddr: $(TESTADDR_OBJ)
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$LDFLAGS $$CFLAGS ${INCLUDES} \
		-DSTANDALONE -o testaddr $(TESTADDR_OBJ)
stand_addr.o: addr.c addr.h defs.h
	cp addr.c stand_addr.c
	@. ./${DEFS_SH}; ${XEXEC} $$CC $$CFLAGS ${INCLUDES} -DSTANDALONE \
		-c stand_addr.c

# special dependency:
version.o: version.h
smtprecv.o: iobpeek.h

# DO NOT REMOVE THIS LINE, OR "make depend" WILL NOT WORK
