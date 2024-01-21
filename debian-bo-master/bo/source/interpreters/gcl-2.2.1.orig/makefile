#  Compiling gcl
# select a machine-type from ./MACHINES

# ./add-defs machine-type
# make

# For more details see README

PREFIX_DIR=/usr/local
# This would cause make install to create /usr/local/bin/gcl and
# /usr/local/lib/gcl-1-??/* with some basic files.
##########################

# begin makedefs
GCLDIR=/home/wfs/gcl-2.2
SHELL=/bin/sh
MACHINE=386-linux
TK_CONFIG_PREFIX="/usr/local/lib"
TCL_CONFIG_PREFIX="/usr/local/lib"
TK_INCLUDE="-I/usr/local/include"
TK_VERSION=4.2
TCL_VERSION=7.6
TK_LIB_SPEC=-L/usr/local/lib -ltk4.2
TK_LIBRARY=/usr/local/lib/tk4.2
TCL_LIBRARY=/usr/local/lib/tcl7.6
TK_BUILD_LIB_SPEC=-L/usr/local.linux14/src/tk4.2/unix -ltk4.2
TK_XLIBSW=-L/usr/X11R6/lib -lX11
TK_XINCLUDES=-I/usr/X11R6/include
TCL_LIB_SPEC=-L/usr/local/lib -ltcl7.6
TCL_DL_LIBS=-ldl
TCL_LIBS=-ldl  -lieee -lm
HAVE_X11=-DHAVE_X11

# Machine dependent makefile definitions for intel 386,486 running linux

LBINDIR=/usr/local/bin

OFLAG	=  -O 
LIBS	= -lm 

ODIR_DEBUG= -O4 

# This CC string will be used for compilation of the system,
# and also in the compiler::*cc* variable for later compilation of
# lisp files.
# (the -pipe is just since our file system is slow..)
CC = gcc -pipe -fwritable-strings  -DVOL=volatile -I$(GCLDIR)/o -fsigned-char
LDCC=${CC} -static
# note for linuxaout on an elf machine add -b i486-linuxaout 
# CC = gcc -pipe -fwritable-strings  -DVOL=volatile -I$(GCLDIR)/o -fsigned-char -b i486-linuxaout 

# Enable the fastloading mechanism which does not use ld -A
# requires c/rel_.. machine dependent code.

RSYM	= rsym
SFASL	= $(ODIR)/sfasl.o


MPFILES= $(MPDIR)/mpi-386d.o   $(MPDIR)/libmport.a


# When using SFASL it is good to have (si::build-symbol-table)
INITFORM=(si::build-symbol-table)

# Use symbolic links
SYMB=-s

LIBFILES=bsearch.o

# the  make to use for saved_kcp the profiler.
KCP=kcp-bsd
# end makedefs









BINDIR	= bin
HDIR	= h/
CDIR	= c
ODIR	= o
LSPDIR	= lsp
CMPDIR	= cmpnew
PORTDIR	= unixport

all:   ${HDIR}cmpinclude.h
	(cd $(BINDIR); make all)
	(cd mp ; make all)
	rm -f o/cmpinclude.h ; ln h/cmpinclude.h o
	(cd $(ODIR); make all)
	(cd $(LSPDIR); make all)
	(cd $(CMPDIR); make all)
	(cd $(PORTDIR); make saved_gcl)
	make command

# 	After making successfully send in a notification to
#	gcl@nicolas.ma.utexas.edu.   This is extremely helpful in
#	tracking on which machines and OS versions there are problems.
#	This will be done automatically by the  xbin/notify line.
	(cd  cmpnew ; make collectfn.o)
	(cd lsp ; make info.o)
	@- if  [ -f cmpnew/collectfn.o ]  ; \
	then echo "Make of GCL `cat majvers`.`cat minvers` completed." ; \
	./xbin/notify "CC=${CC}" ; \
	else echo "The new image failed to compile collectfn.lsp" ;fi ; \
	if [ "${TK_CONFIG_PREFIX}" != "unknown" ] ; \
	then (cd gcl-tk ; make) ; \
	else echo "gcl-tk not made..missing include or lib" ; fi 
	(cd info ; make)

command:
	rm -f bin/gcl xbin/gcl
	make install-command "INSTALL_LIB_DIR=${GCLDIR}" "PREFIX_DIR=${GCLDIR}"
	(cd xbin ; ln -s ../bin/gcl .)

merge:
	${CC} -o merge merge.c

LISP_LIB=cmpnew/collectfn.o lsp/gprof.lsp lsp/info.o lsp/profile.lsp gcl-tk/tkl.o  gcl-tk/tinfo.o gcl-tk/demos/*.lsp gcl-tk/demos/*.lisp gcl-tk/demos/*.o

install-command:
	(echo "#!/bin/sh" ; \
	echo exec ${INSTALL_LIB_DIR}/unixport/saved_gcl \\ ; \
	echo '   -dir' ${INSTALL_LIB_DIR}/unixport/ \\ ; \
	echo '   -libdir' ${INSTALL_LIB_DIR}/ \\ ; \
	echo '   -eval '\''(setq si::*allow-gzipped-file* t)'\' \\ ;\
	echo '   -eval '\''(setq si::*tk-library* '\"${TK_LIBRARY}\"')'\' \\;\
	echo '     '\"\$$@\" ) > ${PREFIX_DIR}/bin/gcl;
	echo '#' other options: -load "/tmp/foo.o" -load "jo.lsp" -eval '"(joe 3)"' >> ${PREFIX_DIR}/bin/gcl
	chmod a+x ${PREFIX_DIR}/bin/gcl

install: 
	make install1 "INSTALL_LIB_DIR=${PREFIX_DIR}/lib/gcl-`cat majvers`.`cat minvers`" "PREFIX_DIR=${PREFIX_DIR}"
INSTALL_LIB_DIR=
install1:
	-if [ -d "${PREFIX_DIR}/lib" ] ;then true; else mkdir ${PREFIX_DIR}/lib ; fi
	-if [ -d "${PREFIX_DIR}/bin" ] ;then true; else mkdir ${PREFIX_DIR}/bin ; fi
	-if [ -d "${INSTALL_LIB_DIR}" ] ; then true; else mkdir o${INSTALL_LIB_DIR} ;fi
	make install-command "INSTALL_LIB_DIR=${INSTALL_LIB_DIR}" "PREFIX_DIR=${PREFIX_DIR}"
	rm -f ${PREFIX_DIR}/bin/gcl.exe
	tar cvf - unixport/saved_gcl info/*info* doc/*.el ${LISP_LIB} \
    	gcl-tk/gcl.tcl gcl-tk/gcltkaux  |  (cd ${INSTALL_LIB_DIR} ;tar xvf -)
	ln ${SYMB} ${INSTALL_LIB_DIR}/unixport/saved_gcl \
	 ${PREFIX_DIR}/bin/gcl.exe
	-cat gcl-tk/gcltksrv | \
	sed -e "s:DIR=.*:DIR=${INSTALL_LIB_DIR}/gcl-tk:g"  \
	-e "s:TK_LIBRARY=.*:TK_LIBRARY=${TK_LIBRARY}:g" > \
	${INSTALL_LIB_DIR}/gcl-tk/gcltksrv
	-if [ "${TK_CONFIG_PREFIX}" != "unknown" ] ; then  \
	chmod a+x ${INSTALL_LIB_DIR}/gcl-tk/gcltksrv ; fi
	-if [ "${TK_CONFIG_PREFIX}" != "unknown" ] ; then  \
	(cd ${INSTALL_LIB_DIR}/gcl-tk/demos ; \
	echo '(load "../tkl.o")(TK::GET-AUTOLOADS (directory "*.lisp"))' | ${PREFIX_DIR}/bin/gcl) ; fi
	(cd info ; make ; make install)

clean:
	(cd $(BINDIR); make clean)
	(cd mp ; make clean)
	(cd $(ODIR); make clean)
	(cd $(LSPDIR); make clean)
	(cd $(CMPDIR); make clean)
	(cd $(PORTDIR); make clean)
	(cd gcl-tk ; make clean)
	


CMPINCLUDE_FILES=${HDIR}cmpincl1.h ${HDIR}compbas.h ${HDIR}enum.h ${HDIR}object.h ${HDIR}vs.h \
	${HDIR}bds.h ${HDIR}frame.h \
	${HDIR}lex.h ${HDIR}eval.h    ${HDIR}funlink.h \
	${HDIR}att_ext.h ${HDIR}new_decl.h ${HDIR}compbas2.h ${HDIR}/compat.h ${HDIR}/cmponly.h

OTHERS=${HDIR}notcomp.h ${HDIR}rgbc.h ${HDIR}stacks.h 

${HDIR}cmpinclude.h: ${CMPINCLUDE_FILES} ${HDIR}config.h
	cat ${HDIR}config.h | sed -e "1,/Begin for cmpincl/d" \
		-e "/End for cmpinclude/,50000d" > tmpx
	cat ${CMPINCLUDE_FILES} >> tmpx
	./xbin/move-if-changed mv tmpx h/cmpinclude.h
	./xbin/move-if-changed ln h/cmpinclude.h o/cmpinclude.h

go:
	mkdir go
	(cd go ; ln -s ../o/makefile ../o/*.o ../o/*.c ../o/*.d ../o/*.ini  .)
	(cd go ; make  go)

tar:
	rm -f gcl-`cat majvers`.`cat minvers`
	xbin/distribute ../ngcl-`cat majvers`.`cat minvers`-beta.tgz

kcp:
	(cd go ; make  "CFLAGS = -I../h -pg  -c -g ")
	(cd unixport ; make gcp)
