
LISP=../unixport/saved_kcl

LOAD='(load "sysdef.lsp")(make::make :bcomp :compile t)'

all:
	echo ${LOAD} | ${LISP}


tests:
	echo ${LOAD}'(load "try1")(load "../tests/all-tests.lsp")(in-package "BCOMP")(do-some-tests)' \
	'(test-sloop)' | ${LISP}


test1:
	echo '(load "../tests/try-comp")' | ${LISP}


TFILES=src/makefile comp/makefile unixport/makefile o/makefile \
	h/enum.h src/kclobjs src/sobjs src/NewInit src/make-init

tar:
	(cd .. ; tar cvf - ${TFILES} src/sobjs src/*.c src/*.el newh/*.el src/makefile comp/*.lsp comp/*.doc newh/*.h newh/makefile  | gzip -c > ${HOME}/`date '+acl-%y%m%d'`.tar.z)

all-tests:
	make
	- make tests test1
	- (cd /u11/wfs/nqthm1 ; rm *.o ; make "LISP=nacl")
	- (cd /u11/wfs/gabriel ; make "LISP=nacl")

tests2:
	make
	- make tests test1
	- (cd /u11/wfs/nqthm1 ; rm *.o ; make "LISP=nacl")


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
