AR = ar qc
MPDIR=.
RANLIB=ranlib
# if you are using gcc for the main link you probably dont need this:
GNULIB1= ${MPDIR}/gnulib1.o
NATIVE_CC=cc

# default mp files (overridden by machine.defs)
MPFILES= $(MPDIR)/mpi.o $(MPDIR)/mp2.o  $(MPDIR)/libmport.a

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


OBJS= mp_divul3.o  mp_bfffo.o mp_mulul3.o mp2.o mp_dblrsl3.o mp_dblrul3.o ${GNULIB1}

all:
	make all1 "MPFILES=$(MPFILES)"

all1: ${MPFILES}

$(MPDIR)/libmport.a: $(OBJS)
	rm -f libmport.a
	$(AR) libmport.a ${OBJS}
	${RANLIB} libmport.a

.s.o:
	$(AS) $*.s -o $*.o

.c.o:
	$(CC)  -c $(OFLAG) -I../h -I. $(CFLAGS) $(ODIR_DEBUG) $*.c

mpi-386d.o: mpi-386d.S
	gcc -traditional -c $*.S -o $*.o

mpi-bsd68k.s: mpi.c
	gcc -S  -I../h -O mpi.c -o mpi-bsd68k.s

mpi-sparc.s: mpi.c
	gcc -S -I../h -O mpi.c -o mpi-sparc.s

mpi-sol-sparc.s: mpi.c
	gcc -S -I../h -O mpi.c -o mpi-sol-sparc.s

${MPDIR}/mpi-386.o: ${MPDIR}/mpi-386.s
	$(AS) $*.s -o $*.o

${MPDIR}/mpi-386.s: mpi.c
	gcc -S -I../h -O mpi.c -o mpi-386.s

${MPDIR}/mpi-gcc.o: mpi.c
	gcc -c -O -I../h mpi.c -o mpi-gcc.o

${MPDIR}/gnulib1.o:
	${NATIVE_CC} -c -O gnulib1.c

clean:
	rm -f *.o *.a

make.defs:
	(cd .. ; for v in h/*.defs; do echo $$v: ; fgrep MPFILES $$v ; done ; true) > make.defs


tar:
	(cd .. ; ls mp/*.c mp/*.s h/*.h h/*.defs mp/makefile mp/make.defs)  | sed -e '/foo/d' > tmpx
	(cd .. ; tar cvf - `cat mp/tmpx`) | compress -c > ${HOME}/tmp/mp.tar.Z
	rm -f tmpx





