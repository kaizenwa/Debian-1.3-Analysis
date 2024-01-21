
.SUFFIXES:
.SUFFIXES: .o .c .lsp .lisp .fn


PORTDIR	= ../unixport
CAT=cat

OBJS	= cmpbind.o cmpblock.o cmpcall.o cmpcatch.o cmpenv.o cmpeval.o \
	cmpflet.o cmpfun.o cmpif.o cmpinline.o cmplabel.o cmplam.o cmplet.o \
	cmploc.o cmpmap.o cmpmulti.o cmpspecial.o cmptag.o cmptop.o \
	cmptype.o cmputil.o cmpvar.o cmpvs.o cmpwt.o

COMPILE_FILE=$(PORTDIR)/saved_gcl $(PORTDIR) -system-p -c-file -data-file \
	-o-file nil -h-file -compile
CFLAGS	= -c -O -I../h 

.lsp.c:
	@ ../xbin/if-exists $(PORTDIR)/saved_gcl \
	"rm -f $*.c $*.h $*.data $*.o" \
	"$(COMPILE_FILE) $* "

.lsp.o:
	@ ../xbin/if-exists $(PORTDIR)/saved_gcl \
	"rm -f $*.c $*.h $*.data $*.o" \
	"$(COMPILE_FILE) $* " \
	"$(CC) $(OFLAG) $(CFLAGS) $*.c" \
	"../xbin/append ${NULLFILE} $*.data $*.o"


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

all:	$(OBJS)

.lsp.fn: ../cmpnew/collectfn.o
	../xbin/make-fn $*.lsp


.c.o:  
	$(CC) $(OFLAG) $(CFLAGS) $*.c
	$(CAT) ${NULLFILE} $*.data >> $*.o

collectfn.o:
	$(PORTDIR)/saved_gcl $(PORTDIR)/ -compile $*.lsp

.lisp.o:
	@ ../xbin/if-exists $(PORTDIR)/saved_gcl \
	"$(PORTDIR)/saved_gcl $(PORTDIR)/ -compile $*.lisp " 
sys-proclaim.lisp:
	echo '(in-package "COMPILER")' \
	'(load "../cmpnew/collectfn")(load "../lsp/sys-proclaim.lisp")'\
	'(compiler::make-all-proclaims "*.fn")' | ../xbin/gcl


newfn:	
	make `echo $(OBJS) | sed -e 's:\.o:.fn:g'`



remake:
	for v in `"ls" *.lsp.V | sed -e "s:\.lsp\.V::g"` ; \
	do rm -f $$v.c $$v.h $$v.data $$v.lsp $$v.o ; \
	ln -s  $(MAINDIR)/cmpnew/$$v.c . ; ln -s $(MAINDIR)/cmpnew/$$v.h . ; \
	ln -s $(MAINDIR)/cmpnew/$$v.data . ; \
	done
	rm -f ../unixport/saved_gcl 
	(cd .. ; make sources)
	(cd .. ; make)
	(cd .. ; make)

clean:
	rm -f *.o core a.out
allclean:
	rm -f *.h *.data *.c
