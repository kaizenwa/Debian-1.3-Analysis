#=======================================================================
# Makefile for bibindex and biblook.
#
# These programs are written in ISO/ANSI Standard C.  They must be
# compiled with a Standard C compiler, or with a C++ compiler.
#
# Current target list:
#	all 			make bibindex and biblook
#	bibindex 		make indexing program
#	bibindex.tar		UNIX tar archive file for bibindex
#				distribution
#	bibindex.tar-lst	listing of UNIX tar archive
#	bibindex.tar.z		compressed UNIX tar archive file for bibindex
#	bibindex.txt 		ascii text file from UNIX man pages
#	bibindex.uue		uuencoded bibindex.tar.z archive
#	bibindex.zip		InfoZip archive file for bibindex
#				distribution
#	bibindex.zip-lst	listing of InfoZip archive file
#	bibindex.zoo		zoo archive file for bibindex distribution
#	bibindex.zoo-lst	listing of zoo archive file
#	biblook.txt 		ascii text file from UNIX man pages
#	biblook 		make lookup program
#	clean 			remove all recreatable files, except
#				executables
#	clobber 		remove all recreatable files
#	distclean		same as clobber (GNU standard)
#	mostlyclean		same as clean (GNU standard)
#	install 		install executables and manual pages
#	install-ftp		install distribution in anonymous ftp tree
#	realclean		same as clobber (GNU standard)
#	reallyclean		same as clobber
#	uninstall 		uninstall executables and manual pages
#
# In addition, the following convenience targets are provided:
#	dec-ultrix-g++
#	dec-ultrix-gcc
#	hp-hpux-c++
#	hp-hpux-cc
#	hp-hpux-c++-memio
#	hp-hpux-cc-memio
#	ibm-3090
#	ibm-ps2
#	ibm-rs6000-c++
#	ibm-rs6000-cc
#	next-c++
#	next-cc
#	next-gcc
#	sgi-c++
#	sgi-cc
#	sgi-g++
#	sgi-gcc
#	sgi-lcc
#	sun-solaris-c++
#	sun-solaris-cc
#	sun-solaris-g++
#	sun-solaris-gcc
#	sun-sunos-acc
#	sun-sunos-g++
#	sun-sunos-gcc
#
# By default, these machine-specific targets will build everything.
# You can make a specific target like this:
#
#	make sgi-cc TARGET=memio
#
# Preprocessor symbol definitions that you might want to change:
#
# CC		see the large selection below of recommended settings.
#
# BINDIR	where the installed programs go
#
# CATDIR	where the formatted manual pages go
#
# DEF_H_FILES	On non-UNIX systems, if your system has the include file
#		netinet/in.h, define HAVE_NETINET_IN_H (this symbol is
#		automatically defined on UNIX), since all (we hope)
#		such systems have that include file.  When
#		HAVE_NETINET_IN_H is defined and non-zero, the .bix
#		file is read and written in network byte order,
#		allowing it to be shared between big-endian and
#		little-endian architectures on the same network.  If
#		you don't require this feature, then you can define
#		HAVE_NETINET_IN_H to 0 at compile time.  Since
#		heterogeneous networks are common at many sites, the
#		default is to use network byte order.
#
#		If your site has malloc.h (most UNIX systems do), add
#		-DHAVE_MALLOC_H.
#
#		If your site has stdlib.h, but not yet a Standard C compiler,
#		add -DHAVE_STDLIB_H.
#
# DEST		root directory of local binaries and man pages
#
# MANDIR	where the unformatted manual pages go
#
# MORE		use pg, more, or less for output paging (NB: see security
# 		remarks there)
#
# Numerous other preprocessor symbols are defined below, but are
# unlikely to need modifications.
#
# [06-Oct-1993]
#=======================================================================

DEST		= /usr/local

BINDIR		= $(DEST)/bin

CATDIR		= $(DEST)/man/cat1

# Compilation with a C++ compiler is preferable.  SunOS 4.1 CC cannot be
# used, however, because of its erroneous function prototypes in stdlib.h
# which use char* instead of void* in many places.  There is no such
# problem with Sun Solaris 2.1 and 2.2 CC, which works fine.
#CC		= CC			## UNIX C++ compilers (HP, SGI, Sun Solaris 2.x)
#CC		= CC -I/usr/CC/incl	## C++ on SunOS 4.1.1
#CC		= acc -Dsun		## SunOS Standard C compiler
#CC		= c89 -D_POSIX_SOURCE	## HP 9000/850 HP-UX A.08.00 D
#CC		= c89 -D_POSIX_SOURCE	## IBM RS/6000 Standard C
#CC		= xlC			## IBM RS/6000 C++
#CC		= cc			## many UNIX systems
#CC		= cc $(GCCFLAGS)	## NeXT
#CC		= lcc -A -A -n		## Princeton/AT&T Standard C compiler
#CC              = gcc $(GCCFLAGS)	## GNU C
CC		= g++ $(GCCFLAGS)	## UNIX systems with GNU C++
#CC		= g++ $(GCCFLAGS) -Dtemplate=Template ## NeXT systems with GNU C++
#CC		= cc -ObjC		## NeXT systems with Objective C

#CC		= CC			## UNIX C++ compilers (HP, SGI, Sun Solaris 2.x)

CFLAGS		= $(DEFINES) $(OPT)	## most cases

GCCFLAGS	= -Wall -Wshadow -Wcast-qual -Wpointer-arith \
		  -Wwrite-strings

COL		= col -b

COMPRESS	= compress

CP		= /bin/cp

DEFINES		= $(DEF_H_FILES) $(DEF_MAXRESULTS) $(DEF_MORE) $(DEF_MEMIO)

# Pick one of these; see the comments above.

DEF_H_FILES	= -DHAVE_MALLOC_H -DHAVE_STDLIB_H -DHAVE_NETINET_IN_H=0
DEF_H_FILES	= -DHAVE_MALLOC_H -DHAVE_NETINET_IN_H=0
DEF_H_FILES	= -DHAVE_MALLOC_H

# Maximum number of bibliographic entries that can be displayed
# A remote lookup service may wish to reduce this limit to prevent
# fetching of large chunks of a bibliography data base.
DEF_MAXRESULTS	= -DMAXRESULTS=500
DEF_MAXRESULTS	=		## use built-in defaults

# Select one or the other of these, and set MEMIO below accordingly.
DEF_MEMIO	=
DEF_MEMIO	= -DUSE_MEMIO

# Use less if possible, and otherwise more, or some other pager.
# Set MOREPATH to the correct absolute path, and MORE to the
# path-less file name.
# NB: Security note: if you set up a service like the "telnet biblio"
# facility on siggraph.org, make sure that you take precautions to
# ensure that the shell escape facility of these pagers cannot give
# a remote user a separate login session!  You can for example use
# the restricted shell (/usr/lib/rsh), and launch biblook from the
# .profile file.
#DEF_MORE	=		## use built-in defaults
#DEF_MORE	= -DMOREPATH=\"/usr/bin/pg\" -DMORE=\"pg\"
#DEF_MORE	= -DMOREPATH=\"/usr/ucb/more\" -DMORE=\"more\"
#DEF_MORE	= -DMOREPATH=\"/usr/local/bin/less\" -DMORE=\"less\"
DEF_MORE	= -DMOREPATH=\"/usr/bin/less\" -DMORE=\"less\"

# This setting is suitable for ftp.math.utah.edu:
FTPDIR		= /usr/spool/ftp/pub/tex/bib
FTP-FILES	= bibindex$(VERSION).tar.z bibindex$(VERSION).zip \
		bibindex$(VERSION).zoo \
		bibindex$(VERSION).tar-lst bibindex$(VERSION).zip-lst \
		bibindex$(VERSION).zoo-lst

LDFLAGS		=

# Use /usr/lib/debug/malloc.o on Sun systems for malloc debugging
# with acc, gcc, or CC
LIBS		= /usr/lib/debug/malloc.o
LIBS		=

MANDIR		= $(DEST)/man/man1

MANEXT		= 1

# If you enable -DUSE_MEMIO, then you must also link with memio.o,
# so interchange the next two lines accordingly.
MEMIO		=
MEMIO		= memio.o

NROFF		= nroff -man

# Define DEBUG_MALLOC on Sun systems for debugging memory allocation
OPT		= -g -DDEBUG_MALLOC
OPT		= -g

RM		= /bin/rm -f

SHELL		= /bin/sh

# If your system lacks strip, use echo instead
STRIP		= echo
STRIP		= strip

TAR		= tar

TAR-FILES	= NEWS README PROBLEMS Makefile bibindex.c bibindex.man \
		bibindex.txt biblook.c biblook.h biblook.man biblook.txt \
		memio.c memio.h $(VMS-FILES)

UNZIP		= unzip

UUENCODE	= uuencode

VERSION		= -2-6

VMS-FILES	= $(VMS-VAX-FILES) $(VMS-ALPHA-FILES)

VMS-ALPHA-FILES = vms/alpha/bibindex.exe vms/alpha/biblook.exe \
		vms/alpha/dbgindex.com vms/alpha/dbglook.com \
		vms/alpha/vmsclean.com vms/alpha/vmsindex.com \
		vms/alpha/vmslook.com vms/alpha/vmsmake.com

VMS-VAX-FILES = vms/vax/bibindex.exe vms/vax/biblook.exe \
		vms/vax/dbgindex.com vms/vax/dbglook.com \
		vms/vax/vmsclean.com vms/vax/vmsindex.com \
		vms/vax/vmslook.com vms/vax/vmsmake.com

ZIP		= zip

ZOO		= zoo

#=======================================================================

.SUFFIXES:	.i .c

.c.i:
	$(CC) $(CFLAGS) -E $< >$@

#=======================================================================

all:	bibindex biblook bibindex.txt biblook.txt

bibindex:	bibindex.o $(MEMIO)
	$(CC) $(CFLAGS) -o bibindex bibindex.o $(MEMIO) $(LDFLAGS) $(LIBS)

bibindex$(VERSION).tar:	$(TAR-FILES)
	$(TAR) cf $@ $(TAR-FILES)

bibindex$(VERSION).tar-lst:	bibindex$(VERSION).tar
	$(TAR) tvf $? >$@

bibindex$(VERSION).tar.z:	bibindex$(VERSION).tar
	$(COMPRESS) <$? >$@

bibindex.txt:	bibindex.man
	$(NROFF) $? | $(COL) >$@

bibindex$(VERSION).uue:	bibindex$(VERSION).tar.z
	$(UUENCODE) $? $? >$@

bibindex$(VERSION).zip:	$(TAR-FILES)
	-$(RM) $@
	$(ZIP) $@ $(TAR-FILES)

bibindex$(VERSION).zip-lst:	bibindex$(VERSION).zip
	$(UNZIP) -v $? >$@

bibindex$(VERSION).zoo:	$(TAR-FILES)
	-$(RM) $@
	$(ZOO) a $@ $(TAR-FILES)

bibindex$(VERSION).zoo-lst:	bibindex$(VERSION).zoo
	$(ZOO) v $? >$@

biblook:	biblook.o $(MEMIO)
	$(CC) $(CFLAGS) -o biblook biblook.o $(MEMIO) $(LDFLAGS) $(LIBS)

biblook.txt:	biblook.man
	$(NROFF) $? | $(COL) >$@

bibindex.o biblook.o: biblook.h memio.h

clean mostlyclean:
	-$(RM) \#*
	-$(RM) *~
	-$(RM) core
	-$(RM) *.i
	-$(RM) *.o
	-$(RM) bibindex$(VERSION).tar bibindex$(VERSION).tar.z
	-$(RM) bibindex$(VERSION).tar-lst
	-$(RM) bibindex$(VERSION).uue
	-$(RM) bibindex$(VERSION).zip bibindex$(VERSION).zip-lst
	-$(RM) bibindex$(VERSION).zoo bibindex$(VERSION).zoo-lst

clobber distclean realclean reallyclean:	clean
	-$(RM) gmon.out mon.out *.d
	-$(RM) biblook bibindex memio
	-$(RM) biblook.txt bibindex.txt
	-$(RM) TAGS

install:	bibindex biblook
	-$(CP) bibindex $(BINDIR)
	-$(STRIP) $(BINDIR)/bibindex
	-chmod 775 $(BINDIR)/bibindex
	-$(CP) biblook $(BINDIR)
	-$(STRIP) $(BINDIR)/biblook
	-chmod 775 $(BINDIR)/biblook
	-$(RM) $(CATDIR)/bibindex.$(MANEXT)
	-$(CP) bibindex.man $(MANDIR)/bibindex.$(MANEXT)
	-chmod 774 $(MANDIR)/bibindex.$(MANEXT)
	-$(CP) biblook.man $(MANDIR)/biblook.$(MANEXT)
	-$(RM) $(CATDIR)/biblook.$(MANEXT)
	-chmod 774 $(MANDIR)/biblook.$(MANEXT)

install-ftp:	$(FTP-FILES)
	-for f in $? ; \
	do \
		$(CP) $$f $(FTPDIR) ; \
		chmod 664 $(FTPDIR)/$$f ; \
	done

# NB: memio.c, NOT memio.o here, because we compile with -DTEST
memio:	memio.c memio.h
	$(CC) $(CFLAGS) -DTEST -o $@ memio.c
	-$(RM) memio.o

memio.o:	memio.c memio.h

uninstall:
	-$(RM) $(BINDIR)/bibindex
	-$(RM) $(BINDIR)/biblook
	-$(RM) $(MANDIR)/bibindex.$(MANEXT)
	-$(RM) $(MANDIR)/biblook.$(MANEXT)
	-$(RM) $(CATDIR)/bibindex.$(MANEXT)
	-$(RM) $(CATDIR)/biblook.$(MANEXT)

uninstall-ftp:
	-for f in $(FTP-FILES) ; \
	do \
		$(RM) $(FTPDIR)/$$f ; \
	done

#=======================================================================
# Convenience targets:
#	make systemname TARGET=targetname
# Default is to make everything for systemname.

WITH-MEMIO	= MEMIO=memio.o DEF_MEMIO=-DUSE_MEMIO

WITHOUT-MEMIO	= MEMIO= DEF_MEMIO=

# DECstation 3100 and 5000, ULTRIX 4.2 and 4.3
# cc is not ANSI/ISO Standard C, so it cannot be used
dec-ultrix-g++:
	$(MAKE) CC='g++' $(WITHOUT-MEMIO) OPT='$(OPT)' $(TARGET)

dec-ultrix-gcc:
	$(MAKE) CC='gcc' $(WITHOUT-MEMIO) OPT='$(OPT)' $(TARGET)

# HP 9000/7xx and HP 9000/8xx HP-UX 9.0; can use WITH-MEMIO or
# WITHOUT-MEMIO, but for bibindex the latter is about 25% faster.  For
# biblook, WITH-MEMIO is faster.  See the remarks in the NEWS file.
hp-hpux-cc:
	$(MAKE) CC='c89 -D_HPUX_SOURCE' $(WITHOUT-MEMIO) OPT='$(OPT)' $(TARGET)

hp-hpux-c++:
	$(MAKE) CC='CC' $(WITHOUT-MEMIO) OPT='$(OPT)' $(TARGET)

hp-hpux-cc-memio:
	$(MAKE) CC='c89 -D_HPUX_SOURCE' $(WITH-MEMIO) OPT='$(OPT)' $(TARGET)

hp-hpux-c++-memio:
	$(MAKE) CC='CC' $(WITH-MEMIO) OPT='$(OPT)' $(TARGET)

# IBM PS/2 AIX 2.1 with Metaware High C compiler
ibm-ps2:
	$(MAKE) CC='cc -Hansi -Hnocpp' $(WITHOUT-MEMIO) OPT='$(OPT)' \
		DEF_MORE=  $(TARGET) 

# IBM 3090 AIX 2.1 with Metaware High C compiler
ibm-3090:
	$(MAKE) CC='cc -Hansi -Hnocpp' $(WITHOUT-MEMIO) OPT='$(OPT)' $(TARGET)

# IBM RS/6000, AIX 3.2
ibm-rs6000-c++:
	$(MAKE) CC='xlC'  $(WITH-MEMIO) OPT='$(OPT)' $(TARGET)

ibm-rs6000-cc:
	$(MAKE) CC='cc'  $(WITH-MEMIO) OPT='$(OPT)' $(TARGET)

# NeXT Mach 3.0; /usr/include/bsd/libc.h uses the C++ reserved word `template'
# in two function prototypes, so rename that word.
next-c++:
	$(MAKE) CC='c++ -Dtemplate=template_' $(WITHOUT-MEMIO) OPT='$(OPT)' $(TARGET)

next-cc:
	$(MAKE) CC='cc'  $(WITHOUT-MEMIO) OPT='$(OPT)' $(TARGET)

next-g++:
	$(MAKE) CC='g++ -Dtemplate=template_' $(WITHOUT-MEMIO) OPT='$(OPT)' $(TARGET)

next-gcc:
	$(MAKE) CC='gcc' $(WITHOUT-MEMIO) OPT='$(OPT)' $(TARGET)

next-objc:
	$(MAKE) CC='cc -ObjC' $(WITHOUT-MEMIO) OPT='$(OPT)' $(TARGET)

# Silicon Graphics Indigo IRIX 4.0.5
sgi-c++:
	$(MAKE) CC='CC'  $(WITH-MEMIO) OPT='$(OPT)' $(TARGET)

sgi-cc:
	$(MAKE) CC='cc'  $(WITH-MEMIO) OPT='$(OPT)' $(TARGET)

sgi-g++:
	$(MAKE) CC='g++' $(WITH-MEMIO) OPT='$(OPT)' $(TARGET)

sgi-gcc:
	$(MAKE) CC='gcc' $(WITH-MEMIO) OPT='$(OPT)' $(TARGET)

# NB: lcc 1.9 will not accept unsigned char bitfields used in
# <sys/wait.h>, so neither bibindex.c nor biblook.c will compile.
# memio.c compiles, but memio core dumps.
sgi-lcc:
	$(MAKE) CC='lcc'  $(WITH-MEMIO) OPT='$(OPT)' $(TARGET)

# Sun SPARC SunOS 4.1.3 and Solaris 2.2
sun-solaris-c++:
	$(MAKE) CC='CC'  $(WITH-MEMIO) OPT='$(OPT)' $(TARGET)

sun-solaris-cc:
	$(MAKE) CC='cc'  $(WITH-MEMIO) OPT='$(OPT)' $(TARGET)

sun-solaris-g++ sun-sunos-g++:
	$(MAKE) CC='g++' $(WITH-MEMIO) OPT='$(OPT)' $(TARGET)

sun-solaris-gcc sun-sunos-gcc:
	$(MAKE) CC='gcc' $(WITH-MEMIO) OPT='$(OPT)' $(TARGET)

sun-sunos-acc:
	$(MAKE) CC='acc' $(WITH-MEMIO) OPT='$(OPT)' $(TARGET)

#=======================================================================
