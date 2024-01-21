# Makefile for GNU Pascal compiler.
#   Copyright (C) 1987, 1996 Free Software Foundation, Inc.
#
# This file is part of GNU Pascal.
#
# GNU Pascal is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 1, or (at your option)
# any later version.
#
# GNU Pascal is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Pascal; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

# This makefile assumes you have already compiled the correct version
# of GNU CC in $(gccsrc) to the directory $(gccbin), which should contain
# necessary object files.

# Wed Aug  7 13:49:07 1996 <j.j.vanderheijden@student.utwente.nl>
# Makefile rewritten for GNU autoconf

# 11 Nov 1996  Peter Gerwinski  <peter.gerwinski@uni-essen.de>
# Patched for compilation with OS/2 and EMX

# Run 'configure' to build Makefile form Makefile.in

# End-user make options:
#    make all:        build the compiler and runtime system
#    make clean:      remove binaries and object code
#    make distclean:  romove config files too. You have to run configure again.
#    make install:    install the compiler
#    make uninstall:  undoes make install
#
# CFLAGS is used for compiler and RTS, RTSFLAGS are for RTS only, i.e.:
#    make CFLAGS="-m486 -O2" RTSFLAGS=-Wall

GPCVERSION=2.0
GCCVERSION=2.7.2.1

CROSS=
VPATH=.

srcdir = .
gccsrc = ../gcc-2.7
gccbin = .
# We must copy all object files to the current directory.
# Otherwise, the command line becomes too long to be handled by OS/2.

gccdossrc = ..\gcc-2.7
gccdosbin = ..\gcc-2.7

# Extracted from $(gccbin)/Makefile by configure
gcc_version         = 2.7.2.1
gcc_target          = emx
gcc_prefix          = /usr/local
gcc_local_prefix    = /usr/local

EXTRA_OBJS          = 
out_object_file     = emx.o

# MAKE = make # we are using dmake

CONFIG_H = config.h

CC = gcc
CFLAGS = -O2
LDFLAGS = -s
RANLIB = ranlib
AR = ar
AR_FLAGS = rc
LINT = lint
LINT_FLAGS = -DGPC
BISON = bison
BISON_FLAGS = -d
TAR = tar
TAR_FLAGS = cvzf
GPERF = gperf
GPERF_FLAGS = -g -o -j1 -t -p -N is_reserved_word
#DIFF = gdiff -u
DIFF = diff -C2 -N -r
SHELL = cmd
INSTALL = $(srcdir)/install-sh

# --------------- No user-servicable parts beyond here ----------------

# IN_GCC tells obstack.h that we are using gcc's <stddef.h> file.
# ??? IN_GCC should be obsolete now.
# Define GPC if you wish to compile GPC (obviously)
INTERNAL_CFLAGS = -DGPC -DIN_GCC -Dabort=fancy_abort $(CROSS)

# compiler flags including the above
ALL_CFLAGS = $(INTERNAL_CFLAGS) $(CFLAGS)

INCLUDES = -I. -I$(srcdir) -I$(gccbin) -I$(gccsrc) -I$(gccsrc)/config

.c.o:
	$(CC) -c $(ALL_CFLAGS) $(CPPFLAGS) $(INCLUDES) $<


gpc_version = $(GPCVERSION)
target = $(gcc_target)

# Common prefix for installation directories.
# NOTE: This directory must exist when you start installation.
prefix = $(gcc_prefix)
# Directory in which to put localized header files. On the systems with
# gcc as the native cc, `local_prefix' may not be `prefix' which is
# /usr'.
# NOTE: local_prefix *should not* default from prefix.
localprefix = $(gcc_local_prefix)
# Directory in which to put host dependent programs and libraries
exec_prefix = $(prefix)
# Directory in which to put the executable for the command `gcc'
bindir = $(exec_prefix)/bin
# Directory in which to put the directories used by the compiler.
libdir = $(exec_prefix)/lib
# Directory in which the compiler finds executables, libraries, etc.
libsubdir = $(libdir)/gcc-lib/$(target)/$(gcc_version)
# Directory in which the compiler finds g++ includes.
gxx_include_dir = $(libdir)/g++-include
# Directory to search for site-specific includes.
includedir = $(localprefix)/include
# Directory in which to find other cross-compilation tools and headers.
# Used in install-cross.
tooldir = $(exec_prefix)/$(target)

infodir = $(prefix)/info

# If you are making gcc for the first time, and if you are compiling it with
# a non-gcc compiler, and if your system doesn't have a working alloca() in any
# of the standard libraries (as is true for HP/UX or Genix),
# then get alloca.c from GNU Emacs and un-comment the following line:
#
# Alloca is automatically loaded if it exists if $(gccbin)/alloca.o exists.
#
#GCC_ALLOCA = `if [ -f "$(gccbin)/alloca.o" ]; then echo "$(gccbin)/alloca.o" ; else true ; fi`

# But if your system has alloca() in /lib/libPW.a, un-comment the following line:
# CLIB= -lPW

# If your system's malloc() routine fails for any reason (and gcc also uses a
# separate malloc package, the following should cause malloc.o to be loaded also
# in gpc)
#
#GCC_MALLOC = `if [ -f "$(gccbin)/malloc.o" ]; then echo "$(gccbin)/malloc.o" ; else true ; fi`

# Change this to a null string if obstacks are installed in the
# system library.
#
OBSTACK = $(gccbin)/obstack.o
OBSTACK_SRC= $(gccbin)/obstack.c

# Dependency on obstack, alloca, malloc or whatever library facilities
# are not installed in the system libraries.
#
LIBDEPS = $(OBSTACK) $(ALLOCA) $(MALLOC)
LINTLIBS = $(OBSTACK_SRC) $(ALLOCA_SRC) $(MALLOC_SRC)

# How to link with both our special library facilities
# and the system's installed libraries.
#
LIBS = $(OBSTACK) $(GCC_ALLOCA) $(ALLOCA) $(GCC_MALLOC) $(MALLOC) \
	$(CLIB) $(LOADLIBES)

# Language-independent object files, borrowed from GCC.
# Therefore, they are in $(gccbin)
#
GCC_OBJS = $(gccbin)/tree.o $(gccbin)/print-tr.o $(gccbin)/stmt.o	\
	$(gccbin)/calls.o $(gccbin)/expmed.o $(gccbin)/explow.o		\
	$(gccbin)/varasm.o $(gccbin)/rtl.o $(gccbin)/print-rt.o		\
	$(gccbin)/rtlanal.o $(gccbin)/emit-rtl.o $(gccbin)/real.o	\
	$(gccbin)/sdbout.o $(gccbin)/dwarfout.o $(gccbin)/xcoffout.o	\
	$(gccbin)/integrat.o $(gccbin)/jump.o $(gccbin)/cse.o		\
	$(gccbin)/loop.o $(gccbin)/unroll.o $(gccbin)/flow.o		\
	$(gccbin)/stupid.o $(gccbin)/combine.o $(gccbin)/regclass.o	\
	$(gccbin)/local-al.o $(gccbin)/global.o $(gccbin)/reload.o	\
	$(gccbin)/reload1.o $(gccbin)/caller-s.o			\
	$(gccbin)/insn-pee.o $(gccbin)/reorg.o $(gccbin)/sched.o	\
	$(gccbin)/final.o $(gccbin)/recog.o $(gccbin)/reg-stac.o	\
	$(gccbin)/insn-opi.o $(gccbin)/insn-rec.o			\
	$(gccbin)/insn-ext.o $(gccbin)/insn-out.o			\
	$(gccbin)/insn-emi.o $(gccbin)/insn-att.o			\
	$(gccbin)/getpwd.o $(gccbin)/$(out_object_file) $(EXTRA_OBJS)	\
	$(gccbin)/c-bounds.o

# Language-independent SOURCE files. For LINT and ETAGS only
#
# See also $(GPC_LANG_IND) They should come back to this
# list when compilers are merged.
# (most of the changes don't affect the C-compiler anyway)
#
GCC_SRC = $(gccsrc)/tree.c $(gccsrc)/print-tree.c $(gccsrc)/stmt.c	\
	$(gccsrc)/calls.c $(gccsrc)/expmed.c $(gccsrc)/explow.c		\
	$(gccsrc)/varasm.c $(gccsrc)/rtl.c $(gccsrc)/print-rt.c		\
	$(gccsrc)/rtlanal.c $(gccsrc)/emit-rtl.c $(gccsrc)/real.c	\
	$(gccsrc)/sdbout.c $(gccsrc)/dwarfout.c $(gccsrc)/xcoffout.c	\
	$(gccsrc)/integrat.c $(gccsrc)/jump.c $(gccsrc)/cse.c		\
	$(gccsrc)/loop.c $(gccsrc)/unroll.c $(gccsrc)/flow.c		\
	$(gccsrc)/stupid.c $(gccsrc)/combine.c $(gccsrc)/regclass.c	\
	$(gccsrc)/local-al.c $(gccsrc)/global.c $(gccsrc)/reload.c	\
	$(gccsrc)/reload1.c $(gccsrc)/caller-s.c			\
	$(gccsrc)/insn-pee.c $(gccsrc)/reorg.c $(gccsrc)/sched.c	\
	$(gccsrc)/final.c $(gccsrc)/recog.c $(gccsrc)/reg-stac.c	\
	$(gccsrc)/insn-opi.c $(gccsrc)/insn-rec.c			\
	$(gccsrc)/insn-ext.c $(gccsrc)/insn-out.c			\
	$(gccsrc)/insn-emi.c $(gccsrc)/insn-att.c			\
	$(gccsrc)/getpwd.c $(gccsrc)/c-bounds.c

# These object files should be language independent but are currently
# not so. Built from gpc $(srcdir) into current directory.
#
GPC_LANG_IND_OBJS = stor-lay.o dbxout.o expr.o fold-con.o		\
	optabs.o convert.o function.o setop.o toplev.o

GPC_LANG_IND_SRC = $(srcdir)/stor-layout.c $(srcdir)/dbxout.c		\
	$(srcdir)/expr.c $(srcdir)/fold-const.c $(srcdir)/optabs.c	\
	$(srcdir)/convert.c $(srcdir)/function.c $(srcdir)/setop.c	\
	$(srcdir)/toplev.c

# Language specific object files for Pascal
#
# Currently the (c-aux-info.c,c-iterate.c) file is not modified
# so I use the original C-compiler file
#
GPC_OBJS_1 = gpc-pars.o gpc-decl.o gpc-type.o gpc-conv.o		\
	gpc-util.o gpc-modu.o gpc-lex.o gpc-comm.o gpc-lang.o		\
	$(gccbin)/c-aux-in.o $(gccbin)/c-iterat.o			\
	$(gccbin)/c-pragma.o						\
	$(GPC_LANG_IND_OBJS)

GPC_SRC_1 = $(srcdir)/gpc-pars.c $(srcdir)/gpc-decl.c			\
	$(srcdir)/gpc-type.c $(srcdir)/gpc-conv.c			\
	$(srcdir)/gpc-util.c $(srcdir)/gpc-modu.c $(srcdir)/gpc-lex.c	\
	$(srcdir)/gpc-comm.c $(srcdir)/gpc-lang.c			\
	$(gccsrc)/c-aux-in.c $(gccsrc)/c-iterat.c			\
	$(gccsrc)/c-pragma.c $(GPC_LANG_IND_SRC)

# Object files that make up the GNU Pascal compiler
GPC_OBJS = gpc-vers.o $(GPC_OBJS_1) $(GCC_OBJS)

# All GPC source files.
GPC_SRC = gpc-vers.c $(srcdir)/gpc-defs.h $(srcdir)/gpc-gper.c	\
	$(GPC_SRC_1)

# Files specific to the C interpreter bytecode compiler(s).
BC_OBJS = $(gccbin)/bc-emit.o $(gccbin)/bc-optab.o

TAGS_FILES = $(srcdir)/gpc-defs.h $(srcdir)/gpc-gper.c $(GPC_SRC_1)	\
	$(GCC_SRC) $(srcdir)/gpc-parse.y $(gccsrc)/*.h

MISC_FILES = $(srcdir)/COPYING $(srcdir)/COPYING.LIB			\
	$(srcdir)/ChangeLog $(srcdir)/INSTALL $(srcdir)/Makefile.in	\
	$(srcdir)/PROBLEMS $(srcdir)/README $(srcdir)/TAGS		\
	$(srcdir)/aclocal.m4 $(srcdir)/config.guess			\
	$(srcdir)/config.sub $(srcdir)/configure $(srcdir)/configure.in	\
	$(srcdir)/gpc-defs.h $(srcdir)/gpc-gper.c			\
	$(srcdir)/gpc-pars.y $(srcdir)/gpc-vers.in 			\
	$(srcdir)/gpc.gperf $(srcdir)/install-sh			\
	$(srcdir)/move-if-change

SUB_DIRS = $(srcdir)/contrib/* $(srcdir)/doc/* $(srcdir)/emx/*		\
	$(srcdir)/old_doc/* $(srcdir)/rts/* $(srcdir)/test/* 

TAR_FILES = $(GPC_SRC_1) $(MISC_FILES) $(SUB_DIRS)

all: gpc.exe gpc-cpp.exe gpc1.exe

copyobj: tree.o

tree.o: $(gccdosbin)\tree.o
	copy $(gccdosbin)\tree.o .
	copy $(gccdosbin)\print-tr.o .
	copy $(gccdosbin)\stmt.o .
	copy $(gccdosbin)\calls.o .
	copy $(gccdosbin)\expmed.o .
	copy $(gccdosbin)\explow.o .
	copy $(gccdosbin)\varasm.o .
	copy $(gccdosbin)\rtl.o .
	copy $(gccdosbin)\print-rt.o .
	copy $(gccdosbin)\rtlanal.o .
	copy $(gccdosbin)\emit-rtl.o .
	copy $(gccdosbin)\real.o .
	copy $(gccdosbin)\sdbout.o .
	copy $(gccdosbin)\dwarfout.o .
	copy $(gccdosbin)\xcoffout.o .
	copy $(gccdosbin)\integrat.o .
	copy $(gccdosbin)\jump.o .
	copy $(gccdosbin)\cse.o .
	copy $(gccdosbin)\loop.o .
	copy $(gccdosbin)\unroll.o .
	copy $(gccdosbin)\flow.o .
	copy $(gccdosbin)\stupid.o .
	copy $(gccdosbin)\combine.o .
	copy $(gccdosbin)\regclass.o .
	copy $(gccdosbin)\local-al.o .
	copy $(gccdosbin)\global.o .
	copy $(gccdosbin)\reload.o .
	copy $(gccdosbin)\reload1.o . 
	copy $(gccdosbin)\caller-s.o .
	copy $(gccdosbin)\insn-pee.o .
	copy $(gccdosbin)\reorg.o .
	copy $(gccdosbin)\sched.o .
	copy $(gccdosbin)\final.o .
	copy $(gccdosbin)\recog.o .
	copy $(gccdosbin)\reg-stac.o .
	copy $(gccdosbin)\insn-opi.o .
	copy $(gccdosbin)\insn-rec.o .
	copy $(gccdosbin)\insn-ext.o .
	copy $(gccdosbin)\insn-out.o .
	copy $(gccdosbin)\insn-emi.o .
	copy $(gccdosbin)\insn-att.o .
	copy $(gccdosbin)\getpwd.o .
	copy $(gccdosbin)\$(out_object_file) .
	copy $(gccdosbin)\c-bounds.o .
	copy $(gccdosbin)\obstack.o .
	copy $(gccdosbin)\cexp.o .
	copy $(gccdosbin)\c-aux-in.o .
	copy $(gccdosbin)\c-iterat.o .
	copy $(gccdosbin)\c-pragma.o .
	copy $(gccdosbin)\bc-emit.o .
	copy $(gccdosbin)\bc-optab.o .
#	touch stor-layout.c
#	touch dbxout.c
#	touch expr.c
#	touch fold-const.c
#	touch optabs.c
#	touch convert.c 
#	touch function.c
#	touch setop.c
#	touch toplev.c

gpc.exe: copyobj gcc.o  $(LIBDEPS) gpc-vers.o
	$(CC) $(ALL_CFLAGS) $(LDFLAGS) -o gpc.exe gcc.o gpc-version.o $(LIBS)

gpc-cpp.exe: copyobj gpc-cccp.o $(gccbin)/cexp.o gpc-vers.o $(LIBDEPS)
	$(CC) $(ALL_CFLAGS) $(LDFLAGS) -o gpc-cpp.exe gpc-cccp.o $(gccbin)/cexp.o gpc-version.o $(LIBS)

gpc1.exe: copyobj gpc-gper.c $(GPC_OBJS) $(BC_OBJS)
	$(CC) $(ALL_CFLAGS) $(LDFLAGS) -o gpc1.exe $(GPC_OBJS) $(BC_OBJS) $(LIBS)

# Generate fresh config file.
#specs: gpc
#	gpc -dumpspecs > specs

gcc.o: gcc.c $(gccsrc)/$(CONFIG_H) $(gccsrc)/multilib.h
	$(CC) $(ALL_CFLAGS) $(CPPFLAGS) $(INCLUDES) \
	  -DSTANDARD_STARTFILE_PREFIX=\"$(libdir)/\" \
	  -DSTANDARD_EXEC_PREFIX=\"$(libdir)/gcc-lib/\" \
	  -DDEFAULT_TARGET_VERSION=\"$(gcc_version)\" \
	  -DDEFAULT_TARGET_MACHINE=\"$(target)\" \
	  -DTOOLDIR_BASE_PREFIX=\"$(exec_prefix)/\" \
	  $(MAYBE_TARGET_DEFAULT) \
          -c gcc.c

gpc-cccp.o: $(gccsrc)/$(CONFIG_H) $(gccsrc)/pcp.h
	$(CC) $(ALL_CFLAGS) $(CPPFLAGS) $(INCLUDES) \
	  -DGCC_INCLUDE_DIR=\"$(libsubdir)/include\" \
	  -DGPLUSPLUS_INCLUDE_DIR=\"$(gxx_include_dir)\" \
	  -DLOCAL_INCLUDE_DIR=\"$(includedir)\" \
	  -DCROSS_INCLUDE_DIR=\"$(libsubdir)/sys-include\" \
	  -DTOOL_INCLUDE_DIR=\"$(tooldir)/include\" \
          -c gpc-cccp.c

gpc-parse.o : $(srcdir)/gpc-pars.c gpc-pars.h

gpc-common.o: gpc-comm.c gpc-pars.h

gpc-lex.o: gpc-lex.c gpc-gper.c gpc-pars.h

gpc-util.o: gpc-util.c $(srcdir)/rts/rts-type.h gpc-pars.h 

# Generate a new edit number
gpc-vers.c:
	@copy config\msdos\gpc-vers.c gpc-vers.c > NUL

# Generate this to the source directory
$(srcdir)/gpc-gper.c: gpc.gperf
	$(GPERF) $(GPERF_FLAGS) $(srcdir)/gpc.gperf > $(srcdir)/gpc-gperf.c

$(srcdir)/gpc-pars.c: gpc-pars.y
	$(BISON) $(BISON_FLAGS) -o tmp-gpc-parse.c $(srcdir)/gpc-parse.y
	sh $(srcdir)/move-if-change tmp-gpc-parse.h $(srcdir)/gpc-parse.h
	sh $(srcdir)/move-if-change tmp-gpc-parse.c $(srcdir)/gpc-parse.c

info:
	cd doc
	$(MAKE) info INFODIR=$(infodir);

library: rts/libgpc.a

# The runtime system.
rts/libgpc.a:
	cd rts
	$(MAKE) RTSFLAGS="$(RTSFLAGS)"

info-clean:
	cd doc
	$(MAKE) clean;


library-clean:
	cd rts
	$(MAKE) clean

clean: library-clean
	@del gpc1 gpc gpc-cpp
	@del *.o *.co *.s *.s[0-9] 
	@del *.greg *.lreg *.combine *.flow *.cse *.jump *.rtl *.tree *.loop
	@del core .stampdir

library-distclean:
	cd rts
	$(MAKE) distclean

distclean: clean library-distclean
	@del *~ *.BAK *.gz
	@del config.cache config.log 
	@del gpc-version.c version gnuvers.conf
	@del Makefile

# Install GPC. Assume we're in gnu/gpc-120 and bindir is ../../bin
install-gpc: all
	copy gpc.exe ..\..\bin\gpc.exe
	copy gpc1.exe ..\..\bin\gpc1.exe
	copy gpc-cpp.exe ..\..\bin\gpc-cpp.exe

install-info:
	cd doc
	$(MAKE) install INFODIR=$(infodir)

install: install-gpc 

# Uninstall GPC. leave (lib)gcc.a and specs alone ; they might be in use by GCC
uninstall:
	@del ..\..\bin\gpc.exe
	@del ..\..\bin\gpc1.exe
	@del ..\..\bin\gpc-cpp.exe
	@del ..\..\lib\gpc.a
	@del ..\..\lib\specs.gpc

# GPC tags file
#
TAGS: $(TAGS_FILES)
	etags $(TAGS_FILES)

#Ignore whether these exist.
.PHONY: clean TAGS install backup ftp test

# Maintainer make options:
#
#    make maintainer-clean : remove everything that can be regenerated, even
#                            if it belongs in the distribution.
#    make distdir          : recover from maintainer-clean
#    make bindist
#    (make srcdist) !given up on this one. Not even gcc has this working...
#             do make extraclean instead

# Delete anything likely to be found in the source directory
# that shouldn't be in the distribution.
extraclean: distclean
	-rm -rf =* ./"#"* *~* rts/=* rts/"#"* rts/*~*
	-rm -rf patch* *.orig *.rej rts/patch* rts/*.orig rts/*.rej
	-rm -rf rts/*/=* rts/*/"#"* rts/*/*~*
	-rm -rf rts/*/*.orig rts/*/*.rej
	-rm -rf *.dvi *.ps *.oaux *.d *.[zZ] *.gz
	-rm -rf *.tar *.xtar *diff *.diff.* *.tar.* *.xtar.* *diffs
	-rm -rf *lose rts/*lose rts/*/*lose
	-rm -rf *.s *.s[0-9] *.i install1.texi rts/ChangeLog
	-rm -rf */=* */"#"* */*~*
	-rm -rf */patch* */*.orig */*.rej
	-rm -rf */*.dvi */*.oaux */*.d */*.[zZ] */*.gz
	-rm -rf */*.tar */*.xtar */*diff */*.diff.* */*.tar.* */*.xtar.* */*diffs
	-rm -rf */*lose */*.s */*.s[0-9] */*.i

# Get rid of every file that's generated from some other file.
# Most of these files ARE PRESENT in the GPC distribution.
# You'll need gperf, etags and autoconf to recover from this one.
maintainer-clean: extraclean
	-rm -rf configure TAGS
	-rm -rf gpc-gperf.c gpc-parse.c gpc-parse.h gpc-parse.output
	-rm -rf gpc.info* gpc.??s gpc.*aux

# Create files that can be rebuilt but go in the distribution anyway.
distdir: gpc-pars.c gpc-pars.y TAGS

# Create a GPC unix binary distribution
bindist:
	@del gpc-$(gpc_version)-$(gcc_version).$(target).tar.gz
	install -d ./bindist/$(prefix)	
	$(MAKE) install prefix=./bindist/$(prefix)
	cd ./bindist && $(TAR) $(TAR_FLAGS) ../gpc-$(gpc_version)-$(gcc_version).$(target).tar.gz *
	-rm -rf bindist

lint-gpc:
	$(LINT) $(LINT_FLAGS) $(LDFLAGS) $(INCLUDES) $(GPC_SRC) $(GCC_SRC) $(LINTLIBS)

# Pascal Validation Suite tests
#
pvsdir = /src/gnu/gpc/pvs

# The following tests assume you have installed the
# Pascal Validation Suite to $(pvsdir)
#
pvs-link:
	-rm pvs pvsc
	-ln -s $(pvsdir) pvs
	-ln -s $(pvsdir)/CONFORMANCE pvsc

pvs-test: pvs-link
	cd pvsc; ./run

# Create a TAR copy of the GPC sources
# Run gpcdist from the generated makefile.
# BROKEN
gpcdist:
	V=$(gcc_version) ; echo $(TAR_FILES) | sed -e "s,$(srcdir),$$V,g" > /tmp/gpc.files; \
	cd $(srcdir)/.. && $(TAR) $(TAR_FLAGS) gpc-$(gpc_version)-$$V.tar.gz `cat /tmp/gpc.files`
	@del /tmp/gpc.files

all-diffs: ind-diff lang-diff driver-diff

# take diffs of language independent files
ind-diff:
	-mkdir DIFF
	-for i in stor-layout.c dbxout.c expr.c fold-const.c optabs.c convert.c function.c toplev.c ; do \
		$(DIFF) $(gccsrc)/$$i $(srcdir)/$$i > DIFF/$${i}.diff; echo $$i; \
	done	

# take diffs of language dependent files
lang-diff:
	-mkdir DIFF-OTHER
	-for i in gpc-parse.c gpc-decl.c gpc-typeck.c gpc-convert.c  \
		  gpc-common.c gpc-lang.c ; do   \
	   $(DIFF) $(gccsrc)/`echo $$i | sed -e "s/gpc-/c-/"` $(srcdir)/$$i > DIFF-OTHER/$${i}.diff; echo $$i; \
	done

driver-diff:
	-mkdir DIFF-DRIVER
	-$(DIFF) $(gccsrc)/gcc.c $(srcdir)/gcc.c > DIFF-DRIVER/gcc.c.diff; echo gcc.c
	-$(DIFF) $(gccsrc)/cccp.c $(srcdir)/gpc-cccp.c > DIFF-DRIVER/gpc-cccp.c.diff; echo gpc-cccp.c

