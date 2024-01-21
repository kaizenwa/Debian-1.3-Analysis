# (c) Copyright 1992, 1993 by Panagiotis Tsirigotis
# All rights reserved.  The file named COPYRIGHT specifies the terms 
# and conditions for redistribution.

#
# $Id: Makefile,v 3.2 1993/06/16 00:06:57 panos Exp $
#
# Based on Library makefile template: *Revision: 1.15 *
#

# 
# Available entries:
# 		lib 			--> creates the library
#		install		--> installs the library (archive, man page(s), header(s))
#		uninstall	--> uninstall the library
#		clean			--> removes all .o and .a files
#		spotless		--> clean + uninstall
# 		lint			--> lints a file (usage: make lint MODULE=foo.c)
#		tags			--> creates a tags file (from the SOURCES and HEADERS)
#		checkout 	--> checkout all files
#		dist			--> distribution support
#

NAME				= str
VERSION			= 1.4.1

HEADERS			= str.h strparse.h \
						ss_impl.h ss_rk.h ss_kmp.h ss_sbm.h ss_bmh.h ss_so.h
SOURCES			= strutil.c strprint.c strparse.c strs.c \
						ss_rk.c ss_kmp.c ss_bf.c ss_sbm.c ss_bmh.c ss_so.c
OBJECTS			= strutil.o strprint.o strparse.o strs.o \
						ss_rk.o ss_kmp.o ss_bf.o ss_sbm.o ss_bmh.o ss_so.o

MANFILES			= strparse.3 strprint.3 strutil.3 strs.3
INCLUDEFILES	= str.h

# The following variables are used by the 'install' entry and
# should be customized:
#     LIBDIR:     where the library will be placed
#     INCUDEDIR:  where the include files will be placed
#     MANDIR:     where the man pages will be placed
#
LIBDIR			= $(HOME)/.links/libraries/$(ARCH)
MANDIR			= $(HOME)/.links/manpages/man3
INCLUDEDIR		= $(HOME)/.links/includes

#
# Available flags
#	NBIC				: number of bits in a character variable (defaults to 8)
#	WIDE_INT			: widest integer supported by the CPU/compiler
#						  (defaults to 'long')
#	WIDE_INT_SIZE  : size of the WIDE_INT type in bits (defaults to 32);
#						  effective (and required) only when WIDE_INT is defined
#	NO_SIO			: if the SIO library is not available (results in turning
#						  all the string printing functions to no-ops)
#
DEFS				= 		# for example, -DNO_SIO
DEBUG				= -g				# -g or -O
VERSION_DEF		= -DVERSION=\"STR\ Version\ $(VERSION)\"

CPP_DEFS			= $(VERSION_DEF) $(DEFS)

#
# The following variables shouldn't need to be changed
#
LINT_FLAGS		= -hbux
CPP_FLAGS		= $(CPP_DEFS) -I$(INCLUDEDIR)
CC_FLAGS			= $(DEBUG)
CFLAGS			= $(CPP_FLAGS) $(CC_FLAGS)

INSTALL			= install -c
FMODE				= -m 640			# used by install
RANLIB			= ranlib

PAGER				= less


LIBNAME			= lib$(NAME).a

lib: $(LIBNAME)

libopt: clean
	make DEBUG=-O "DEFS=$(DEFS)" lib
	$(INSTALL) $(FMODE) $(LIBNAME) $(LIBDIR)-O

$(LIBNAME): $(OBJECTS)
	ar r $@ $?
	$(RANLIB) $@

LINT_IGNORE=possible pointer alignment|RCSid unused

lint:
	lint $(CPP_FLAGS) $(LINT_FLAGS) $(MODULE) 2>&1 | egrep -v '$(LINT_IGNORE)' | $(PAGER)

install: $(LIBNAME)
	@if test "$(LIBDIR)" -a "$(INCLUDEDIR)" -a "$(MANDIR)" ;\
	then \
		$(INSTALL) $(FMODE) $(LIBNAME) $(LIBDIR) ;\
		echo "Installed $(LIBNAME) to $(LIBDIR)" ;\
		for i in $(INCLUDEFILES); do $(INSTALL) $(FMODE) $$i $(INCLUDEDIR) ; done ;\
		echo Installed $(INCLUDEFILES) to $(INCLUDEDIR) ;\
		for i in $(MANFILES) ; do $(INSTALL) $(FMODE) $$i $(MANDIR) ; done ;\
		echo Installed $(MANFILES) to $(MANDIR) ;\
	else \
		echo "You forgot to set one of the following variables: LIBDIR,INCLUDEDIR,MANDIR" ;\
	fi

uninstall:
	a=`pwd` ; cd $(INCLUDEDIR) ;\
	if test $$a != `pwd` ; then rm -f $(INCLUDEFILES) ; fi
	a=`pwd` ; cd $(LIBDIR) ;\
	if test $$a != `pwd` ; then rm -f $(LIBNAME) ; fi
	a=`pwd` ; cd $(MANDIR) ;\
	if test $$a != `pwd` ; then rm -f $(MANFILES) ; fi

clean:
	rm -f $(OBJECTS) $(LIBNAME) core

spotless: clean uninstall

tags: $(SOURCES) $(HEADERS)
	ctags -w $(SOURCES) $(HEADERS)

checkout:
	co $(SOURCES) $(HEADERS) $(MANFILES)

#
# Distribution section
# This section contains the 2 targets for distribution support: dist, dirs
# "dist" checks out all files to be distributed
# "dirs" prints a list of directories to be included in the distribution.
# These directories should have a Makefile with a "dist" target
#
DISTRIBUTION_FILES	= $(SOURCES) $(HEADERS) $(MANFILES) COPYRIGHT README
DIRS						=

dist:
	-co -q $(DISTRIBUTION_FILES)

dirs:
	@echo $(DIRS)

#
# PUT HERE THE RULES TO MAKE THE OBJECT FILES
#
strparse.o:		strparse.h str.h
strprint.o:		str.h
strutil.o:		str.h
strs.o:			ss_impl.h str.h
ss_bf.o:			ss_impl.h
ss_rk.o:			ss_impl.h ss_rk.h
ss_kmp.o:		ss_impl.h ss_kmp.h
ss_sbm.o:		ss_impl.h ss_sbm.h
ss_bmh.o:		ss_impl.h ss_bmh.h
ss_so.o:			ss_impl.h ss_so.h


#
# Test program
#
tt: tt.c $(LIBNAME)
	$(CC) -I$(INCDIR) -g -o $@ tt.c $(LIBNAME) -L$(LIBDIR) -lsio -lmisc

