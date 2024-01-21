# (c) Copyright 1993 by Panagiotis Tsirigotis
# All rights reserved.  The file named COPYRIGHT specifies the terms 
# and conditions for redistribution.

#
# $Id: Makefile,v 2.2 1993/05/06 07:50:44 panos Exp $
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

NAME				= pq
VERSION			= 1.0.2

HEADERS			= hpq.h hpqimpl.h pq.h
SOURCES			= hpq.c pq.c
OBJECTS			= hpq.o pq.o

MANFILES			= pq.3
INCLUDEFILES	= hpq.h pq.h

# The following variables are used by the 'install' entry and
# should be customized:
#     LIBDIR:     where the library will be placed
#     INCUDEDIR:  where the include files will be placed
#     MANDIR:     where the man pages will be placed
#
LIBDIR			= $(HOME)/.links/libraries/$(ARCH)
MANDIR			= $(HOME)/.links/manpages/man3
INCLUDEDIR		= $(HOME)/.links/includes

DEFS				=				# used for command line defined flags
DEBUG				= -g			# -g or -O
VERSION_DEF		= -DVERSION=\"HPQ\ Version\ $(VERSION)\"

CPP_DEFS			= $(VERSION_DEF) $(DEFS)

#
# The following variables shouldn't need to be changed
#
LINT_FLAGS		= -hbux
CPP_FLAGS		= $(CPP_DEFS)
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
	$(INSTALL) $(FMODE) $(LIBNAME) $(LIBDIR)/optimized

$(LIBNAME): $(OBJECTS)
	ar r $@ $?
	$(RANLIB) $@

lint:
	lint $(CPP_FLAGS) $(LINT_FLAGS) $(MODULE) 2>&1 | $(PAGER)

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
DISTRIBUTION_FILES	= $(HEADERS) $(SOURCES) $(MANFILES) COPYRIGHT
DIRS						=

dist:
	-co -q $(DISTRIBUTION_FILES)

dirs:
	@echo $(DIRS)

#
# PUT HERE THE RULES TO MAKE THE OBJECT FILES
#
hpq.o: hpq.h hpqimpl.h pq.h

#
# Simple test program
#
pqtest: pqtest.c $(LIBNAME)
	cc $(DEBUG) -o $@ pqtest.c $(LIBNAME)

