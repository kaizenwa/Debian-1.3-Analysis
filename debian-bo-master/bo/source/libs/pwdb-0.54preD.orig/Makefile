##
## $Id: Makefile,v 1.26 1996/09/05 06:55:22 morgan Exp morgan $
##
## $Log: Makefile,v $
##

# this should be the name of this directory
RELNAME = libpwdb-0.54preD

# this is the name of the archive file
DISTFILE = $(RELNAME).tar.gz

# Here we include the defines for the preferred operating system
# these include things like CC, CFLAGS and destination directories 
# etc.. By default, this is a symbolic link to one of the .defs files
# the .../defs/ directory. Please take a moment to check that you are
# using the correct one.

include default.defs

# For extra debugging information
# EXTRAS += -DDEBUG

# ////////////////////////////////////////////////////
# // You should not modify anything below this line //
# ////////////////////////////////////////////////////

# the sub-directories to make things in

DIRS = libpwdb conf examples

#
# Conditional defines..
#

#
# basic defines
#
CRTDIR = $(shell pwd)

WARNINGS += -ansi -D_POSIX_SOURCE -Wall -Wwrite-strings \
        -Wpointer-arith -Wcast-qual -Wcast-align \
        -Wtraditional -Wstrict-prototypes -Wmissing-prototypes \
        -Wnested-externs -Winline -Wshadow -pedantic
INCLUDEDIR=-I$(CRTDIR)/include

#
# build composite defines
#

LOADLIBES = -L$(shell pwd)/libpwdb -lpwdb $(ULIBS)
CFLAGS += $(EXTRAS) $(WARNINGS) $(INCLUDEDIR) -fPIC

#
# generic exports
#

export CC			# the C compiler
export INSTALL			# to do instalations with
export MKDIR			# to ensure directories exist
export CFLAGS			# CC flags used to compile everything
export LD_D			# build a shared object file (module)
export LD			# build a generic library
export LDCONFIG			# rebuild the shared libraries
export AR			# build a static library
export RANLIB			# reorder a static library
export LOADLIBES		# libraries needed for application linking

#
# where to install things
#

export PREFIX			# the basic prefix for all other directories
export INCLUDED			# where to store pam---.h files
export SUPLEMENTED		# where to store module helper binaries
export CONFIGED			# where pam.conf file goes
export LIBDIR			# where libpam and libpam_misc go
export SECUREDIR		# where the modules will be placed

#
# Conditional exporting ( ... these go on for a while... )
#

##
## the rules
##

all:
	@for i in $(DIRS) ; do \
		make -C $$i all ; \
		if [ $$? -ne 0 ]; then break ; fi ; \
	done

install:
	for i in $(DIRS) ; do \
		make -C $$i install ; \
		if [ $$? -ne 0 ]; then break ; fi ; \
	done

sterile:
	@make remove
	@make extraclean

remove:
	for i in $(DIRS) ; do \
		make -C $$i remove ; \
	done

clean:
	rm -f *~ core
	for i in $(DIRS) ; do \
		make -C $$i clean ; \
	done
	make -C doc clean

extraclean: clean

check:
	make -C conf check

RCScheck:
	make -C conf RCScheck

# this can be used to see what hasn't been check'd into RCS

open:
	find . \( -type f -a -perm 644 \) -print

release:
	@make extraclean
	rm -f .filelist .RCSlist .freezemake
	cd .. ; find $(RELNAME) \! -type d -print | grep -v RCS | grep -v 'conf/.md5sum' > $(RELNAME)/.filelist
	chmod 444 .filelist
	@make check
	(cat .filelist ; echo $(RELNAME)/conf/.md5sum) | (cd .. ; tar -cz -f$(DISTFILE) -T-)
#	cd .. ; find $(RELNAME) -type f -print | grep RCS | fgrep -v 'conf/.RCSsum' > $(RELNAME)/.RCSlist
#	chmod 444 .RCSlist
#	@make RCScheck
#	(cat .RCSlist ; echo $(RELNAME)/conf/.RCSsum) | (cd .. ; tar -cz -fRCS+$(DISTFILE) -T-)
