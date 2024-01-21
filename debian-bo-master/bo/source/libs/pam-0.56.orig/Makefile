##
## $Id: Makefile,v 1.30 1997/02/15 19:29:28 morgan Exp $
##
## $Log: Makefile,v $
## Revision 1.30  1997/02/15 19:29:28  morgan
## major and minor numbers are set now in top level
##
## Revision 1.29  1997/01/04 19:02:41  morgan
## make -> $(MAKE)
##
## Revision 1.28  1996/12/01 03:29:52  morgan
## update for .54
##
##

# major and minor numbers of this release
MAJOR_REL=0
MINOR_REL=56

# this should be the name of this directory
RELNAME = Linux-PAM-$(MAJOR_REL).$(MINOR_REL)

# this is the name of the archive file
DISTFILE = $(RELNAME).tar.gz

# Comment out either line to disable that type of linking for *modules only*
# Both at once is a legal configuration!
DYNAMIC=-DPAM_DYNAMIC
#STATIC=-DPAM_STATIC

# Comment out these lines to disable building dynamic/static libpam.*
DYNAMIC_LIBPAM=yes
#STATIC_LIBPAM=yes

# All combinations of the above four variable definitions are legal,
# however, not defining either dynamic or static modules and yet
# creating a some flavor of LIBPAM will make an authentication library
# that always fails!

# Here we indicate which libraries are present on the local system
# they control the building of some modules in this distribution
# Note, these definitions are all "export"ed below...

HAVE_PWDBLIB=yes
HAVE_CRACKLIB=yes
HAVE_AFSLIBS=no
HAVE_KRBLIBS=no

# NB. The following is the generic defines for compilation.
# They can be overridden in the default.defs file below
#
WARNINGS = -ansi -D_POSIX_SOURCE -Wall -Wwrite-strings \
        -Wpointer-arith -Wcast-qual -Wcast-align \
        -Wtraditional -Wstrict-prototypes -Wmissing-prototypes \
        -Wnested-externs -Winline -Wshadow -pedantic
PIC=-fPIC

# Here we include the defines for the preferred operating system
# these include things like CC, CFLAGS and destination directories 
# etc.. By default, this is a symbolic link to one of the .defs files
# the .../defs/ directory. Please take a moment to check that you are
# using the correct one.

include default.defs

# Uncomment this for the pam_fail_delay() libpam function to be
# activated. There is an example of how it might be used in
# examples/blank.c and also in the pam_unix module
EXTRAS += -DPAM_FAIL_DELAY_ON

# to turn on the fprintf(stderr, ..) debugging lines throughout the
# distribution uncomment this line
#EXTRAS += -DDEBUG

#
# Changing the next defines requires that you 'make sterile'
# before you next do a generic make.
#

# For serious memory allocation tracing uncomment the following
#MEMORY_DEBUG=-DMEMORY_DEBUG

#######################################################################
# The pam_unix module in this file will not work on NIS based systems.#
#######################################################################

# ////////////////////////////////////////////////////
# // You should not modify anything below this line //
# ////////////////////////////////////////////////////

# the sub-directories to make things in

DIRS = modules libpam conf libpam_misc examples

#
# Conditional defines..
#

ifdef DYNAMIC
# need the dynamic library functions
LIBDL=-ldl
ifdef STATIC_LIBPAM
# needed because pam_xxx() fn's are now in statically linked library
RDYNAMIC = -rdynamic
endif
endif

#
# basic defines
#

INCLUDEDIR=-I$(shell pwd)/include
PAMLIB=-L$(shell pwd)/libpam -lpam

# This is Linux-PAM and not a version from Sun etc..
# [Note, this does not describe the operating system you are using
# only that you are compiling the "Linux" (read FREE) implementation
# of Pluggable Authentication Modules.
EXTRAS += -DLINUX_PAM

#
# build composite defines
#

LOADLIBES = $(PAMLIB) $(RDYNAMIC) -L$(shell pwd)/libpam_misc -lpam_misc \
	$(LIBDL) $(ULIBS)
CFLAGS += $(EXTRAS) $(MEMORY_DEBUG) $(WARNINGS) $(INCLUDEDIR) $(PIC)

#
# export the libraries-available info; the modules should know how
# to deal with this...
#
export HAVE_PWDBLIB		
export HAVE_CRACKLIB
export HAVE_AFSLIBS
export HAVE_KRBLIBS

#
# generic exports
#
export MAJOR_REL                # the major release of this distribution
export MINOR_REL		# the minor release of this distribution
export OS			# operating system
export ARCH			# architecture
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
export PAMLIB			# where to find the local libpam.xx file
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
ifdef DYNAMIC 
export DYNAMIC
endif
ifdef STATIC
export STATIC
endif
ifdef DYNAMIC_LIBPAM
export DYNAMIC_LIBPAM
endif
ifdef STATIC_LIBPAM
export STATIC_LIBPAM
endif
ifdef MEMORY_DEBUG
export MEMORY_DEBUG
endif

##
## the rules
##

all: .freezemake
	@for i in $(DIRS) ; do \
		$(MAKE) -C $$i all ; \
		if [ $$? -ne 0 ]; then break ; fi ; \
	done

.freezemake: Makefile
	@touch .freezemake
	@echo "*WARNING*: If you are running a system that is dependent"
	@echo "  on PAM to work. DO NOT make sterile NOR make remove."
	@echo "  These options will delete the PAM files on your system"
	@echo "  and make it unusable!"
	@echo ""
	@echo "If you are in any doubt, just do 'make all' (or just"
	@echo "'make'). It is likely that this is the SAFEST thing to do...."
	@exit 1

install:
	@for i in $(DIRS) ; do \
		$(MAKE) -C $$i install ; \
		if [ $$? -ne 0 ]; then break ; fi ; \
	done

sterile: .freezemake
	@$(MAKE) remove
	@$(MAKE) extraclean

remove: .freezemake
	@for i in $(DIRS) ; do \
		$(MAKE) -C $$i remove ; \
	done

clean:
	@rm -f *~ core
	@for i in $(DIRS) ; do \
		$(MAKE) -C $$i clean ; \
	done

extraclean:
	@for i in $(DIRS) doc; do \
		$(MAKE) -C $$i extraclean ; \
	done

check:
	@$(MAKE) -C conf check

RCScheck:
	@$(MAKE) -C conf RCScheck

# this can be used to see what hasn't been check'd into RCS

open:
	@find . \( -type f -a -perm 644 \) -print

release:
	@$(MAKE) extraclean
	@rm -f .filelist .RCSlist .freezemake
	@cd .. ; find $(RELNAME) \! -type d -print | grep -v RCS | grep -v 'conf/.md5sum' > $(RELNAME)/.filelist
	@chmod 444 .filelist
	@$(MAKE) check
	@(cat .filelist ; echo $(RELNAME)/conf/.md5sum) | (cd .. ; tar -cz -f$(DISTFILE) -T-)
	@cd .. ; find $(RELNAME) -type f -print | grep RCS | fgrep -v 'conf/.RCSsum' > $(RELNAME)/.RCSlist
	@chmod 444 .RCSlist
	@$(MAKE) RCScheck
	@(cat .RCSlist ; echo $(RELNAME)/conf/.RCSsum) | (cd .. ; tar -cz -fRCS+$(DISTFILE) -T-)
