# Top-level makefile for Exim; handles creating a build directory with
# appropriate links, and then running the default makefile in that directory.

# Copyright (c) 1996 University of Cambridge.
# See the file NOTICE for conditions of use and distribution.

# IRIX make uses the shell that is in the SHELL variable, which often defaults
# to csh, so put this in to make it use the Bourne shell.

SHELL=/bin/sh

# If a build name has not been specified by running this make file via a
# command of the form "make build=xxxx", then determine the name of the
# operating system and the machine architecture and use that.

buildname=$${build:-`scripts/os-type`-`scripts/arch-type`}

# The default target tests for the existence of the build directory, and
# if is not present, creates it and use the MakeLinks script to put in
# symbolic links to the relevant source files. It then checks for the
# presence of a makefile in the build directory - this can get removed
# by some kinds of failure. It it's not there, it puts back a link to the
# base makefile, so that it will get rebuilt.

all:
	@builddir=build-$(buildname); \
	case "$$builddir" in *UnKnown*) exit 1;; esac; \
	sh -c "test -d $$builddir || (mkdir $$builddir; cd $$builddir; ../scripts/MakeLinks)"; \
	sh -c "test -r $$builddir/makefile -o -r $$builddir/Makefile || (cd $$builddir; ln -s ../OS/Makefile-Base Makefile;)"; \
	cd $$builddir; \
	$(MAKE) $(MFLAGS)

# The "install" and "clean" targets just pass themselves on to the build
# directory make file.

install clean:
	cd build-$(buildname); $(MAKE) $(MFLAGS) $@

# The "makefile" or "configure" target removes the build directory's make file,
# puts back a link to the default make file, and runs it, which has the effect
# of rebuilding the tailored make file.

makefile configure:
	cd build-$(buildname); \
	rm -f ?akefile; \
	ln -s ../OS/Makefile-Base Makefile; \
	$(MAKE) $(MFLAGS) makefile

# End of top-level makefile
