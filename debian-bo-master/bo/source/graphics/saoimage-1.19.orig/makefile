#
# Makefile for SAOimage
#
# To build:
#	% make aix		# IBM RS/6000 running AIX
#	% make alliant		# Alliant FX series
#	% make alpha		# DEC Alpha AXP Digital Unix
#	% make apollo		# Any Apollo, software floating point
#	% make apollo_68882	# Apollo for MC68882 fp
#	% make apollo_fpa	# Apollo for FPA1
#	% make apollo_prism	# Apollo for PRISM architecture (a88k)
#	% make hp		# HP-UX
#	% make linux		# Intel PC running Linux
#	% make mips		# MIPStation running RISCOS
#	% make sgi		# SGI Personal Iris running Irix
# 	% make sun		# All Sun architectures, maybe software fp
#	% make sun_sparc	# Sun SPARC
#	% make sun_ffpa		# Sun-3 floating point accelerator
#	% make sun_f68881	# Sun-3 motorola f68881 floating point
#	% make sun_fswitch	# All Suns, switchable floating point
#	% make ultrix		# DECstation and VAXstation
#
# The full "make install" both installs SAOimage in Unix and sets up for
# communications with IRAF:
#	% make install
#	(or, % make install SAOBINDIR = "/ourown/local/bindir")
#
# Partial installs may also be used:
#	% make install_saobin	# just install binary (can add SAOBINDIR)
#	% make install_saoman	# just install manual page
#	% make install_fifo	# just install iraf fifo's
#	% make install_imtoolrc	# just install iraf imtoolrc file
#
# NOTICE: You can't run the installation targets from this umbrella makefile.
# You must run the correct makefile.<machine> for the workstation you are
# installing on.  For instance,
#
#	% make -f makefile.dec install
#
# Other options:
#	% make manual		# make manual.dvi file in doc subdirectory
#	% make clean		# clean up object & miscellaneous other files
#	% make tar		# make saoimage.tar in CWD
#	% make -s pipe		# create tarfile on stdout for piping
#	% make tape		# create tarfile on /dev/nrxt8
#
# Make macros (parameters) that can be specified on command line:
#	SAOBINDIR		# Destination directory for SAOimage program
#	SAOMANDIR		# Destination directory for on-line manpage
#	ARCFILE			# Disk archive file name (used with make tar)
#	TAPEDEV			# Tape archive device name (used with make tape)

SAOIMAGE = saoimage

oops:
	@echo No architecture specified.  Please re-run as \"make ARCH\", \
	where ARCH is one of
	@grep '^[a-z].*:' makefile | grep -v $@ | sed 's/:.*$$//'

# It would be nicer to print out the supported architectures in a pretty
# table, but the AIX awk complains that END is a syntax error, so we will
# leave this out for now.

#	    awk '{printf ("    %-14.14s", $$0); np++; \
#		 if (np == 3) {printf ("\n"); np=0} \
#		 } END {if (np > 0) print ""}'


# Following are targets for all the supported architecures.  Note that there
# is a different makefile.<vendor> for each workstation manufacturer; this
# is sufficient at this stage.  The only changes that should be required in
# selecting a particular architecture is the adjustment of the ADFLAGS macro,
# which sets the architecture-dependent flags for compilation and linking.


aix:						# IBM RT6000 (AIX)
	$(MAKE) -f makefile.ibm $(SAOIMAGE) ARCH=$@ \
		ADFLAGS="-D_BSD"


alliant:					# Alliant FX
	$(MAKE) -f makefile.alnt $(SAOIMAGE) ARCH=$@


# For Apollo, note that you need ADFLAGS = -DXV11R2 for OS prior to SR10.2.
# The XV11R2 flag is necessary when libX11 does not contain an entry for
# "XVisualIDFromVisual"; the relevant ifdef is in clrinit.c.

apollo:						# Apollo, software floating pt
	$(MAKE) -f makefile.apo $(SAOIMAGE) ARCH=$@ \
		ADFLAGS="-W0,-opt,3,-ndb -L/usr/lib/X11"

apollo_68882:					# Apollo, MC68882 floating pt
	$(MAKE) -f makefile.apo $(SAOIMAGE) ARCH=$@ \
		ADFLAGS="-W0,-opt,3,-cpu,3000,-ndb -L/usr/lib/X11"

apollo_fpa:					# Apollo, Weitek FPA floating pt
	$(MAKE) -f makefile.apo $(SAOIMAGE) ARCH=$@ \
		ADFLAGS="-W0,-opt,3,-cpu,FPA1,-ndb -L/usr/lib/X11"

apollo_prism:					# Apollo, PRISM architecture
	$(MAKE) -f makefile.apo $(SAOIMAGE) ARCH=$@ \
		ADFLAGS="-W0,-opt,3,-cpu,a88k,-ndb -L/usr/lib/X11"

apollo_a88k:
	$(MAKE) apollo_prism

apollo_m68k:
	$(MAKE) apollo_68882


hp:						# Hewlett-Packard (HPUX)
	$(MAKE) -f makefile.hp $(SAOIMAGE) ARCH=$@

linux:						# Linux on Intel chip
	$(MAKE) -f makefile.linux $(SAOIMAGE) ARCH=$@

mips:						# MIPS, Inc. (RISCOS)
	$(MAKE) -f makefile.mips $(SAOIMAGE) ARCH=$@

sgi:						# SGI Personal Iris (Irix)
	$(MAKE) -f makefile.sgi $(SAOIMAGE) ARCH=$@ \
		ADFLAGS="-I/usr/include/sun -I/usr/include/bsd"

solaris:					# Sun Solaris
	$(MAKE) -f makefile.sol $(SAOIMAGE) ARCH=$@

sun:						# Sun generic => SPARC
	$(MAKE) sun_sparc

sun_sparc:					# Sun, sparc architecture
	$(MAKE) -f makefile.sun $(SAOIMAGE) ARCH=$@

sun_f68881:					# Sun3, mc68881 floating pt
	$(MAKE) -f makefile.sun $(SAOIMAGE) ARCH=$@ \
		ADFLAGS="-f68881"

sun_ffpa:					# Sun3, ffpa floating pt
	$(MAKE) -f makefile.sun $(SAOIMAGE) ARCH=$@ \
		ADFLAGS="-ffpa"

sun_fswitch:					# Suns, switchable fp
	$(MAKE) -f makefile.sun $(SAOIMAGE) ARCH=$@ \
		ADFLAGS="-fswitch"

alpha:						# DEC Alpha (Digital Unix)
	$(MAKE) -f makefile.alpha $(SAOIMAGE) ARCH=$@

ultrix:						# DECstation/VAXstation (Ultrix)
	$(MAKE) -f makefile.dec $(SAOIMAGE) ARCH=$@

dec:						# DEC (Ultrix)
	$(MAKE) ultrix


# Some of the utility targets are generic enough that we can just point to
# one of the architecture-specific makefiles.

MAKEUTIL = makefile.dec

ARCFILE = saoimage.tar
TAPEDEV = /dev/nrxt8

tar:
#	$(MAKE) -f makefile`ls .buildarch.* | sed 's/\.buildarch//'` $@
	$(MAKE) -f $(MAKEUTIL) $@ ARCFILE=$(ARCFILE)
pipe:
	$(MAKE) -f $(MAKEUTIL) -s tar ARCFILE=-
tape:
	$(MAKE) -f $(MAKEUTIL) tar ARCFILE=$(TAPEDEV)
clean:
	$(MAKE) -f $(MAKEUTIL) $@

install:
	@echo You can\'t run install from this umbrella makefile. \
		You must run the correct
	@echo makefile.machine for the workstation \
		you are installing on.  For instance,
	@echo
	@echo "	% make -f makefile.dec install"

manual:
	cd doc; latex $@
	@echo $@.dvi created in `pwd`.
	@echo You must run a DVI translator appropriate for \
		your printer, e.g., dvips.
