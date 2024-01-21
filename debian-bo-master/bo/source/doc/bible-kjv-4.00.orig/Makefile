# -*-Fundamental-*-
###############################################################################
#
# File:         Makefile
# RCS:          $Header: Makefile,v 1.27 93/04/27 12:57:55 chip Exp $
# Description:  Makefile for Bible Retrieval System
# Author:       Chip Chapin, Hewlett Packard Company
# Created:      Thu Aug 24 17:26:25 1989
# Modified:     Tue Apr 27 12:57:45 1993 (Chip Chapin) chip@hpclbis
# Language:     make
# Package:      Bible Retrieval System
# Status:       Experimental (Do Not Distribute)
#
###############################################################################
#
# Revisions:
#
# Tue Apr 27 12:57:34 1993 (Chip Chapin) chip@hpclbis
#  Removed dependencies from "install" target.
###############################################################################
#
# $Log:	Makefile,v $
# Revision 1.27  93/04/27  12:57:55  12:57:55  chip (Chip Chapin)
# Removed dependencies from "install" target".
#
###############################################################################

# name of main program -- this determines a lot of other names
PROGRAM       = bible

# name of supplemental index file -- generated from raw data file
PROGINDEX     = $(PROGRAM)-index

README	      = README.$(PROGRAM)

# primary man page
MAN	      = $(PROGRAM).1

# raw data file
RAWDATA	      = $(PROGRAM).rawtext

# Derived data file.  Comes from processing raw data file.
DATA	      = $(PROGRAM).data

# Ninstall package
NINSTPKG      = $(PROGRAM).pkg

# where to install this stuff
DEST	      = /usr
#DEST	      = $(HOME)/local
DESTBIN	      = $(DEST)/bin
DESTLIB       = $(DEST)/lib
DESTMAN	      = $(DEST)/man
DESTMAN1      = $(DESTMAN)/man1

# I use environment variables for these...
#CFLAGS	      = -g
CFLAGS	      = -O2 -fwritable-strings -DDESTLIB=\"$(DESTLIB)\"

# release directories.  Nobody should care about this but me
FTPHOME	      = /mnt/ftp
RELEASEDIR    = $(FTPHOME)/pub/$(PROGRAM)
NINSTDIR      = /usr/adm/netdist
NINSTPKGDIR   = $(NINSTDIR)/packages

# This is only used by the author when making a new release ftp-able.
INSTALL	      = .do cp

LD	      = cc

MAKEFILE      = Makefile

PRINT	      = pr

SHELL	      = /bin/sh



###############################################################################
# Sources, objects, and tools
###############################################################################
# PROGOBJS and PROGSRCS are the components of the main program.
PROGOBJS      = $(PROGRAM).o \
		brl-index.o \
		brl.o \
		$(PROGINDEX).o \
		tsl.o \
    	    	util.o \
		compresslib.o

PROGSRCS      = $(PROGRAM).c \
		brl-index.c \
		brl.c \
		$(PROGINDEX).c \
		tsl.c \
    	    	util.c \
		compresslib.c

# TOOLOBJS and TOOLSRCS are objects and sources used in building TOOLS.
TOOLOBJS      = buildcmp.o \
		makeindex.o \
		makeconcfile.o \
		squish.o

TOOLSRCS      = buildcmp.c \
		makeindex.c \
		makeconcfile.c \
		squish.c

TOOLS	      = buildcmp squish makeindex makeconcfile

# TOOLS2 are additional scripts and data files used in constructing stuff.
TOOLS2	      = makeindex2 \
		makeconcordance \
		makeconc.pl \
		$(PROGRAM).stopwords

# HDRS lists all header files used by PROGRAM and TOOLS
HDRS	      = brl.h \
		brl-startverse.h \
		brl-startchapter.h \
		tsl.h \
		util.h \
		version.h


###############################################################################
# Distribution and version-control packages
###############################################################################
# SRCS includes all archivable sources except headers, both tools and program
SRCS	      = $(PROGSRCS) \
		$(TOOLSRCS) \
		$(TOOLS2)

# DOC includes all available program documentation
DOC	      = $(README) \
		README.ftp \
		$(MAN)

# List of files controlled by RCS
RCSFILES      = $(DOC) \
		$(SRCS) \
		$(HDRS) \
		$(MAKEFILE) \
		$(NINSTPKG) \
		testall

# PROGDIST contains only those files needed to build PROGRAM, plus doc.
PROGDIST      = $(HDRS) \
		$(PROGSRCS) \
		$(MAKEFILE) \
		$(DOC)

# DATADIST contains the fully-processed compressed data files
DATADIST      = $(DATA) $(DATA).conc

# TOOLSDIST contains all additional files needed to build a new
# version of this application from scratch, including re-processing data.
TOOLSDIST     = $(TOOLSRCS) \
		$(TOOLS2) \
		$(NINSTPKG) \
		testall 


###############################################################################
# TARGETS
###############################################################################
$(PROGRAM):	$(PROGOBJS)
		@echo "Linking $@ ..."
		@$(LD) $(LDFLAGS) $(PROGOBJS) -o $@
		@echo "done"


all:		$(PROGRAM) $(TOOLS) $(DATA) $(DATA).conc

buildcmp:	buildcmp.o compresslib.o util.o
		@echo "Linking $@ ..."
		@$(LD) $(LDFLAGS) $@.o compresslib.o util.o -o $@
		@echo "done"
		
squish:		squish.o
		@echo "Linking $@ ..."
		@$(LD) $(LDFLAGS) $@.o -o $@
		@echo "done"
		
makeindex:	makeindex.o
		@echo "Linking $@ ..."
		@$(LD) $(LDFLAGS) $@.o -o $@
		@echo "done"
		
makeconcfile:	makeconcfile.o util.o
		@echo "Linking $@ ..."
		@$(LD) $(LDFLAGS) $@.o util.o -o $@
		@echo "done"
		
brl-startverse.h brl-startchapter.h:	$(RAWDATA)
		./makeindex2 $(RAWDATA)


###############################################################################
# The nested makes are used on the following items to
# avoid establishing an explicit dependency on those files.
# Such a dependency causes lots of extra work.
$(DATA).conc:	$(PROGRAM).rawconcordance
		make makeconcfile
		./makeconcfile $@ < $(PROGRAM).rawconcordance

squish.stats squish.data:	$(RAWDATA)
		make squish
		if ./squish -w 65536 -v < $(RAWDATA) >squish.tmpdata; \
		then	mv squish.tmpdata squish.data; \
		else	rm squish.tmpdata squish.stats; fi
		

$(PROGINDEX).c:	$(RAWDATA)
		make makeindex
		./makeindex $(RAWDATA) > $@

$(DATA):	squish.stats squish.data
		make buildcmp
		./buildcmp
		mv squish.output $@


#
###############################################################################

# The next one ensures that $(PROGINDEX).c gets its dependency satisfied.
# Note that $(PROGINDEX).c is included in source distributions, while 
# $(RAWDATA) is NOT included in all distributions.  $(PROGINDEX).c does NOT 
# need to be rebuilt unless someone (probably me) has munged $(RAWDATA).
SAFEDATE = 0101000193
$(RAWDATA):
		@touch $(SAFEDATE) $@
		@if [ -r $(SAFEDATE) ]; then \
			/bin/rm $(SAFEDATE) $@; \
			if [ -x /usr/5bin/touch ]; then \
				/usr/5bin/touch $(SAFEDATE) $@; \
			else \
				# major kludge alert! ... \
				touch brl-startverse.h brl-startchapter.h; \
			fi; \
		fi
		@echo "  A dummy $(RAWDATA) file has been created."

# The dependency on $(RAWDATA) helps to ensure that the raw concordance file
# is not rebuilt unnecessarily.
$(PROGRAM).rawconcordance:	$(RAWDATA)
		@echo "To exclude \"stopwords\" from concordance use this:"
		@echo "     ./makeconc.pl bible.stopwords"
		@echo "NOTE: The next command will FAIL if you don't have PERL."
		@echo "  If it fails, edit Makefile to use this instead:"
		@echo "     ./makeconcordance"
		@echo "Here we go..."
		./makeconc.pl
#		./makeconcordance

$(MAN).gz:	$(MAN)
		/bin/rm $@
		gzip < $(MAN) > $(MAN).gz

clean:;		@rm -f $(PROGOBJS) $(TOOLOBJS) core

clobber:;	@rm -f $(PROGOBJS) $(TOOLOBJS) $(PROGRAM) $(TOOLS) $(DATA) \
			$(PROGINDEX).c squish.stats squish.data core tags

# depend:;	@mkmf -f $(MAKEFILE) ROOT=$(ROOT)

echo:;		@echo $(HDRS) $(SRCS)

index:;		@ctags -wx $(HDRS) $(SRCS)

print:;		@$(PRINT) $(HDRS) $(SRCS)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)



###############################################################################
# Distribution Targets
###############################################################################
shar:		$(PROGRAM).shar $(DATA).shar $(PROGRAM)-tools.shar

$(PROGRAM).shar: $(PROGDIST)
		shar -cstem $(PROGDIST) > $<

$(DATA).shar:	$(DATADIST)
		shar -cstem $(DATADIST) > $<

$(PROGRAM)-tools.shar: $(TOOLSDIST)
		shar -cstem $(TOOLSDIST) > $<


TARFILES      =	$(PROGRAM).tar.Z \
		$(DATA).tar \
		$(PROGRAM)-tools.tar.Z \
		$(PROGRAM).rawtext.Z

tar:		$(TARFILES)

$(PROGRAM).tar.Z: $(PROGDIST)
		tar cvf $(PROGRAM).tar $(PROGDIST)
		compress $(PROGRAM).tar

$(DATA).tar:	$(DATADIST)
		tar cvf $@ $(DATADIST)

$(PROGRAM)-tools.tar.Z: $(TOOLSDIST)
		tar cvf $(PROGRAM)-tools.tar $(TOOLSDIST)
		compress $(PROGRAM)-tools.tar

# compress raw text data (without deleting uncompressed)
$(PROGRAM).rawtext.Z:	$(PROGRAM).rawtext
		compress <$(PROGRAM).rawtext >$@
		
linustape:	tar
		@echo "Writing Linus tape on device /dev/rct...\c"
		@echo $(TARFILES) | cpio -ocv | tcio -orveVS 8 /dev/rct
		@echo " done."

dattape:	tar
		@echo "Writing tar-format tape on device /dev/dat...\c"
		tar cvf /dev/dat README.ftp $(TARFILES)
		@echo " done."



###############################################################################
# Testing Targets
###############################################################################
stdresults:	test.results
		@echo "Storing Standard test results in test.stdresults...\c"
		@cp test.results test.stdresults
		@touch stdresults
		@echo " done."

test.results:	testall $(PROGRAM)
		@echo "Running Test Suite (results in test.results)...\c"
		@./testall > test.results 2>&1
		@echo " test suite completed"

test:		test.results test.stdresults
		@echo "Comparing results to standard results"
		@if diff -c test.results test.stdresults; \
		then echo "  results OK."; \
		else echo "  results differ.  Possible error."; fi


###############################################################################
# Release/Version Control/Installation Targets
# Except for "install" this will be of no interest to users other than 
# the author. 
###############################################################################
checkin:	$(RCSFILES)
		ci -l $?
		touch checkin

relcheckin:	checkin 
		@if [ "$(RELNAME)" = "" ]; \
		then echo "*** No RELNAME value specified ***"; \
		echo "*** Set RELNAME to release name and export it ***"; \
		exit 1; fi
		@if [ "$(CFLAGS)" != "-O" ]; \
		then echo "*** CFLAGS not set to -O for release ***"; \
		echo "*** Set CFLAGS to -O and export it ***"; \
		exit 1; fi
		rcs -n"$(RELNAME):" -sRel $(RCSFILES)
		echo "$(RELNAME)" > relcheckin

## $(RCSFILES):;	co $@

# The nested makes are here because the timestamps don't get updated
# correctly from the ci -l otherwise.
release:	relcheckin
		make all
		strip $(PROGRAM)
		make stdresults
		make tar
		make relinstall
		touch release

# The "relinstall" target puts all the pieces of a new release into place on 
# my home distribution machine.  Probably nobody else will want this...
relinstall: $(PROGRAM) $(DATA) $(DATA).conc $(TARFILES) \
	    $(MAN) $(README) README.ftp ninstall
		if hp9000s800;then $(INSTALL) $(PROGRAM) $(RELEASEDIR)/$(PROGRAM).hp-s800;fi
		$(INSTALL) $(DATA) $(RELEASEDIR)
		$(INSTALL) $(DATA).conc $(RELEASEDIR)
		$(INSTALL) $(TARFILES) $(RELEASEDIR)
		$(INSTALL) $(MAN) $(RELEASEDIR)
		$(INSTALL) $(README) $(RELEASEDIR)/$(README)
		$(INSTALL) README.ftp $(RELEASEDIR)/README.ftp

ninstall:	$(NINSTPKG)
		$(INSTALL) $(NINSTPKG) $(NINSTPKGDIR)
		.do $(NINSTDIR)/restartd

# The "install" target could be of general use.  That's why there are
# no explicit dependencies here.  They just confuse things...
install:
		@if [ ! -r $(DEST) ]; then mkdir $(DEST); fi
		@echo Installing $(PROGRAM) in $(DESTBIN)
		@-strip $(PROGRAM)
		@if [ ! -r $(DESTBIN) ]; then mkdir $(DESTBIN); fi
		@if [ $(DESTBIN) != . ]; then \
		((cd $(DESTBIN);rm -f $(PROGRAM)); \
		cp $(PROGRAM) $(DESTBIN)); \
		chown bin $(DESTBIN)/$(PROGRAM); \
		chgrp bin $(DESTBIN)/$(PROGRAM); \
		chmod 755 $(DESTBIN)/$(PROGRAM); fi
		@echo Installing $(DATA) and $(DATA).conc in $(DESTLIB)
		@if [ ! -r $(DESTLIB) ]; then mkdir $(DESTLIB); fi
		@if [ $(DESTLIB) != . ]; then \
		cp $(DATA) $(DATA).conc $(DESTLIB); \
		chown bin $(DESTLIB)/$(DATA) $(DESTLIB)/$(DATA).conc; \
		chgrp bin $(DESTLIB)/$(DATA) $(DESTLIB)/$(DATA).conc; \
		chmod 644 $(DESTLIB)/$(DATA) $(DESTLIB)/$(DATA).conc; fi
		@if [ ! -r $(DESTMAN) ]; then mkdir $(DESTMAN); fi
		@echo Installing $(MAN) in $(DESTMAN1)
		@if [ ! -r $(DESTMAN1) ]; then mkdir $(DESTMAN1); fi
		@if [ $(DESTMAN1) != . ]; then \
		cp $(MAN) $(DESTMAN1)/$(MAN); \
		chown bin $(DESTMAN1)/$(MAN); \
		chgrp bin $(DESTMAN1)/$(MAN); \
		chmod 644 $(DESTMAN1)/$(MAN); fi

uninstall:;	@echo Un-installing $(PROGRAM), $(DATA) and $(MAN)
		rm -f $(DESTBIN)/$(PROGRAM)
		rm -f $(DESTLIB)/$(DATA) $(DESTLIB)/$(DATA).conc
		rm -f $(DESTMAN1)/$(MAN)

# update:		$(DESTBIN)/$(PROGRAM) $(DESTLIB)/$(DATA)



###############################################################################
# Header DEPENDENCIES
###############################################################################
###
$(PROGRAM).o:	brl.h tsl.h util.h version.h
brl.o:		brl.h tsl.h util.h
tsl.o:		tsl.h util.h
buildcmp.o:	tsl.h util.h
makeconcfile.o:	tsl.h util.h
brl-index.o:	brl-startverse.h brl-startchapter.h
