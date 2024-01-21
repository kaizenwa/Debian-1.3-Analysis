#			    -*-makefile-*-
## Custumizable names:
# irz301:/ftpserv/ftp/pub/people/jerry
# First an excusion:
#  I din't dare to change all the internal occurences of the
#  program name. I still hope to find a better one...
#  where ever you find `typeset': don't wonder it's just the old name,
#  which was droped because some shells have it as an internal command.
#
## Where to install:

# BASE is just to make the usual case easier

prefix = /usr/local

## EXE is the name of the executable. As long as I don't find a good
# name ist's `sdc'.

EXE=sdc

## SR is the installation directory. Down from here the directory
# structure in used by typeset itself.
# This is the "logical" place of typeset, where users will find it.

SR = $(prefix)/lib/$(EXE)

## DD is the "destination directory" except for the documentation which
# is installed separatly no file is created outside. This is the pysical
# place of typeset.
# Usually this is the same as $(SR) 

DD = $(SR)

## internal adjustments

# Sgmls happens to forget the -i switches for subdoc's. If you use sgmls
# you need to undergo a HACK (which implies to haver write permissions
# into the first directory of your DOCPATH). Set the following to sgmls
# or nsgmls. (The latter will remove the hack at compile time).
#
# You can put the full path name here to save a little on execution time.
#
# Make absolutly sure that -- if you are using sgmls -- this points
# to a version 1.1.91 NOT 1.1 of sgmls!

PARSEPROG=sgmls

# The following decides whether or not some graphic converters are
# installed at the local host or called from a remote one. The latter
# is a rather complicated procedure, but localy nessesary at the host
# of developing. Pretty sure this should be "local" instead of
# "remote"

REMLOC=local

# GRPHPKG determines the graphic package to use for LaTeX
# either "grphcs" or "epsfig"
# Don't attempt to use epsfig in conjunction with LaTeX2e!

GRPHPKG=grphcs

# For custumization of installed documentation go to the Makefile of
# the doc directory!

## The installation process

# Make sure you have an echo which understands the -n option (Solaris is
# known to have a broken one, use /usr/ucb/echo on this system)

ECHO=echo

# Need something to make dirs and the path up to them. Either mkdir -p or
# install -d. For those who have an install which doesn't understand -d
# (e.g., Ultrix) there is one in the install directory.

#MKDIR=install
#MKDIR=mkdir
MKDIR=$(HOME)/build/$(EXE)/install/install
MKDIROPT=-d

# Need also a cp. Either cp or install.
#CP=cp
CP=$(HOME)/build/$(EXE)/install/install

MAKE=/usr/bin/make

# WWW is where the local home Web accessiable things go

WWW=$(prefix)/www/doc/$(EXE)

#--------------------------------------------------------------------------
# There should be nothing to be changed below this line.
# -------------------------------------------------------------------------

# Private archive. Everything including RCS files
TARFILE = $(EXE).tz
# Release archive.
RTARFILE = $(EXE).tar.gz
# Mailable distribution
DISTFILE = $(EXE).uue

#SUBDIRS=`find ./* -maxdepth 0 -type d -print`
SUBDIRS=bin sgml include rc target doc

SETTINGS=prefix=$(prefix) SR=$(SR) DD=$(DD) EXE=$(EXE) \
	  PARSEPROG=$(PARSEPROG) \
	  MAKE=$(MAKE) CP=$(CP) \
	  MKDIR=$(MKDIR) MKDIROPT=$(MKDIROPT) ECHO=$(ECHO)

.SUFFIXES: .sgml .html .lout

help:

prepare:	all

all: # prepare in place for install
all:
	$(ECHO) -n $(SR) > sgml/instdir
	cd sgml; $(MAKE) SETTING=$(SETTINGS) prepare
	V=`basename \`pwd\`|sed "s/^[a-z]*-//"`; \
	cd bin; $(MAKE) VERSION=$$V SETTINGS=$(SETTINGS) $(EXE)
	cd bin; $(MAKE) REMLOC=$(REMLOC) GRPHPKG=$(GRPHPKG) \
		 SETTINGS=$(SETTINGS) converters
	rm sgml/instdir

install:: # public installation, do after make prepare
install::
	if [ ! -d $(DD) ]; then $(MKDIR) $(MKDIROPT) $(DD); else true; fi
	chmod -R +w $(DD)
	for i in $(SUBDIRS); do cd $$i; \
		$(MAKE) install $(SETTINGS); cd ..; done;
	$(CP) Makefile *.data $(DD)
	cd $(DD); $(ECHO) -n $(SR) > sgml/instdir

instbbset: # install bbset
instbbset:
	if [ ! -d $(prefix)/bin ];\
	then $(MKDIR) $(MKDIROPT) $(prefix)/bin; else true;\
	fi
	cd $(prefix)/bin; $(SR)/bin/$(EXE) -V0 -O literate \
	$(SR)/bin/bbset.sgml
	chmod +x $(prefix)/bin/bbset
	if [ ! -d $(prefix)/man/man1 ]; \
		then $(MKDIR) $(MKDIROPT) $(prefix)/man/man1; else true; fi
	cd $(prefix)/man/man1; $(SR)/bin/$(EXE) -V0 -O man -o bbset.1\
		$(DD)/bin/bbset.sgml

uninstall: # remove installed files (except doc)
uninstall: #    don't forget to remove the local choosen link (which typeset)
uninstall:
	rm -rf $(DD)/*

installdoc:	# Install the documentation (only valid AFTER make install)
installdoc:	# custumize the Makefile of the doc directory before!
installdoc:	
	cd $(DD)/doc ; $(MAKE) $(SETTINGS) doc

# Some development aid

homepg:	# prepare the home page
	sed -e s%@@VERSION@@%`pwd |sed -e "s/^.*-//" `%g homepage.sgml \
	>homepage.tmp
	$(EXE) -V0 -o $(WWW)/index.html -O html homepage.tmp
	rm homepage.tmp

INSTALL: # prepare the INSTALL file (not installation)
INSTALL: doc/INSTALL.sgml
	$(EXE) -L . -V0 -D doc -Oascii -Rno-margin\
	 -o INSTALL doc/INSTALL.sgml

README: # prepare the README file
README: doc/README.sgml
	$(EXE) -L . -V0 -D doc -Oascii -Rno-margin\
	 -o README doc/README.sgml

# archive and something like that

FINDFILES = -name "*" ! -type d ! -name "*~" ! -name "*.bak" \
	! -path "*/CVS/*" \
	! -name core ! -name "*.o" ! -name "$(EXE)" ! -path "./install/*" \
	! -name $(TARFILE)

list:
	find . $(FINDFILES) -print > FILES

filelist: force
	@find $(FINDFILES) -print

listafter: force
	@if [ -f $(AFTER) ]; then \
	find -newer $(AFTER) $(FINDFILES) -print; fi

$(TARFILE): FILES
	tar -czv --files-from FILES -f $(TARFILE)

tar:	# archivieren
	rm -f FILES;make $(TARFILE)

xtar:	# Archiv auspacken
	chmod -R +w *
	tar xzvf $(TARFILE)

# tricky: assemble the release.
$(DISTFILE): RELEASEFILES INSTALL README
	cp RELEASEFILES rRELEASEFILES
	$(ECHO) ./AllDoc.ps >>rRELEASEFILES
	cp $(WWW)/AllDoc.ps .
	sed "s/^\./$(DIREC)/" rRELEASEFILES >xRELEASEFILES
	cd ..; tar -czvf $(DIREC)/$(RTARFILE) \
	--files-from $(DIREC)/xRELEASEFILES
	rm rRELEASEFILES xRELEASEFILES AllDoc.ps
	uuencode $(RTARFILE) $(RTARFILE) > $(WWW)/$(DISTFILE) 
	mv $(RTARFILE) $(WWW)

dist:	# create the distribution
dist:
	$(MAKE) $(DISTFILE) DIREC=`basename \`pwd\`` prefix=$(prefix)

clean:	# remove redundant files
	@touch core
	rm -f `find . \( -name "*~" -o -name *.o -o -name core \) -print`

clobber: # remove all generated files (did I catch all?)
clobber: clean
	for i in $(SUBDIRS); do cd $$i; make clobber; cd ..; done;

purge:	# same as clean & removes files containing only "deleted"
purge:  clean
	@$(ECHO) -n deleted	>deleted7
	@$(ECHO) deleted	>deleted8
	@rm -f `find . \
	       \( \( -size 8c -exec cmp -s {} deleted8 \; \)	\
	       -o \( -size 7c -exec cmp -s {} deleted7 \; \) \)	\
	       -print`
	@echo purged

wipe:	# use: make wipe file=<file> # prepares file for purge
	mv -f $(file) $(file)~
	echo deleted >$(file)

e.dd+pwd:
	if [ ! -d $(DD) ]; \
	then $(MKDIR) $(MKDIROPT) $(DD)/`basename \`pwd\`` ; else true; fi

force:

help:	# Help on make targets
	@echo Recommented: either edit the settings of the Makefile variables
	@echo prefix SR DD
	@echo as explaint in the comments of the Makefile or
	@echo just call the targets like
	@echo make prefix=\<basedir\> \<target\>
	@echo ------------------------------
	-@egrep "^[A-Za-z\._]+::?.*#" [Mm]akefile

# Local Variables:
# outline-regexp: ".+:[ \t]+#"
# End:
