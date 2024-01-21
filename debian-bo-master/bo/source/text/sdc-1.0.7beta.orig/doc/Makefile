#			    -*-makefile-*-
# Names

# These definitions are somewhat redundant, cause they are the same as
# in the toplevel makefile. They tell where the doc source got pyhsical
# installed

prefix=/usr/local
BASE = $(prefix)

# Here went the distribution by the toplevel make install.

DD = $(BASE)/lib/sgml

# Here goes the documentation:

WWW = $(BASE)/www/doc/$(EXE)
INFO = $(BASE)/info
MAN = $(BASE)/man/man1

# The program names used within this Makefile

# Don't use the full path on `EXE'
EXE=sdc
MKDIR=mkdir
MKDIROPT=-p
CP=cp

# The first make target should be help. A personal rule of mine.
all:	help

doc:	# Create the documentation
doc:	# you might select a subset for your installation
# .gdate	german doc
# .indate	english intro
# .mandate	manpage
# .infodate	info file

# remove unwanted targets

doc:	.indate .mandate .infodate .alldate

#-------------------------------------------------------------------------
# There should be noting to change below this line.
#-------------------------------------------------------------------------

# This is only a shorthand
DOCD = $(DD)/doc
TYPESET = $(DD)/bin/$(EXE)

.SUFFIXES: .sgml .html .lout .info .man

# Switch to typeset

TSO = -V0 -D$(DD)/doc -i use-inl

COMTXT = common.ent descript.text environ.text files.text options.text

install: # copy the doc-source into the installation directory
install: e.dd+pwd installdirs
	$(CP) `find * ! -path "CVS" ! -name "*~" -print` $(DD)/doc

intro-en.ps::
	cd $(WWW) ; $(TYPESET) $(TSO) -o intro-en.ps $(DOCD)/intro-en.sgml

intro-en2.ps::
	cd $(WWW); $(TYPESET) $(TSO) -O latex \
		-o intro-en2.tex $(DOCD)/intro-en.sgml
	cd $(WWW); latex intro-en2; latex intro-en2
	cd $(WWW); dvips intro-en2
	cd $(WWW); mv intro-en2.ps ps.intro-en2; rm intro-en2*; \
		mv ps.intro-en2 intro-en2.ps

intro-en.html::
	cd $(WWW) ; $(TYPESET) $(TSO) -R HTML2 -o intro-en.html \
		$(DOCD)/intro-en.sgml

targets.html::
	cd $(WWW) ; $(TYPESET) $(TSO) -o targets.html $(DOCD)/targets.sgml

AllDoc.ps::
	cd $(WWW) ; $(TYPESET) $(TSO) -o AllDoc.ps $(DOCD)/AllDoc.sgml

AllDoc.html::
	cd $(WWW) ; $(TYPESET) $(TSO) -o AllDoc.html $(DOCD)/AllDoc.sgml

nottest2.ps::
	cd $(WWW); $(TYPESET) $(TSO) -o nottest2.tex -O latex \
		$(DOCD)/nottest.sgml
	cd $(WWW); ( latex nottest2; dvips nottest2; \
		mv nottest2.ps ps.nottest2; rm nottest2*; \
		mv ps.nottest2 nottest2.ps )

nottest.ps::
	cd $(WWW) ; $(TYPESET) $(TSO) -o nottest.ps $(DOCD)/nottest.sgml; \
		rm nottest-*

.gdate:	# update the german documentation
.gdate: intro.sgml
	cd $(WWW) ; $(TYPESET) $(TSO) -o intro.ps $(DOCD)/intro.sgml
	cd $(WWW) ; $(TYPESET) $(TSO) -o intro.html $(DOCD)/intro.sgml
	touch .gdate

.indate: # update the english intro
.indate: intro-en.sgml $(COMTXT) intro-bib.sgml targets.sgml nottest.sgml
	$(MAKE) -e intro-en.html intro-en.ps intro-en2.ps targets.html \
		nottest.ps nottest2.ps
	touch .indate

.mandate: # update the manpage
.mandate: manpage.sgml $(COMTXT)
	cd $(MAN) ; $(TYPESET) $(TSO) -Oman -o typeset.1 $(DOCD)/manpage.sgml
	cd $(WWW) ; $(TYPESET) $(TSO) -o typesetman.html $(DOCD)/manpage.sgml
	touch .mandate

.infodate: # update the info file
.infodate: intro-en.sgml
	cd $(INFO) ; $(TYPESET) $(TSO) -Oinfo -o $(EXE) $(DOCD)/intro-en.sgml
	touch .infodate

.alldate: # update the english intro
.alldate: intro-en.sgml $(COMTXT) intro-bib.sgml targets.sgml nottest.sgml
	$(MAKE) -e intro-en.html intro-en.ps intro-en2.ps targets.html \
		AllDoc.ps AllDoc.html \
		nottest.ps nottest2.ps
	touch .alldate

installdirs:
	$(MAKE) e.dir DIRECTORY=$(WWW)
	$(MAKE) e.dir DIRECTORY=$(MAN)
	$(MAKE) e.dir DIRECTORY=$(INFO)
	$(MAKE) e.dir DIRECTORY=$(DOCD)

nottest: $(WWW)/nottest.ps

$(WWW)/nottest.ps: $(DOCD)/nottest.sgml installdirs
	cd $(WWW) ; $(TYPESET) $(TSO) -o nottest.ps $(DOCD)/nottest.sgml

clean:	# remove redundant files
	touch core
	rm -f `find . \( $(FINDBAK) -o $(FINDTMP) -o -name core\) -print`

purge:	# same as clean but removes files containing only "deleted"
purge:  clean
	@echo -n deleted	>deleted7
	@echo deleted	>deleted8
	@rm -f `find . \
	       \( \( -size 8c -exec cmp -s {} deleted8 \; \)	\
	       -o \( -size 7c -exec cmp -s {} deleted7 \; \) \)	\
	       -print`
	@echo purged

wipe:	# use: make wipe file=<file> # prepares files for purge
	mv -f $(file) $(file)~
	echo deleted >$(file)

e.dir:
	@if [ ! -d $(DIRECTORY) ];\
	then $(MKDIR) $(MKDIROPT) $(DIRECTORY) ; else true; fi

e.dd+pwd:
	@if [ ! -d $(DD)/`basename \`pwd\`` ]; \
	then $(MKDIR) $(MKDIROPT) $(DD)/`basename \`pwd\`` ; else true; fi

help:	# Help on make targets
	-@egrep "^[A-Za-z\._]+::?.*#" [Mm]akefile

# Local Variables:
# outline-regexp: ".+:[ \t]+#"
# End:
