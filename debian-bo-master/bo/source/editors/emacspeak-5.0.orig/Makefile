# $Id: Makefile,v 5.0 1996/11/22 18:04:26 raman Exp $
# $Author: raman $ 
# Description:  Makefile for Emacspeak 
# Keywords: Emacspeak, Dectalk, Makefile 
# {{{ LCD Entry: 

# LCD Archive Entry:
# emacspeak| T. V. Raman |raman@crl.dec.com 
# A speech interface to Emacs |
# $date: $ |
#  $Revision: 5.0 $ | 
# Location undetermined
#

# }}}

# {{{ Copyright:  


# Copyright (c) 1994, 1995 by Digital Equipment Corporation.
# All Rights Reserved. 
#
# This file is not part of GNU Emacs, but the same permissions apply.
#
# GNU Emacs is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

# }}}
# {{{ Installation instructions:

                # Set some Make  configuration variables here.
# If you have a Dectalk connected to a serial port, for example, /dev/tty00 
# specify it in the following line:
DECTALK = > /dev/null 
# and the installation will give you spoken feedback.
# Alternately, you can specify this on the make command line as in
#    make "DECTALK =>/dev/tty00"
# Note: (the above is only for spoken feedback during compilation.)
# If you're reading this, then you've already unpacked the tar archive
# and extracted the sources in a directory.
# cd to the  directory where you placed the sources.
# This directory is referred to henceforth as EMACSPEAK_DIR.
# and then type
#    make config
# Now type
#    make emacspeak 
# to compile the files, then (as superuser)
#    make install
# to install them.
#
# By default, files are installed in subdirectories of /usr/local --
# that is, executables in /usr/local/bin, .info files in
# /usr/local/info, and compiled files in /usr/local/share/lib/emacs/site-lisp/emacspeak. 
# If you want them somewhere else, you may add a "prefix=" parameter to all
# make commands.  For example, to place files in subdirectories /usr
# instead of /usr/local, use these commands:
#    make prefix=/usr config
#    make prefix=/usr emacspeak
#    make prefix=/usr install
#
#Note: The intent is to place emacspeak in a subdirectory of site-lisp.
#Newer emacsuns have this under /usr/local/share (or /usr/share)
#older emacsuns used /usr/local/lib/...
# emacspeak uses tclx --extended tcl-- for the synthesizer driver.
# Note:  Extended TCL  --tclx-- is *not* tclsh
# Setting up synthesizer driver:
# Emacspeak comes with two drivers written in TCL:
# 1) dtk-exp for the Dectalk Express
# 2) dtk-mv for the MultiVoice and older Dectalk 3 synthesizers
# emacspeak uses the shell environment variable DTK_PROGRAM to determine 
# which driver to use, and the shell environment variable DTK_PORT
# to determine the port where the Dectalk is connected. 
# Examples: If using csh or tcsh 
#    setenv DTK_PROGRAM "dtk-exp"
# or if using bash
#    export DTK_PROGRAM=dtk-exp
# By default the port is /dev/tty00 on ultrix/osf1, and /dev/ttyS0 on linux.
#
# Finally, make sure that tcl  is present in your search path by typing 
#    which tcl
# Assuming you're using dtk-exp: 
# Check that the dtk-exp can be run by typing
#    tcl <emacspeak-dir>/dtk-exp 
# You should hear the Dectalk speak and get a TCL prompt if everything is okay.
# Next, check that your serial port is working correctly, and that your stty 
# settings are correct. You can do this by executing the following sequence 
# of TCL commands in the TCL session you just started:
#    dectalk_speak {this is a test. }
# should speak the text within the braces.
#    dectalk_stop
# The above command stops speech.
# You should see a TCL prompt when you execute it.
# If things appear to hang when you execute dectalk_stop
# i.e. you don't see a TCL prompt (%) then
# a) The serial cable conecting your speech device is flaky
# b) Your serial port is flaky
# c) The stty settings on the port are incorrect for your system
# In the case of (c) please report the problem
# quit this tcl session by typing ctrl-d
#
# To use emacspeak you can do one of the following:
# Add the line
# (load-file (expand-file-name "<EMACSPEAK_DIR>/emacspeak-setup.el"))
# to the start of your .emacs
# This will start emacspeak every time you use emacs
# or alternatively set the following alias.
# If you use csh or tcsh  
# alias emacspeak "emacs -q -l <EMACSPEAK_DIR>/emacspeak-setup.el -l $HOME/.emacs"
# If you use bash (the default under linux)
# alias emacspeak="emacs -q -l <EMACSPEAK_DIR>/emacspeak-setup.el -l $HOME/.emacs"
# Note: in all of the above you should replace <EMACSPEAK_DIR> with your 
# site-specific value. The distribution also creates a shell executable 
# emacspeak.sh that does the same thing as the alias shown above. 

# }}}
# {{{  Site Configuration

##### Site  Configuration #####
PREFIX = /usr
# where executables go
bindir = ${PREFIX}/bin
# where info files should go
infodir = ${PREFIX}/info
# where the emacspeak library directory should go
#for older emacsuns use /usr/lib
libparentdir = ${PREFIX}/share/emacs/site-lisp
# where .elc files should go
libdir =$(libparentdir)/emacspeak
# what emacs is called on your system
EMACS = emacs
# How to run in batch mode
BATCH = -batch -q
# How to compile 
COMPILE = -f batch-byte-compile
# Change this to be how to convert texinfo files into info files
# examples:
#	${EMACS} -batch -q -f batch-texinfo-format
#	makeinfo
MAKEINFO      = makeinfo
# Assuming you're doing 'make' in the source directory: 
CWD = `pwd`
# How to install files
INSTALL = install

# }}}
# {{{  define make variables and file groupings

############## no user servicable parts beyond this point ###################
SNAPSHOT = `date "+%h-%d-%y-%H-%M"`
#New make rules 
.SUFFIXES: .elc .el .info .texinfo .el .def 

.el.elc:
	$(EMACS) $(BATCH)  -l $(CWD)/emacspeak-load-path $(COMPILE) $(CWD)/$<
	@echo  "Compiled  $<.  " $(DECTALK)

.texinfo.info: 
	$(MAKEINFO) $(CWD)/$<
# source files to distribute
ID = README
TABLE_SAMPLES=tables/*.tab
SOUNDS=sounds/*.au
TCL_PROGRAMS = dtk-exp dtk-mv remote-tcl
ELISP = dtk-css-speech.el \
dtk-macros.el \
dtk-speak.el \
dtk-tcl.el \
dtk-voices.el \
emacspeak-actions.el \
emacspeak-advice.el \
emacspeak-auctex.el \
emacspeak-bbdb.el \
emacspeak-bibtex.el \
emacspeak-buff-sel.el \
emacspeak-c.el \
emacspeak-calc.el \
emacspeak-calendar.el \
emacspeak-compile.el \
emacspeak-dired.el \
emacspeak-dismal.el \
emacspeak-dmacro.el \
emacspeak-ediff.el \
emacspeak-eterm.el \
emacspeak-fix-interactive.el \
emacspeak-folding.el \
emacspeak-forms.el \
emacspeak-gnus.el \
emacspeak-gud.el \
emacspeak-hyperbole.el \
emacspeak-info.el \
emacspeak-ispell.el \
emacspeak-keymap.el \
emacspeak-kotl.el \
emacspeak-load-path.el \
emacspeak-man.el \
emacspeak-message.el \
emacspeak-new-ediff.el \
emacspeak-oo-browser.el \
emacspeak-outline.el \
emacspeak-perl.el \
emacspeak-pronounce.el \
emacspeak-redefine.el \
emacspeak-replace.el \
emacspeak-rmail.el \
emacspeak-setup.el \
emacspeak-sounds.el \
emacspeak-speak.el \
emacspeak-table-ui.el \
emacspeak-table.el \
emacspeak-tabulate.el \
emacspeak-tcl.el \
emacspeak-tempo.el \
emacspeak-view-process.el \
emacspeak-view.el \
emacspeak-vm.el \
emacspeak-w3.el \
emacspeak-widget.el \
emacspeak-wrolo.el \
emacspeak.el \
html-voice.el \
voice-lock.el \
w3-util.el 

TEMPLATES = emacspeak-load-path.def \
emacspeak-setup.def \
emacspeak.def \
play.def


DISTFILES =${ELISP}  ${TEMPLATES}     $(TCL_PROGRAMS) \
	     Makefile COPYRIGHT HELP  emacspeak.info emacspeak.texinfo 
#The various modules:
DTK_OBJECTS =  dtk-macros.elc\
	dtk-speak.elc\
	dtk-tcl.elc\
        dtk-css-speech.elc \
	dtk-voices.elc
SPEAK_OBJECTS =   \
	emacspeak-speak.elc  \
        emacspeak-actions.elc \
	voice-lock.elc  \
        html-voice.elc \
        emacspeak-pronounce.elc \
	emacspeak-sounds.elc 

CORE_OBJECTS =  \
	emacspeak.elc \
	emacspeak-advice.elc\
	emacspeak-keymap.elc\
	emacspeak-fix-interactive.elc\
	emacspeak-redefine.elc\
	emacspeak-replace.elc \
        emacspeak-table.elc \
        emacspeak-table-ui.elc \
	emacspeak-tabulate.elc 

AUX_OBJECTS = emacspeak-calendar.elc \
emacspeak-calc.elc \
emacspeak-c.elc \
emacspeak-compile.elc \
emacspeak-auctex.elc \
emacspeak-bibtex.elc \
emacspeak-dired.elc \
emacspeak-info.elc\
emacspeak-folding.elc \
emacspeak-outline.elc \
emacspeak-hyperbole.elc \
emacspeak-kotl.elc \
emacspeak-bbdb.elc \
emacspeak-dmacro.elc \
emacspeak-wrolo.elc \
emacspeak-oo-browser.elc \
emacspeak-tempo.elc \
emacspeak-tcl.elc \
emacspeak-perl.elc \
emacspeak-gud.elc \
emacspeak-man.elc \
emacspeak-forms.elc \
emacspeak-view.elc \
emacspeak-message.elc \
emacspeak-dismal.elc \
emacspeak-buff-sel.elc \
emacspeak-view-process.elc \
emacspeak-vm.elc \
emacspeak-rmail.elc \
emacspeak-widget.elc \
emacspeak-w3.elc \
emacspeak-ispell.elc \
emacspeak-eterm.elc \
emacspeak-ediff.elc \
emacspeak-new-ediff.elc \
emacspeak-gnus.elc\
emacspeak-setup.el

ADDON_OBJECTS=w3-util.elc # custom.elc

OBJECTS = $(DTK_OBJECTS) $(SPEAK_OBJECTS) $(CORE_OBJECTS)  \
	$(AUX_OBJECTS) $(ADDON_OBJECTS)

# }}}

emacspeak: $(OBJECTS)
	@echo "Compiled on $(SNAPSHOT) by `whoami` on `hostname`" >> $(ID)
	@echo "Now check installation of  the synthesizer driver. "  $(DECTALK)
	@echo "See Makefile for instructions." $(DECTALK)
	@echo "Warning!!! The accompanying emacspeak.info is out of date." $(DECTALK)
	@echo "Type make emacspeak.info for  the online  manual." $(DECTALK)
	@echo "Type make print for a printed copy. " $(DECTALK)
	@echo "Now check installation of  the synthesizer driver."  
	@echo "See Makefile for instructions." 
	@echo "Type make emacspeak.info for  the online  manual."
	@echo "Type make print for a printed copy. "

print: emacspeak.texinfo
	make emacspeak.dvi
	dvips emacspeak.dvi

tar:
	rm -f $(ID)
	@echo "This is Emacspeak from  $(SNAPSHOT)" > $(ID)
	@echo "Distribution created by `whoami` on `hostname`" >> $(ID)
	@echo "Place  the files from this distribution in a directory" >> $(ID)
	@echo "And type make config " >> $(ID)
	@echo "To configure the source files. Then type make" >> $(ID)
	@echo "See the Makefile for details. " >> $(ID)
	@echo "If you have a Dectalk connected to a serial port," >>$(ID)
	@echo 'e.g. /dev/tty00 then use make "DECTALK=>/dev/tty00"' >> $(ID)
	@echo "And  get spoken feedback during make. " >> $(ID)
	tar cvf  ems.tar $(DISTFILES)   $(ID) \
         ${TABLE_SAMPLES} ${SOUNDS} DOC
	gzip -9 ems.tar
	mv ems.tar.gz ems.tgz

snapshot: $(DISTFILES)
	rcs -n$(SNAPSHOT):$$ $(DISTFILES)

dist: $(DISTFILES)
	make tar
	make snapshot 
	@echo "emacspeak $(SNAPSHOT)  prepared for distribution in ems.tgz"

config:
	@rm -f emacspeak-setup.el emacspeak-load-path.el  play emacspeak.sh
	@sed "s@<emacspeak-dir>@$(CWD)@g" \
		emacspeak.def > emacspeak.sh
	@sed "s@<emacspeak-dir>@$(CWD)@g" \
		emacspeak-load-path.def > emacspeak-load-path.el
	@sed "s@<emacspeak-dir>@$(CWD)@g" \
		emacspeak-setup.def  > emacspeak-setup.el
	@sed "s@<emacspeak-dir>@$(CWD)@g" \
		play.def > play
	@chmod 444 emacspeak-setup.el emacspeak-load-path.el
	@chmod 755 play  emacspeak.sh
	@echo "Configured emacspeak in directory $(CWD)." $(DECTALK)
	 @echo "Now type make emacspeak" $(DECTALK)
	@echo "Configured emacspeak in directory $(CWD). Now type make emacspeak"

install:
	if test ! -d $(infodir) ; then mkdir $(infodir); fi
	$(INSTALL) -m 644 emacspeak.info $(infodir) 
	if test ! -f $(infodir)/dir ; then \
	  echo '  --- PLEASE ADD DOCUMENTATION TO THIS TREE. (See INFO topic first.) ---' >$(infodir)/dir ; \
	  echo '* Menu:' >>$(infodir)/dir ; \
	fi 
	if fgrep emacspeak $(infodir)/dir 1> /dev/null 2> /dev/null; then \
	  echo >/dev/null; \
	else \
	  echo '* Emacspeak:(emacspeak).  A speech interface to Emacs' >>$(infodir)/dir ;\
	fi
	make config CWD=$(libdir)
	if test ! $(libdir) = $(CWD) ; then \
	  if test ! -d $(libparentdir) ; then mkdir $(libparentdir); fi ; \
	  if test ! -d $(libdir) ; then mkdir $(libdir); fi ; \
	  $(INSTALL) -m 0644  $(OBJECTS) dtk-exp dtk-mv remote-tcl  $(libdir); \
	  $(INSTALL) -d $(libdir)/sounds; \
	  $(INSTALL)  -m 0644 $(SOUNDS) $(libdir)/sounds; \
	fi
	if test ! -d $(bindir) ; then mkdir $(bindir); fi
	$(INSTALL) emacspeak.sh $(bindir)/emacspeak

uninstall:
	rm -rf $(infodir)/emacspeak.info $(bindir)/emacspeak;
	if test ! $(libdir) = $(CWD) ; then \
	  (cd $(libparentdir); rm -rf $(libdir) ); \
	fi

# Do this in this brain-dead way because different makes do pattern
# rules differently. grumble grumble...
#dtk module:
#dtk-tcl contains inline functions.
#Compile dtk-tcl first, and make dtk-speak depend on it
#Modules using functions from the dtk module should depend on dtk-speak
dtk-macros.elc: dtk-macros.el
dtk-speak.elc: dtk-speak.el dtk-tcl.el
dtk-tcl.elc: dtk-tcl.el dtk-voices.elc emacspeak-pronounce.elc
dtk-voices.elc: dtk-voices.el
dtk-css-speech.elc: dtk-css-speech.el dtk-voices.elc
voice-lock.elc: voice-lock.el
html-voice.elc: html-voice.el voice-lock.elc 
#Emacspeak module:
emacspeak-sounds.elc: emacspeak-sounds.el
emacspeak-pronounce.elc: emacspeak-pronounce.el
emacspeak-advice.elc: emacspeak-advice.el dtk-speak.elc
emacspeak-calendar.elc: emacspeak-calendar.el dtk-speak.el
emacspeak-calc.elc: emacspeak-calc.el 
emacspeak-c.elc: emacspeak-c.el dtk-speak.elc
emacspeak-dired.elc: emacspeak-dired.el dtk-speak.elc
emacspeak-auctex.elc: emacspeak-auctex.el dtk-speak.elc
emacspeak-bibtex.elc: emacspeak-bibtex.el dtk-speak.elc
emacspeak-info.elc: emacspeak-info.el dtk-speak.elc 
emacspeak-folding.elc: emacspeak-folding.el dtk-speak.elc
emacspeak-outline.elc: emacspeak-outline.el dtk-speak.elc
emacspeak-hyperbole.elc: emacspeak-hyperbole.el dtk-speak.elc
emacspeak-kotl.elc: emacspeak-kotl.el dtk-speak.elc
emacspeak-bbdb.elc: emacspeak-bbdb.el dtk-speak.elc
emacspeak-dmacro.elc: emacspeak-dmacro.el dtk-speak.elc
emacspeak-wrolo.elc: emacspeak-wrolo.el dtk-speak.elc 
emacspeak-oo-browser.elc: emacspeak-oo-browser.el dtk-speak.elc 
emacspeak-tempo.elc: emacspeak-tempo.el dtk-speak.elc
emacspeak-tcl.elc: emacspeak-tcl.el dtk-speak.elc
emacspeak-perl.elc: emacspeak-perl.el dtk-speak.elc
emacspeak-gud.elc: emacspeak-gud.el dtk-speak.elc
emacspeak-man.elc: emacspeak-man.el dtk-speak.elc
emacspeak-forms.elc: emacspeak-forms.el dtk-speak.elc
emacspeak-view.elc: emacspeak-view.el dtk-speak.elc
emacspeak-message.elc: emacspeak-message.el dtk-speak.elc
emacspeak-dismal.elc: emacspeak-dismal.el dtk-speak.elc
emacspeak-buff-sel.elc: emacspeak-buff-sel.el dtk-speak.elc
emacspeak-view-process.elc: emacspeak-view-process.el dtk-speak.elc
emacspeak-eterm.elc: emacspeak-eterm.el dtk-speak.elc
emacspeak-ediff.elc: emacspeak-ediff.el dtk-speak.elc
emacspeak-new-ediff.elc: emacspeak-new-ediff.el dtk-speak.elc
emacspeak-fix-interactive.elc: emacspeak-fix-interactive.el dtk-speak.elc
emacspeak-gnus.elc: emacspeak-gnus.el dtk-speak.elc
emacspeak-ispell.elc: emacspeak-ispell.el dtk-speak.elc
emacspeak-keymap.elc: emacspeak-keymap.el 
emacspeak-load-path.elc: emacspeak-load-path.elc
emacspeak-redefine.elc: emacspeak-redefine.el  dtk-speak.elc
emacspeak-replace.elc: emacspeak-replace.el dtk-speak.elc
emacspeak-table.elc: emacspeak-table.el
emacspeak-table-ui.elc: emacspeak-table-ui.el emacspeak-table.elc
emacspeak-tabulate.elc: emacspeak-tabulate.el dtk-speak.elc
emacspeak-setup.elc: emacspeak-setup.el
emacspeak-speak.elc: emacspeak-speak.el dtk-speak.elc
emacspeak-actions.elc: emacspeak-actions.el 
emacspeak-vm.elc: emacspeak-vm.el dtk-speak.elc
emacspeak-rmail.elc: emacspeak-rmail.el dtk-speak.elc
emacspeak-widget.elc: emacspeak-widget.el dtk-speak.elc
emacspeak-w3.elc: emacspeak-w3.el dtk-speak.elc
emacspeak.elc: emacspeak.el
#targets
#the complete build 
all: emacspeak 
#Compiling support for different packages:
#this is done as part of make all.
#You can recompile support for individual packages, e.g. calendar by typing
# make calendar
#
actions: emacspeak-actions.elc 
calendar: emacspeak-calendar.elc
calc: emacspeak-calc.elc 
c: emacspeak-c.elc
compile: emacspeak-compile.elc
auctex: emacspeak-auctex.elc
bibtex: emacspeak-bibtex.elc
dired: emacspeak-dired.elc  
folding: emacspeak-folding.elc
outline:emacspeak-outline.elc
hyperbole: emacspeak-hyperbole.elc
kotl: emacspeak-kotl.elc
bbdb: emacspeak-bbdb.elc
dmacro: emacspeak-dmacro.elc 
wrolo: emacspeak-wrolo.elc
oo-browser: emacspeak-oo-browser.elc
tempo: emacspeak-tempo.elc
tcl: emacspeak-tcl.elc
perl: emacspeak-perl.elc
gud: emacspeak-gud.elc 
man: emacspeak-man.elc
forms: emacspeak-forms.elc
view: emacspeak-view.elc
message: emacspeak-message.elc
dismal: emacspeak-dismal.elc
buff-sel: emacspeak-buff-sel.elc
view-process: emacspeak-view-process.elc
vm: emacspeak-vm.elc
rmail: emacspeak-rmail.elc
widget: emacspeak-widget.elc
w3: emacspeak-w3.elc  
ispell: emacspeak-ispell.elc  
eterm: emacspeak-eterm.elc
ediff: emacspeak-ediff.elc
new-ediff: emacspeak-new-ediff.elc
gnus: emacspeak-gnus.elc  
emacspeak.dvi:		emacspeak.texinfo
	tex emacspeak.texinfo
	texindex emacspeak.cp  emacspeak.fn  emacspeak.ky  emacspeak.pg  emacspeak.tp  emacspeak.vr
	tex emacspeak.texinfo
	rm -f 	emacspeak.cp  emacspeak.fn  emacspeak.ky  emacspeak.pg  emacspeak.tp  emacspeak.vr 	\
		emacspeak.cps emacspeak.fns emacspeak.kys emacspeak.pgs emacspeak.tps emacspeak.vrs	\
		emacspeak.log emacspeak.toc emacspeak.aux

clean:
	rm -f *.elc  	emacspeak.cp  emacspeak.fn  emacspeak.ky  emacspeak.pg  emacspeak.tp  emacspeak.vr 	\
		emacspeak.cps emacspeak.fns emacspeak.kys emacspeak.pgs emacspeak.tps emacspeak.vrs	\
		emacspeak.log emacspeak.toc emacspeak.aux

#label  releases when ready
LABEL=
label: $(DISTFILES)
	co -l $(DISTFILES)
	   ci -f -u${LABEL} -m"Releasing ${LABEL}" \
        -n$(SNAPSHOT) $(DISTFILES)
