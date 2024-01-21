# *****************************************************************************
#  $Id: Makefile,v 2.36 1994/12/11 18:48:54 gerd Exp gerd $
# =============================================================================
#  
#  This file is part of BibTool.
#  It is distributed under the GNU General Public License.
#  See the file COPYING for details.
#  
#  (c) 1996 Gerd Neugebauer
#  
#  Net: gerd@informatik.uni-koblenz.de
#  
# *****************************************************************************

#  Look also in the file config.h for configuration parameters.
#  Read the file INSTALL for explanation.
# =============================================================================

#  This is the generic makefile for BibTool.
#  Especially it can be used on Unix systems and as a basis for
#  systems not directly mentioned in the distribution.

# =============================================================================
#  Configuration Section
# =============================================================================
# 
# -------------------------------------------------------
#  Destination directory for make install
#  This is usually a directory where public executables
#  are installed.

BINDIR     = /usr/local/bin

# -------------------------------------------------------
#  Destination directory for make install.man
#  This is usually a directory where public man pages
#  are installed.
#  Additionally name the section of the man pages to use.
#  This should be 1 (user commands), l (local), or n (new)
# 
MANDIR     = /usr/local/man
MANSECT    = 1

# -------------------------------------------------------
#  Destination directory for make install.lib
#  This is usually a directory where public shared files
#  are installed plus a final BibTool.
# 

LIBDIR     = /usr/local/lib/BibTool

# -------------------------------------------------------
#  Name the C compiler
# 

CC         = gcc
# CC       = cc

# -------------------------------------------------------
#  C_OPTIONS are the flags for the C compiler.
#  For debugging it should be -g. 
#  For final version you can try to activate 
#  the optimizer with -O or -O2 or whatever
#  BEWARE: Some C compiler produce erroneous code if the
# 	   optimizer is turned on.
# 

C_OPTIONS =
# C_OPTIONS = -O
#  GNU C
# C_OPTIONS = -g -Wall
# C_OPTIONS = -g -Wall -pedantic -Wpointer-arith -Wshadow -Wcast-align

# -------------------------------------------------------
#  EXT is the optional extension of the final executable.
#  Various Operating Systems have their own ideas about
#  that.
# 

EXT        =

# -------------------------------------------------------
#  Extension of object files
# 

OBJ        = .o

# -------------------------------------------------------
#  Name some programs
# 
#  MV is a command to move certain files maybe to other
#  directories or drives. Only plain files are moved.

MV         = mv

#  RM is a command to remove certain files. It should not
#  be confused when trying to remove non-existent files.
#  Only plain files are removed.

RM         = rm -rf

# 
#  MAKEDEPEND is a program to find dependencies for
#  .c and .h files.
#  Not present on MS-DOS, Atari, Amiga, Next
MAKEDEPEND = makedepend
# MAKEDPEND = gcc -MM
# 
#  INSTALL is a program to properly install some files.
#  Maybe cp should also do this job (on 1)

INSTALL    = install
# INSTALL   = cp

# 
#  INSTALL_DIR is a command to create a directory if not
#  already existent. If your install does not support the 
#  -d option you can try to use mkdir instead.

INSTALL_DIR = mkdir
# INSTALL_DIR = install -d

# 
#  LINT is a program to check the C source for problems.
#  
LINT       = lint
# 
#  CXREF is a C cross reference program.
#  
CXREF      = cxref

# -------------------------------------------------------
#  Defines to support (non-ANSI) C compilers.
#  For Solaris 2 you may have to define HAVE_STRING_H
#  For ANSI C compilers they should be empty.
#  Mostly needed for the GNU regex library.
#  Include
#  -DHAVE_STRING_H	if your C compiler has string.h
# 			(e.g. on Solaris 2)
# 			Maybe you have to enlarge the
# 			include search path.
#  -DREGEX_MALLOC	if you have NO alloca() library
# 			function
#  -DHAVE_ALLOCA_H	if you need alloca.h for alloca()
# 

NON_ANSI_DEFS =

# -------------------------------------------------------
#  GNU Regular Expression Library support.
#  First of all the (sub)directory containing the 
#  necessary files (excluding trailing slash).
# 
#  This directory is contained in the BibTool distribution.
#  I have tried newer versions of this library without
#  positive results. (Try it if you don't believe me:-)
# 
REGEX_DIR  = regex-0.12
# 

REGEX_DEF  = -DREGEX -I$(REGEX_DIR)

REGEX      = regex$(OBJ)

# -------------------------------------------------------
#  Default search paths
#  The values are NULL or a string containing a colon
#  separated list of directories.
#  DON'T FORGET THE CURRENT DIRECTORY!
# 
#  The character ENV_SEP separates the directories in
#  environment search paths.
# 

BIBINPUTS_DEFAULT = NULL
BIBTOOL_DEFAULT   = \".:$(LIBDIR)\"
ENV_SEP           = \":\"

# -------------------------------------------------------
#  Declare the file naming conventions.
#  FILE_TYPES contains optionally a macro definition
#  determining the file naming conventions.
#  -DMS-DOS denotes MSDOG-like file names. (Also for Atari)
#  -DAMIGA denotes the Amiga file names.
#  The default (empty) are UN*X-like file names.
# 
#  DIR_SEP is the directory-file separator.

FILE_TYPES        = 
DIR_SEP		  = /

# =============================================================================
#  End of Configuration Section
# =============================================================================

SHELL      = /bin/sh

RSC_DEF	   = -DRSC_BIBINPUTS_DEFAULT=$(BIBINPUTS_DEFAULT)	\
	     -DRSC_BIBTOOL_DEFAULT=$(BIBTOOL_DEFAULT)		\
	     -DENV_SEP=$(ENV_SEP)

DONT_LINK 	= -c
LINK_TO   	= -o
STANDALONE	= -DSTANDALONE

CFLAGS	   = $(C_OPTIONS) $(FILE_TYPES)

CFILES	   = entry.c	\
	     error.c	\
	     expand.c	\
	     key.c	\
	     main.c	\
	     macros.c	\
	     names.c	\
	     parse.c	\
	     print.c	\
	     pxfile.c	\
	     record.c	\
	     rewrite.c	\
	     rsc.c	\
	     s_parse.c	\
	     symbols.c	\
	     stack.c	\
	     sbuffer.c	\
	     tex_aux.c	\
	     tex_read.c	\
	     type.c	\
	     wordlist.c
HFILES	   = bibtool.h	\
	     config.h	\
	     entry.h	\
	     error.h	\
	     expand.h	\
	     key.h	\
	     keynode.h	\
	     macros.h	\
	     names.h	\
	     parse.h	\
	     print.h	\
	     pxfile.h	\
	     record.h	\
	     resource.h	\
	     rewrite.h	\
	     rsc.h	\
	     s_parse.h	\
	     sbuffer.h	\
	     stack.h	\
	     symbols.h	\
	     tex_aux.h	\
	     tex_read.h	\
	     type.h	\
	     version.h	\
	     wordlist.h

OFILES	   = entry$(OBJ)	\
	     error$(OBJ)	\
	     expand$(OBJ)	\
	     key$(OBJ)		\
	     main$(OBJ)		\
	     macros$(OBJ)	\
	     names$(OBJ)	\
	     parse$(OBJ)	\
	     print$(OBJ)	\
	     pxfile$(OBJ)	\
	     record$(OBJ)	\
	     rewrite$(OBJ)	\
	     rsc$(OBJ)		\
	     s_parse$(OBJ)	\
	     symbols$(OBJ)	\
	     stack$(OBJ)	\
	     sbuffer$(OBJ)	\
	     tex_aux$(OBJ)	\
	     tex_read$(OBJ)	\
	     type$(OBJ)		\
	     wordlist$(OBJ)

DOCFILES   = Doc$(DIR_SEP)bibtool.1	\
	     Doc$(DIR_SEP)bibtool.tex	\
	     Doc$(DIR_SEP)bibtool.bib	\
	     Doc$(DIR_SEP)bibtool.bbl	\
	     Doc$(DIR_SEP)bibtool.aux	\
	     Doc$(DIR_SEP)bibtool.ind	\
	     Doc$(DIR_SEP)bibtool.idx	\
	     Doc$(DIR_SEP)bibtool.ist	\
	     Doc$(DIR_SEP)dina4.sty	\
	     Doc$(DIR_SEP)ref_card.tex	\
	     Doc$(DIR_SEP)Makefile

LIBFILES   = Lib$(DIR_SEP)default.rsc	\
	     Lib$(DIR_SEP)check_y.rsc	\
	     Lib$(DIR_SEP)field.rsc	\
	     Lib$(DIR_SEP)improve.rsc	\
	     Lib$(DIR_SEP)iso2tex.rsc	\
	     Lib$(DIR_SEP)month.rsc	\
	     Lib$(DIR_SEP)opt.rsc	\
	     Lib$(DIR_SEP)sort_fld.rsc	\
	     Lib$(DIR_SEP)tex_def.rsc

ETCFILES   = README			\
	     INSTALL			\
	     COPYING			\
	     Changes			\
	     makefile			\
	     makefile.unx		\
	     makefile.dos		\
	     makefile.ata		\
	     makefile.ami		\
	     ToDo			\
	     THANKS			\
	     pxfile.man			\
	     sbuffer.man		\
	     $(MSDOS_targets)

DISTFILES  = $(ETCFILES)		\
	     $(CFILES)			\
	     $(HFILES)			\
	     $(LIBFILES)		\
	     $(DOCFILES)		\
	     $(REGEX_DIR)
# -----------------------------------------------------------------------------
#  The main target
# -----------------------------------------------------------------------------

all: bibtool$(EXT)

bibtool$(EXT): $(OFILES) $(REGEX)
	$(CC) $(CFLAGS) $(LINK_TO) $@ $(OFILES) $(REGEX)

tex_read$(EXT): tex_read.c
	$(CC) $(CFLAGS) $(STANDALONE) tex_read.c $(LINK_TO) tex_read$(EXT)

tex_aux$(OBJ): tex_aux.c
	$(CC) $(CFLAGS) $(REGEX_DEF) $(DONT_LINK) tex_aux.c

rewrite$(OBJ): rewrite.c
	$(CC) $(CFLAGS) $(REGEX_DEF) $(DONT_LINK) rewrite.c

rsc$(OBJ): rsc.c
	$(CC) $(CFLAGS) $(RSC_DEF) $(DONT_LINK) rsc.c

# __________________________________________________________________
#  Include the makefile into the dependencies to force updates
#  when the makefile is modified.

$(OFILES): makefile config.h

# __________________________________________________________________
#  Targets from the GNU Regular Expression Library.

regex$(OBJ): $(REGEX_DIR)$(DIR_SEP)regex.c makefile
	$(CC) $(CFLAGS) -I$(REGEX_DIR) $(NON_ANSI_DEFS) $(REGEX_DIR)$(DIR_SEP)regex.c $(DONT_LINK) -o $@

# -----------------------------------------------------------------------------
#  General development targets
# -----------------------------------------------------------------------------

CLEAN_TARGETS	= *$(OBJ) xref *.bak core #* *~ ; cd Doc; $(MAKE) clean

clean:
	-$(RM) $(CLEAN_TARGETS)

veryclean: clean
	-$(RM) bibtool

doc: Doc$(DIR_SEP)config.tex
	cd Doc; $(MAKE) DIR_SEP=$(DIR_SEP)

test:
	(cd Test; $(MAKE))

depend:
	-$(MAKEDEPEND) -DMAKEDEPEND $(CFILES)

install: install.bin install.lib

install.bin: bibtool$(EXT)
	-$(INSTALL_DIR) $(BINDIR)
	$(INSTALL) bibtool$(EXT) $(BINDIR)

INSTALL_LIB_FILES = Lib$(DIR_SEP)*.*

install.lib: 
	-$(INSTALL_DIR) $(LIBDIR)
	$(INSTALL) $(INSTALL_LIB_FILES) $(LIBDIR)

install.man: 
	-$(INSTALL_DIR) $(MANDIR)$(DIR_SEP)man$(MANSECT)
	$(INSTALL) Doc$(DIR_SEP)bibtool.1 \
		$(MANDIR)$(DIR_SEP)man$(MANSECT)$(DIR_SEP)bibtool.$(MANSECT)

uninstall.bin:
	-$(RM) $(BINDIR)$(DIR_SEP)bibtool$(EXT)

uninstall.lib:
	-$(RM) $(LIBDIR)

uninstall.man:
	-$(RM) $(MANDIR)$(DIR_SEP)man$(MANSECT)$(DIR_SEP)bibtool.$(MANSECT)

Doc$(DIR_SEP)config.tex: Makefile config.h
	@echo "\\def\\LIBDIR{$(LIBDIR)}" >> $@

# =============================================================================
# DO NOT DELETE THIS LINE -- make depend depends on it.

entry.o: bibtool.h config.h /usr/include/stdio.h /usr/include/stdlib.h
entry.o: /usr/include/sys/stdtypes.h /usr/include/stddef.h
entry.o: /usr/include/string.h entry.h symbols.h error.h type.h
error.o: bibtool.h config.h /usr/include/stdio.h /usr/include/stdlib.h
error.o: /usr/include/sys/stdtypes.h /usr/include/stddef.h
error.o: /usr/include/string.h error.h
expand.o: bibtool.h config.h /usr/include/stdio.h /usr/include/stdlib.h
expand.o: /usr/include/sys/stdtypes.h /usr/include/stddef.h
expand.o: /usr/include/string.h error.h expand.h sbuffer.h symbols.h macros.h
expand.o: type.h
key.o: bibtool.h config.h /usr/include/stdio.h /usr/include/stdlib.h
key.o: /usr/include/sys/stdtypes.h /usr/include/stddef.h
key.o: /usr/include/string.h type.h entry.h symbols.h error.h key.h record.h
key.o: wordlist.h keynode.h rsc.h resource.h names.h sbuffer.h tex_read.h
key.o: expand.h
main.o: tex_aux.h record.h wordlist.h bibtool.h config.h /usr/include/stdio.h
main.o: /usr/include/stdlib.h /usr/include/sys/stdtypes.h
main.o: /usr/include/stddef.h /usr/include/string.h error.h key.h parse.h
main.o: pxfile.h print.h rewrite.h rsc.h resource.h entry.h symbols.h
main.o: macros.h type.h s_parse.h sbuffer.h version.h
macros.o: bibtool.h config.h /usr/include/stdio.h /usr/include/stdlib.h
macros.o: /usr/include/sys/stdtypes.h /usr/include/stddef.h
macros.o: /usr/include/string.h symbols.h error.h type.h macros.h sbuffer.h
names.o: /usr/include/stdio.h bibtool.h config.h /usr/include/stdlib.h
names.o: /usr/include/sys/stdtypes.h /usr/include/stddef.h
names.o: /usr/include/string.h names.h error.h symbols.h stack.h sbuffer.h
names.o: s_parse.h type.h
parse.o: bibtool.h config.h /usr/include/stdio.h /usr/include/stdlib.h
parse.o: /usr/include/sys/stdtypes.h /usr/include/stddef.h
parse.o: /usr/include/string.h type.h entry.h symbols.h error.h pxfile.h
parse.o: rsc.h resource.h stack.h sbuffer.h macros.h print.h record.h
parse.o: wordlist.h
print.o: bibtool.h config.h /usr/include/stdio.h /usr/include/stdlib.h
print.o: /usr/include/sys/stdtypes.h /usr/include/stddef.h
print.o: /usr/include/string.h print.h record.h wordlist.h entry.h symbols.h
print.o: rsc.h resource.h macros.h type.h sbuffer.h expand.h error.h
pxfile.o: config.h pxfile.h /usr/include/stdio.h bibtool.h
pxfile.o: /usr/include/stdlib.h /usr/include/sys/stdtypes.h
pxfile.o: /usr/include/stddef.h /usr/include/string.h
record.o: bibtool.h config.h /usr/include/stdio.h /usr/include/stdlib.h
record.o: /usr/include/sys/stdtypes.h /usr/include/stddef.h
record.o: /usr/include/string.h error.h type.h record.h wordlist.h symbols.h
record.o: s_parse.h entry.h
rewrite.o: bibtool.h config.h /usr/include/stdio.h /usr/include/stdlib.h
rewrite.o: /usr/include/sys/stdtypes.h /usr/include/stddef.h
rewrite.o: /usr/include/string.h symbols.h entry.h error.h macros.h type.h
rewrite.o: rsc.h resource.h key.h record.h wordlist.h s_parse.h sbuffer.h
rewrite.o: rewrite.h
rsc.o: bibtool.h config.h /usr/include/stdio.h /usr/include/stdlib.h
rsc.o: /usr/include/sys/stdtypes.h /usr/include/stddef.h
rsc.o: /usr/include/string.h error.h symbols.h macros.h parse.h tex_aux.h
rsc.o: record.h wordlist.h tex_read.h key.h type.h rewrite.h s_parse.h
rsc.o: entry.h rsc.h resource.h
s_parse.o: bibtool.h config.h /usr/include/stdio.h /usr/include/stdlib.h
s_parse.o: /usr/include/sys/stdtypes.h /usr/include/stddef.h
s_parse.o: /usr/include/string.h type.h symbols.h error.h parse.h s_parse.h
symbols.o: bibtool.h config.h /usr/include/stdio.h /usr/include/stdlib.h
symbols.o: /usr/include/sys/stdtypes.h /usr/include/stddef.h
symbols.o: /usr/include/string.h symbols.h error.h
stack.o: bibtool.h config.h /usr/include/stdio.h /usr/include/stdlib.h
stack.o: /usr/include/sys/stdtypes.h /usr/include/stddef.h
stack.o: /usr/include/string.h error.h
sbuffer.o: config.h /usr/include/stddef.h /usr/include/sys/stdtypes.h
sbuffer.o: /usr/include/stdlib.h sbuffer.h
tex_aux.o: bibtool.h config.h /usr/include/stdio.h /usr/include/stdlib.h
tex_aux.o: /usr/include/sys/stdtypes.h /usr/include/stddef.h
tex_aux.o: /usr/include/string.h rsc.h resource.h type.h error.h rewrite.h
tex_aux.o: record.h wordlist.h symbols.h sbuffer.h tex_aux.h
tex_read.o: config.h /usr/include/stdio.h /usr/include/ctype.h
tex_read.o: /usr/include/stdlib.h /usr/include/sys/stdtypes.h
tex_read.o: /usr/include/string.h error.h
type.o: type.h /usr/include/stdio.h
wordlist.o: bibtool.h config.h /usr/include/stdio.h /usr/include/stdlib.h
wordlist.o: /usr/include/sys/stdtypes.h /usr/include/stddef.h
wordlist.o: /usr/include/string.h wordlist.h error.h type.h
