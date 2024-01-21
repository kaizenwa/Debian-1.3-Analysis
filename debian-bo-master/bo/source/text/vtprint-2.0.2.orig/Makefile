##
## Makefile for vtprint v2.x
## -------------------------
##
## Copyright 1994, Garrett D'Amore
##
## List of user adjustable defines:
##
## CC		-- name of ANSI compliant C compiler
## COMPILE_OPTS	-- C compiler options (same as CFLAGS)
## LINK_OPTS	-- linker/loader options (same as LDFLAGS)
## DEVTTY	-- tty device filename (normally /dev/tty)
## LIBFILE	-- full path name of vtprintcap file (compiled in)
## TERMCAP	-- location of termcap (normally /etc/termcap)
## LPR_BIN	-- name of print spooler binary (normally lpr)
## LPR_SECT	-- section of manual LPR_BIN is referenced
## MAN5_SECT	-- section of manual for file formats (normally 5)
## MAN1_SECT	-- section of manual for vtprint man page (normally 1)
## MAN1_DIR	-- system directory to install section 1 man pages in
## MAN5_DIR	-- system directory to install section 5 man pages in
## BIN_DIR	-- system directory to install binaries in
## LIB_DIR	-- system directory to install vtprintcap in
## USR_HOME	-- user's home directory ($HOME by default)
## USR_MAN	-- master directory for man pages in private user install
## USR_MAN1	-- directory for section 1 man pages in private user install
## USR_MAN5	-- directory for section 5 man pages in private user install
## USR_BIN	-- binary directory for private user installation
## USR_LIB	-- lib directory for vtprintcap in private user install
## MAN_PS	-- How to build postscript man pages from 'roff source
## MAN_ASCII	-- How to build ASCII man (cat) pages from 'roff source
##

###
### User defined parameters
###

# Compiler & compile flags

CC			 = gcc
COMPILE_OPTS = -O -g
LINK_OPTS	 =

# Default device name

DEVTTY		= /dev/tty

# Library file (vtprintcap)

LIBFILE		= /usr/local/lib/vtprint/vtprintcap

# Termcap location (man page reference)

TERMCAP		= /etc/termcap

# Print spooler (man page reference)

LPR_BIN		= lpr
LPR_SECT	= 1

# Manual page sections (1 = user commands, 5 = file formats)

MAN5_SECT	= 5
MAN1_SECT	= 1

# System installation directories

MAN1_DIR	= /usr/local/man/man$(MAN1_SECT)
MAN5_DIR	= /usr/local/man/man$(MAN5_SECT)
BIN_DIR		= /usr/local/bin
LIB_DIR		= /usr/local/lib/vtprint


# User directories (for "make userinstall")

USR_HOME	= $(HOME)
USR_MAN		= $(USR_HOME)/man
USR_MAN1	= $(USR_MAN)/man$(MAN1_SECT)
USR_MAN5	= $(USR_MAN)/man$(MAN5_SECT)
USR_BIN		= $(USR_HOME)/bin
USR_LIB		= $(USR_HOME)/lib/vtprint

# Manual building commands

MAN_PS		= groff -man -Tps
MAN_ASCII	= groff -man -Tascii

#################################################
#################################################
##                                             ##
##   Don't change anything below this line!!   ## 
##                                             ##
#################################################
#################################################

# Version information

VERSION_NO = 2.0.2
VER_MO	= October
VER_DY 	= 25
VER_YR	= 1994
VERSION_DT = $(VER_MO)\ $(VER_DY)\,\ $(VER_YR)

# Consolidated defines

DEFINES = -DDEVTTY=\"$(DEVTTY)\" -DVERSION_NO=\"$(VERSION_NO)\"\
 -DVERSION_DT=\"$(VERSION_DT)\" -DLIBFILE=\"$(LIBFILE)\"

# Compiler options & flags 

CFLAGS	=	$(DEFINES) $(COMPILE_OPTS)
LDFLAGS	=	$(LINK_OPTS)

TARGET	=	vtprint
OBJS	= \
	arglist.o \
	escape.o \
	termstate.o

#
# Provide some "rational" targets, and ensure that the default target 
# is the master target.
#

all:	$(TARGET)

test:	all
	cat ./testfile | ./$(TARGET) -L./vtprintcap

install: installmain 
installmain: all
	./install -d -m 755 $(LIB_DIR)
	./install -d -m 755 $(BIN_DIR)
	./install -c -s -m 555 vtprint $(BIN_DIR)
	./install -c -m 644 vtprintcap $(LIB_DIR)
	./install -l  $(BIN_DIR)/vtprint $(BIN_DIR)/vtprtoff
	./install -l  $(BIN_DIR)/vtprint $(BIN_DIR)/vtprton

userinstall: userinstallmain 
userinstallmain: all
	./install -d -m 755 $(USR_LIB)
	./install -d -m 755 $(USR_BIN)
	./install -c -s -m 555 vtprint $(USR_BIN)
	./install -c -m 644 vtprintcap $(USR_LIB)
	./install -l $(USR_BIN)/vtprint $(USR_BIN)/vtprtoff
	./install -l $(USR_BIN)/vtprint $(USR_BIN)/vtprton
	echo Don't forget to include $(USR_BIN) in your PATH
	echo Also, please set the following environment variable:
	echo VTPRINT="-L $(USR_LIB)/vtprintcap"
	echo Use "make userinstall.man" if you wish to install man pages.

man: vtprint.man vtprint.ps vtprintcap.man vtprintcap.ps\
  vtprtoff.man vtprtoff.ps
	echo Are You a god?  Don't forget to make woman also!

woman:
	echo ... just to be politically correct, I suppose??

FAQ: FAQ.ms FAQ.macros
	groff -Tascii -mgs FAQ.ms > FAQ

FAQ.ps: FAQ.ms FAQ.macros
	groff -Tps -mgs FAQ.ms > FAQ.ps

install.man:
	./install -d -m 755 $(MAN1_DIR)
	./install -d -m 755 $(MAN5_DIR)
	./install -c -m 444 vtprint.1 $(MAN1_DIR)/vtprint.$(MAN1_SECT)
	./install -c -m 444 vtprtoff.1 $(MAN1_DIR)/vtprtoff.$(MAN1_SECT)
	./install -c -m 444 vtprintcap.5 $(MAN5_DIR)/vtprintcap.$(MAN5_SECT)
	./install -l $(MAN1_DIR)/vtprtoff.$(MAN1_SECT) $(MAN1_DIR)/vtprton.$(MAN1_SECT)
	chmod 444 $(MAN1_DIR)/vtprton.$(MAN1_SECT)

userinstall.man:
	./install -d -m 755 $(USR_MAN1)
	./install -d -m 755 $(USR_MAN5)
	./install -c -m 444 vtprint.1 $(USR_MAN1)/vtprint.$(MAN1_SECT)
	./install -c -m 444 vtprtoff.1 $(USR_MAN1)/vtprtoff.$(MAN1_SECT)
	./install -c -m 444 vtprintcap.5 $(USR_MAN5)/vtprintcap.$(MAN5_SECT)
	./install -l -m 444 $(USR_MAN1)/vtprtoff.$(MAN1_SECT) $(USR_MAN1)/vtprton.$(MAN1_SECT)
	echo Don't forget to make sure that $(USR_MAN) is in your MANPATH.

clean::
	rm -f *.o core

print::
	for f in *.[hc] *.cc; do expand -4 $$f | enscript -2rG -b$$f; done
	for f in *.ps; do mpage -2 $$f; done

#
# Sed defines (sed scripts)
#

SEDDEVTTY	= `echo $(DEVTTY) | sed -e s/\\\\//\\\\\\\\\\\\//g`
SEDTERMCAP	= `echo $(TERMCAP) | sed -e s/\\\\//\\\\\\\\\\\\//g`
SEDLIBFILE	= `echo $(LIBFILE) | sed -e s/\\\\//\\\\\\\\\\\\//g`
SED 	= sed
SED1	= $(SED) -e s/VERSION_NO/$(VERSION_NO)/g
SED2	= $(SED) -e s/VER_MO/$(VER_MO)/g
SED3	= $(SED) -e s/VER_DY/$(VER_DY)/g
SED4	= $(SED) -e s/VER_YR/$(VER_YR)/g
SED5	= $(SED) -e s/DEVTTY/$(SEDDEVTTY)/g
SED6	= $(SED) -e s/MAN1_SECT/$(MAN1_SECT)/g
SED7	= $(SED) -e s/MAN5_SECT/$(MAN5_SECT)/g
SED8	= $(SED) -e s/LPR_BIN/$(LPR_BIN)/g
SED9	= $(SED) -e s/LPR_SECT/$(LPR_SECT)/g
SED10	= $(SED) -e s/LIBFILE/$(SEDLIBFILE)/g
SED11	= $(SED) -e s/TERMCAP/$(SEDTERMCAP)/g
SED_SCR	= $(SED1)|$(SED2)|$(SED3)|$(SED4)|$(SED5)|$(SED6)|$(SED6)|$(SED7)|$(SED8)|$(SED9)|$(SED10)|$(SED11)

#
# Build man pages
#

vtprint.1: vtprint.sed Makefile
	cat vtprint.sed | $(SED_SCR) > vtprint.1

vtprintcap.5: vtprintcap.sed Makefile
	cat vtprintcap.sed | $(SED_SCR) > vtprintcap.5

vtprtoff.1: vtprtoff.sed Makefile
	cat vtprtoff.sed | $(SED_SCR) > vtprtoff.1

vtprint.man: vtprint.1 Makefile
	$(MAN_ASCII) vtprint.1 > vtprint.man

vtprintcap.man: vtprintcap.5 Makefile
	$(MAN_ASCII) vtprintcap.5 > vtprintcap.man

vtprtoff.man: vtprtoff.1 Makefile
	$(MAN_ASCII) vtprtoff.1 > vtprtoff.man

vtprint.ps: vtprint.1 Makefile
	$(MAN_PS) vtprint.1 > vtprint.ps

vtprintcap.ps: vtprintcap.5 Makefile
	$(MAN_PS) vtprintcap.5 > vtprintcap.ps

vtprtoff.ps: vtprtoff.1 Makefile
	$(MAN_PS) vtprtoff.1 > vtprtoff.ps

#
# Target for building distributions
#

DIST	 = vtprint-$(VERSION_NO)
DISTTAR	 = $(DIST).tar
DISTSRC  = /home/garrett/src/$(DIST)
DISTTEMP = /tmp/$(DIST)
DISTDEST = /home/garrett

dist::
	rm -rf $(DISTDEST)/$(DISTTAR).gz
	rm -rf $(DISTTEMP)
	rm -rf /tmp/$(DISTTAR)*
	mkdir $(DISTTEMP)
	cp $(DISTSRC)/* $(DISTTEMP)
	cd $(DISTTEMP); tabstrip *.c *.h [A-z][A-Z]* template testfile
	rm -f $(DISTTEMP)/vtprint
	chdir /tmp ; tar -cf $(DISTTAR) $(DIST)/* 
	gzip /tmp/$(DISTTAR)
	cp /tmp/$(DISTTAR).gz $(DISTDEST)
	rm -rf /tmp/$(DIST)*

$(TARGET):	Makefile $(OBJS) $(TARGET).o
	$(CC) $(LDFLAGS) -o $(TARGET) $(TARGET).o $(OBJS) $(LIBS)

