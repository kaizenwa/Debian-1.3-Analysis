###############################################################################
# TARGETS
###############################################################################
# make install:
#	does a full make and installs the lde binary
#
# make install.terminfo:
#	installs terminfo files in /usr/lib/terminfo - don't do this if you
#       already have a recent version of ncurses installed
#
# make install.man
#	install the man page
###############################################################################

VERSION=2.3.4

.EXPORT_ALL_VARIABLES:

###############################################################################
# Variables for binary installation
###############################################################################
# INSTALL_DIR = directory to install binary in (/sbin)
# OWNER       = user who should own the installed file (root)
# GROUP       = group that should own the installed file (system)
# PROG_NAME   = name of installed binary (lde)
###############################################################################
INSTALL_DIR=/sbin
OWNER=root
GROUP=system
PROG_NAME=lde

###############################################################################
# Variables for man page installation
###############################################################################
# INSTALL_MAN = filename and directory to install man page
#                (/usr/man/man8/lde.8)
# MAN_OWNER   = user who should own the installed man page (root)
# MAN_GROUP   = group that should own the installed man page (man)
###############################################################################
INSTALL_MAN=/usr/man/man8/lde.8
MAN_OWNER=root
MAN_GROUP=man

###############################################################################
# Variables for terminfo
###############################################################################
# INSTALL_TERM = destination directory for terminfo files
###############################################################################
INSTALL_TERM = /usr/lib/terminfo

###############################################################################
# Curses support
###############################################################################
# To remove the curses support, comment out all the lines in this
# section.
# Where is your ncurses stuff?
# Define NC_HEADER if your include file is called <ncurses.h>,
# otherwise undefine it and set INCLUDE_DIR to point to the directory
# which contains curses.h for ncurses.
###############################################################################
LIBCURSES=-lncurses
# LIBCURSES=-ldcurses
CURSES_HEADER=-DNC_HEADER
# CURSES_INCLUDE_DIR=-I/usr/local/include
CURSES_INCLUDE_DIR=-I/usr/include/ncurses
CURSES_SRC=nc_lde.c nc_block.c nc_inode.c nc_dir.c
CURSES_OBJ=nc_lde.o nc_block.o nc_inode.o nc_dir.o
CURSES_FLAGS=-DLDE_CURSES $(CURSES_HEADER) $(CURSES_INCLUDE_DIR)

###############################################################################
# Other defines which might be useful
#
# -DREAD_PART_TABLES -- for ext2fs currently, can read in the full
#                       disk tables if undefined, or just one group at
#                       a time when defined (memory savings should really be on the
#                       order of kb, maybe 10's of kb)
# -DNO_WRITE_INODE   -- turn off inode writes
# -DPARANOID         -- open device read only at all times
# -DNO_KERNEL_BITOPS -- use the C bitop replacement code instead of the ones
#			in <asm/bitops.h>
# -DALPHA_CODE       -- Activate some of the code that is still Alpha
# -DBETA_CODE        -- Activate some of the code that is still Beta
# -DERRORS_SAVED=30  -- define the number of errors which are saved in the
#			error history log (default to 30 if not set below).
#
###############################################################################
# Distribution settings
CFLAGS= -Wall -O2 -DVERSION=\"$(VERSION)\" $(CURSES_FLAGS)
LDFLAGS=-s --static
#LDFLAGS=
# Lots of warnings
#CFLAGS= -Wall -O2 -DVERSION=\"$(VERSION)\" $(CURSES_FLAGS) -Wstrict-prototypes -Wmissing-prototypes
#LDFLAGS=-s
# Debugging settings
#CFLAGS=-g -Wall -O2 -DVERSION=\"$(VERSION)\" -DALPHA_CODE $(CURSES_FLAGS) \
	-DERRORS_SAVED=300 -Wstrict-prototypes -Wmissing-prototypes
#LDFLAGS=-g


###############################################################################
# Define compilers
###############################################################################
CC=gcc
MAKE=make
YACC=bison --yacc
AR=ar

###############################################################################
#                          STOP EDITING HERE
###############################################################################

$(PROG_NAME):	all

all:
	( cd src ; $(MAKE) -f Makefile all )

clean:
	( cd src ; $(MAKE) -f Makefile clean )

clear:
	( cd src ; $(MAKE) -f Makefile clear )
	rm -f lde

dep:	depend

depend:
	( cd src ; $(MAKE) -f Makefile depend )

install:	$(PROG_NAME)
	install -o $(OWNER) -g $(GROUP) -m0500 $(PROG_NAME) $(INSTALL_DIR)/$(PROG_NAME)

install.man:
	install -o $(MAN_OWNER) -g $(MAN_GROUP) -m 666 doc/lde.man $(INSTALL_MAN)

install.terminfo:
	install -d $(INSTALL_TERMINFO)/c
	install -d $(INSTALL_TERMINFO)/l
	install -d $(INSTALL_TERMINFO)/v
	install -d $(INSTALL_TERMINFO)/x
	install terminfo/x/xterm $(INSTALL_TERMINFO)/x
	install terminfo/v/vt100 $(INSTALL_TERMINFO)/v
	install terminfo/c/console $(INSTALL_TERMINFO)/c
	install terminfo/l/linux $(INSTALL_TERMINFO)/l
	install terminfo/l/linux-m $(INSTALL_TERMINFO)/l
	install terminfo/l/linux-nic $(INSTALL_TERMINFO)/l

tar:	clear all clean
	sh tarme $(VERSION)

zip:	clear
	sh zipme $(VERSION)
