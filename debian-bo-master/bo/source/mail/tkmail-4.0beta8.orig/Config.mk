# User configuration for for TkMail 4.0
VERSION = 4.0beta8

# This file is included by mfv/Makefile and scripts/Makefile
# Look over all the settings below and edit the ones necessary to
# conform to your site. Eventually, some of this will move into
# the regular Makefiles when I figure out more about autoconfig.

#----------------------------------------------------------------------
# Basic compiler and installation 
#----------------------------------------------------------------------

# The following definition can be set to non-null for special systems
# like AFS with replication.  It allows the pathnames used for installation
# to be different than those used for actually reference files at
# run-time.  INSTALL_ROOT is prepended to $prefix and $exec_prefix
# when installing files.
INSTALL_ROOT =

# Directory in which to install the programs tkmail, mfv_wish and dotlock:
BIN_INSTALL_DIR =       $(INSTALL_ROOT)$(exec_prefix)/bin

# Directory from which the programs mfv_wish and dotlock should be referenced:
BIN_DIR =               $(exec_prefix)/bin

# Top-level directory for manual entries:
MAN_INSTALL_DIR =       $(INSTALL_ROOT)$(prefix)/man

# Directory in which to install manual entry for tkmail, mfv_wish and dotlock:
MAN1_INSTALL_DIR =      $(MAN_INSTALL_DIR)/man1

# To change the compiler switches, for example to change from -O
# to -g, change the following line:
CFLAGS = -g

# If you want to change the C compiler, do it here
CC = cc

#----------------------------------------------------------------------
# Mail spool directory: set to the full path to directory in which
# incoming mail is written
#----------------------------------------------------------------------

SPOOL = -DMAILSPOOLDIR=\"/usr/mail\"
#SPOOL = -DMAILSPOOLDIR=\"/usr/spool/mail\"

#----------------------------------------------------------------------
# Standard Tcl7.4/Tk4.0 or greater include files and library locations
#----------------------------------------------------------------------

TK_INC  = -I/usr/local/include
TCL_INC =
TK_LIB  = -L/usr/local/lib -ltk4.1 -ljpeg
TCL_LIB = -ltcl7.5 -lm

#----------------------------------------------------------------------
# Extended TCL
#----------------------------------------------------------------------

# If you have tclX installed, uncomment the following.
# You may have to edit the value of TCLX_LIB, TKX_LIB, and TCLX_INC
# for the location of include files and libraries.
#
# TCLX_OBJS = 
# TCLX_SRCS = 
# TCLX_INC =
# TCLX_LIB = -ltclx7.5
# TKX_LIB = -ltkx7.5
# HAVE_TCLX =-DHAVE_TCLX


# If you do not have tclX installed, uncomment the following.
#
TCLX_OBJS = tclXsignal.o tclXutil.o
TCLX_SRCS = tclXsignal.c tclXutil.c
TCLX_INC = 
TCLX_LIB = 
TKX_LIB = 
HAVE_TCLX =

#----------------------------------------------------------------------
# Script Install Defines
#----------------------------------------------------------------------

# Directory to put TkMail support scripts in
TKMAIL_DIR = $(INSTALL_ROOT)$(prefix)/lib/tkmail4

# Directory to put tkBindExtended support scripts in
TKBIND_DIR = $(TKMAIL_DIR)/tkbind

# Name of the tkmail executable script
TKMAIL = tkmail4

#----------------------------------------------------------------------
# Support programs and files
#----------------------------------------------------------------------

# sendmail style mail delivery agent -- check your sendmail man page
#    to verify that the options below are right for your site
# the -bm options makes it deliver mail in the normal way
# the -t options makes it parse the message for delivery info
# the -oi makes it not treat single periods on a line as end of mesg
# the -odb makes it deliver in the background
DELIVER = /usr/lib/sendmail -bm -odb -oi -t
 
# whether you have ispell (not GNU ispell!). Set to 1 if you do, 0 if not.
# Read the top of ispell.tk file for more info. If you set this to 0,
# there is no need to change the two below.
HAVEISPELL = 1

# Location of ispell hash files (usually named american.hash, english.hash, ...)
ISPELLLIB = /usr/local/lib

# name of ispell program (may need full path if not in user PATH)
ISPELLPROG = ispell

# ascii text print command for your site (%F is place holder for filename)
PRINTCMD = lpr %F

# directory to use for temporary files
TMPDIR = /usr/tmp

# bitmap directory containing flagdown, flagup, and letters bitmaps
BITMAPDIR = $(X11_INCLUDES)/bitmaps
