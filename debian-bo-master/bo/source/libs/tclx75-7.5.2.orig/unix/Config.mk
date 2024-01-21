#
# Config.mk --
#
#   Master configuration file for Extended Tcl.  This should be the only
# file you have to modify to get Extended Tcl to work.  It is used to
# set attributes that configure can't figure out and to override certain 
# attributes set by configure.
# 
#   All the values in this directory are set to reasonable defaults.  You might
# want to tune them to your taste.  You may set the value of "CC" and "CFLAGS"
# in the file or on the make command line or set them.  For example:
#
#       make -k CC=gcc CFLAGS=-O
#
#------------------------------------------------------------------------------
# Copyright 1992-1996 Karl Lehenbauer and Mark Diekhans.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice appear in all copies.  Karl Lehenbauer and
# Mark Diekhans make no representations about the suitability of this
# software for any purpose.  It is provided "as is" without express or
# implied warranty.
#------------------------------------------------------------------------------
# $Id: Config.mk,v 7.0 1996/06/16 05:34:11 markd Exp $
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#              READ THIS FIRST: FILE PATH SPECIFICATION RULES.
#------------------------------------------------------------------------------
# All paths to files outside of the distribution MUST follow these rules.
# The rules make it easy to specify locations of files either relative to
# the source or build directories or or as absolute directories.  If these
# rules are not followed, it will not build.  All values are defaulted to
# reasonable locations.  If Tcl and Tk are in directories that are siblings
# of the TclX source directory, things will probably work just fine.
#
# File paths MUST be one of:
#   o Absolute paths (starting with /), e.g.  /usr/local/lib/libtcl.a
#   o Paths relative to the source directory make macro ${srcbasedir}, e.g.
#     -I$(srcbasedir)/../tk4.0
#   o Paths relative to the build directory make macro ${bldbasedir}, e.g.
#     ${bldbasedir}/../tk4.0/libtk.a
#
# Other macros used in file paths:
#   o TCLX_FULL_VERSION is the full TclX version.
#   o TKX_FULL_VERSION is the full TkX version.
#   o ARCH is either ".arch", as specified to configure, or empty.
#   o prefix and exec_prefix.
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# The Tcl source distribution directory, the path to tclConfig.sh, the Tcl
# library (libtcl7.4.a) and the flags neccessary to link with the Tcl shared
# library (libtcl7.4.so).  Note, access is required to tclInt.h which is not
# installed by Tcl.  If you want to use the installed Tcl library, set TCL_LIB
# to ${TCL_LIB_SPEC}.
# 

TCL_SRC=${srcbasedir}/../tcl7.5
TCL_BUILD=${bldbasedir}/../tcl7.5
TCL_CONFIG=${TCL_BUILD}/unix/tclConfig.sh
TCL_LIB=${TCL_BUILD_LIB_SPEC}

#------------------------------------------------------------------------------
# Unless configure is going to be run with --with-tk=NO, these defines must be
# set.  They define the directory containing the Tk source distribution, the
# path to tkConfig.sh, the path to the Tk library (libtk4.1.a) and the flags
# neccessary to link with the Tk shared library (libtk4.1.so).  If you want to
# use the installed Tk library, set TK_LIB to ${TK_LIB_SPEC}.

TK_SRC=${srcbasedir}/../tk4.1
TK_BUILD=${bldbasedir}/../tk4.1
TK_CONFIG=${TK_BUILD}/unix/tkConfig.sh
TK_LIB=${TK_BUILD_LIB_SPEC}

#------------------------------------------------------------------------------
# C compiler and debug/optimization/profiling flag to use.  Set by configure,
# and are normally overridden on the make command line (make CFLAGS=-g).  The
# can also be overridden here.

#CC=cc
#CFLAGS=-O

#------------------------------------------------------------------------------
# Definition of programs you wish to use. RANLIB is set by configure in the
# Makefiles, but they can be overridden here.
#

#RANLIB=ranlib

AR=ar
STRIP=strip

#------------------------------------------------------------------------------
# X is often in strange places, override what configure figured out if
# its wrong.

#XINCLUDES=-I/usr/somewhere/include
#XLIBSW=-L/usr/somewhere/lib -lX11

#------------------------------------------------------------------------------
# EXtra flags:
#   o XCFLAGS - Extra compiler flags on all compiles and links
#   o XLDFLAGS - Extra compiler flags to specify at link time.
#   o XLDLIBS - Extra libraries to use at link time.

XCFLAGS=
XLDFLAGS=
XLDLIBS=

#------------------------------------------------------------------------------
# The following definition can be set to non-null for special systems
# like AFS with replication.  It allows the pathnames used for installation
# to be different than those used for actually reference files at
# run-time.  INSTALL_ROOT is prepended to $prefix and $exec_prefix
# when installing files.

INSTALL_ROOT =

#------------------------------------------------------------------------------
# The TclX runtime directory that the Extended Tcl runtime files are installed
# into.

TCLX_INST_RUNTIME=${prefix}/tclX/${TCLX_FULL_VERSION}

#------------------------------------------------------------------------------
# The TkX runtimes that the Tk runtime files are installed into.  A directory
# whose name is the TkX version number will be built in this directory.

TKX_INST_RUNTIME=${prefix}/tkX/${TKX_FULL_VERSION}

#------------------------------------------------------------------------------
# The directory to install the tcl, wishx and tclhelp binaries in when the
# standard install model is used.

TCLX_INST_BIN=${exec_prefix}/bin${ARCH}

#------------------------------------------------------------------------------
# The directory to install the libtclx.a library in when the standard install
# model is used.

TCLX_INST_LIB=${exec_prefix}/lib${ARCH}

#------------------------------------------------------------------------------
# The directory the TclX .h files go in when the standard install model is
# used.

TCLX_INST_INCL=${prefix}/include

#==============================================================================
# These defines specify where and how the manual pages are to be installed.
# Since there are so many manual pages provided, they are placed together in
# one Tcl manual page directory by default, rather than splitting into the
# standard manual pages directories. You might want to modify these values.
#..............................................................................

#------------------------------------------------------------------------------
# o TCLX_MAN - Base manual directory where all of the man* and cat*
#   directories live.

TCLX_INST_MAN=${prefix}/man

#------------------------------------------------------------------------------
# o TCLX_MAN_CMD_SECTION - Section for TclX command manual pages.
# o TCLX_MAN_FUNC_SECTION - Section for TclX C level function manual pages.
#

TCLX_MAN_CMD_SECTION=n
TCLX_MAN_FUNC_SECTION=3

#------------------------------------------------------------------------------
# o MAN_DIR_SEPARATOR - The separator character used in the directory name
#   of the cat* and man* manual directories.  This is usually empty or 
#   a period. i.e "/usr/man/man1" or "/usr/man/man.1".  Autoconf attempts to
#   determine it but it can be overridden here.

#MAN_DIR_SEPARATOR=.
