#
# genindex.tcl --
#
#  Interface to the buildpackageindex that doesn't go through the auto-load
# mechanism.  This helps if something is broken with auto-load so the
# build at least completes.
#------------------------------------------------------------------------------
# Copyright 1992-1995 Karl Lehenbauer and Mark Diekhans.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice appear in all copies.  Karl Lehenbauer and
# Mark Diekhans make no representations about the suitability of this
# software for any purpose.  It is provided "as is" without express or
# implied warranty.
#------------------------------------------------------------------------------
# $Id: genindex.tcl,v 5.0 1995/07/25 05:59:37 markd Rel $
#------------------------------------------------------------------------------
#

global tclx_library
source $tclx_library/buildidx.tcl

buildpackageindex $argv
