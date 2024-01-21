#
# compat --
#
# This file provides commands compatible with older versions of Extended Tcl.
# 
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
# $Id: compat.tcl,v 5.0 1995/07/25 05:59:46 markd Rel $
#------------------------------------------------------------------------------
#

#@package: TclX-Compatibility assign_fields server_open cexpand

proc assign_fields {list args} {
    if [lempty $args] {
        return
    }
    return [uplevel lassign [list $list] $args]
}

proc server_open args {
    set cmd server_connect

    set buffered 1
    while {[string match -* [lindex $args 0]]} {
        set opt [lvarpop args]
        if [cequal $opt -buf] {
            set buffered 1
        } elseif  [cequal $opt -nobuf] {
            set buffered 0
        }
        lappend cmd $opt
    }
    if $buffered {
        lappend cmd -twoids
    }
    set cmd [concat $cmd $args]

    uplevel $cmd
}

proc cexpand str {subst -nocommands -novariables $str}
