#
# forfile.tcl --
#
# Proc to execute code on every line of a file.
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
# $Id: forfile.tcl,v 5.0 1995/07/25 05:59:58 markd Rel $
#------------------------------------------------------------------------------
#

#@package: TclX-forfile for_file

proc for_file {var filename cmd} {
    upvar $var line
    set fp [open $filename r]
    set code 0
    set result {}
    while {[gets $fp line] >= 0} {
        set code [catch {uplevel 1 $cmd} result]
        if {$code != 0 && $code != 4} break
    }
    close $fp

    if {$code == 0 || $code == 3 || $code == 4} {
        return $result
    }
    if {$code == 1} {
        global errorCode errorInfo
        return -code $code -errorcode $errorCode -errorinfo $errorInfo $result
    }
    return -code $code $result
}
