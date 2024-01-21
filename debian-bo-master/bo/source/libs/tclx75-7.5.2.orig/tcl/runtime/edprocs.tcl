#
# edprocs.tcl --
#
# Tools for Tcl developers. Procedures to save procs to a file and to edit
# a proc in memory.
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
# $Id: edprocs.tcl,v 7.0 1996/06/16 05:31:17 markd Exp $
#------------------------------------------------------------------------------
#

#@package: TclX-developer_utils saveprocs edprocs

proc saveprocs {fileName args} {
    set fp [open $fileName w]
    puts $fp "# tcl procs saved on [fmtclock [getclock]]\n"
    puts $fp [eval "showproc $args"]
    close $fp
}

proc edprocs {args} {
    global env

    set tmpFilename /tmp/tcldev.[id process]

    set fp [open $tmpFilename w]
    puts $fp "\n# TEMP EDIT BUFFER -- YOUR CHANGES ARE FOR THIS SESSION ONLY\n"
    puts $fp [eval "showproc $args"]
    close $fp

    if [info exists env(EDITOR)] {
        set editor $env(EDITOR)
    } else {
	set editor vi
    }

    set startMtime [file mtime $tmpFilename]
    system "$editor $tmpFilename"

    if {[file mtime $tmpFilename] != $startMtime} {
	source $tmpFilename
	echo "Procedures were reloaded."
    } else {
	echo "No changes were made."
    }
    unlink $tmpFilename
    return
}
