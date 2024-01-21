#! ../bltwish

source bltDemo.tcl

# --------------------------------------------------------------------------
#
# debug --
#
#	A simple command tracing utility.  Uses the BLT "watch" command
#	to set up a command trace on all Tcl commands at or above a given
#	execution stack level.  If level is 0, then tracing is disabled.
#
# --------------------------------------------------------------------------

proc bltAfterWatch { level cmdStr argList code results } {
    set cmd [lindex $argList 0]
    puts stderr "$level $cmd => $argList\n<= ($code) $results"
}
proc bltBeforeWatch { level cmdStr argList } {
    set cmd [lindex $argList 0]
    puts stderr "$level $cmd => $cmdStr"
}

proc debug { level } {
    if { $level > 0 } {
	watch create DebugWatch \
	    -precmd bltBeforeWatch -postcmd bltAfterWatch -maxlevel $level 
    } else {
	watch delete DebugWatch
    }
}

