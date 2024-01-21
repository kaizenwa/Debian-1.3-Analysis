#-----------------------------------------------------------------------------
# tkx.tcl -- Extended Tcl Tk initialization.
#-----------------------------------------------------------------------------
# $Id: tkx.tcl,v 8.0 1996/11/21 00:25:34 markd Exp $
#-----------------------------------------------------------------------------

if {[info exists tkx_library] && ![cequal $tkx_library {}]} {
    if ![lcontain $auto_path $tkx_library] {
	lappend auto_path $tkx_library
    }
}
