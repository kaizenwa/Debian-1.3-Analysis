#
# $Source: /home/nlfm/Working/Zircon/Released/lib/RCS/Help.tcl,v $
# $Date: 1996/06/04 08:37:39 $
# $Revision: 1.17.1.1 $
#
# ----------------------------------------------------------------------
#   AUTHOR:  Lindsay Marshall <lindsay.marshall@newcastle.ac.uk>
# ----------------------------------------------------------------------
# Copyright 1995 The University of Newcastle upon Tyne (see COPYRIGHT)
# ======================================================================
#
proc makeHelp {win nm} {
    $win delete 0 end
    global zircon
    if [catch {set st [glob $zircon(lib)/help/$nm/*]}] {
	$win add command -label {Sorry, not yet available.} -command {}
    } {
	$win delete 0 end
	foreach x $st {
	    set tx [file tail $x]
	    set ntx [string tolower $tx]
	    if [file isdir $x] {
		$win add cascade -label $tx -menu $win.$ntx
		if ![winfo exists $win.$ntx] {
		    menu $win.$ntx -tearoff 0 \
		      -postcommand "makeHelp $win.$ntx $nm/$tx"
		}
	    } { 
		$win add command -label $tx \
		  -command "zHelpWind .@$nm$tx $x {Zircon Help : $nm > $tx}"
	    }
	}
    }
}
#
proc zHelpWind {ctl file title} {
    if [winfo exists $ctl] { popup $ctl ; return }
    toplevel $ctl -class Zircon -borderwidth 2
    set oFrm $ctl
    wm title $ctl $title
    wm resizable $ctl 1 1
    wm protocol $ctl WM_DELETE_WINDOW "destroy $ctl"
    set f [frame $ctl.t]
    scrollbar $f.vs -command "$f.txt yview"
    text $f.txt -yscrollcommand "$f.vs set" -height 10 -width 40
    $f.txt tag configure input -foreground red
    $f.txt tag configure output -foreground black
    pack $f.vs -side right -fill y
    pack $f.txt -side left -expand 1 -fill both
    pack $f -fill both -expand 1
    set f [frame $ctl.btn]
    global ztrans
    button $f.close -text $ztrans(dismiss) -command "destroy $ctl"
    pack $f.close -expand 1 -side left
    pack $f -fill x
    set desc [open $file r]
    while {![eof $desc]} {
	gets $desc buffer
	if [string match {} $buffer] {
	    $ctl.t.txt insert end "\n\n"
	} {
	    $ctl.t.txt insert end "$buffer "
	}
    }
    close $desc
    $ctl.t.txt configure -state disabled
}
