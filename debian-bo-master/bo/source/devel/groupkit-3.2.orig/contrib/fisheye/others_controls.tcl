# Create the top-level fisheye viewing widget that lets one adjust the effect on
# all the other participants focal point
# usage: MakeOthersControl 

set fish(structure_string) ""

proc MakeOthersControls {} {
    global fish
    set top .top2
    if {[winfo exists $top]} {return}

    toplevel $top
    wm title $top "Fisheye Controls"
    wm iconname $top "Fisheye"
    wm protocol $top WM_DELETE_WINDOW "destroy $top"

    set mb $top.mb
    set m $top.mb.m

    menubutton $mb -menu $mb.m -indicator yes -relief ridge -bd 2
    _SetMenuLabel $mb
    menu $m 
    foreach pair $fish(mag_factor) {
	set size [lindex $pair 0]
	$m add radio \
		-label "$size point" \
		-value $size \
		-variable fish(others_size) \
		-command "_SetMenuLabel $mb; ReConfigureFont; UpdateOthers"
    }
    pack $mb

    set s $top.s
    scale $s -orient horizontal -from 0 -to 10 -showvalue yes \
	-width 8 -variable fish(others_range) -command "UpdateOthers"
    pack $s

    set structure [StructureEditor $top]
    pack $structure
    # Buttons
    frame $top.fb
    pack $top.fb
    button $top.fb.d -text Done -command "destroy $top"
    pack $top.fb.d -side left
}

proc _SetMenuLabel {m} {
   global fish 
   $m configure -text "Font Size: $fish(others_size)"
   
}

proc StructureEditor {w} {
    global fish
    set f $w.f
    set fl  $f.label

    frame $f -relief ridge -bd 2

    label $fl -text "Set the other user's focal point\nto the regular expression that\nprecedes the current line"
    pack $fl -fill x
    return $f
}

proc MakeFormEntry {w label} {
    set fname $w.f[UniqueId]
    frame $fname
    pack $fname
    label $fname.l -text $label
    pack $fname.l -side left
    entry $fname.e
    bind $fname.e <Key> "global fish; set fish(structure_string) [$fname.e get]; puts [$fname.e get]"
    pack $fname.e -side right -fill x -expand y
    return $fname
}

proc UniqueId {} {
    global uniqueid
    if {[info exists uniqueid] == 0} {return [set uniqueid 0]} {return [incr uniqueid]} 
}
