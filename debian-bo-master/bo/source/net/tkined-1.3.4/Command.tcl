##
## Command.tcl
##
## This file contains the commands of the Tkined editor. Commands are
## invoked from the pulldown menus or by the editor tools. Most
## commands simply call the corresponding proc for every selected
## object. Commands never change anything on the canvas directly.
## For most commands, the undo/redo buffer is modified with an
## inverse command.
##
## Copyright (c) 1993, 1994, 1995
##
## J. Schoenwaelder
## TU Braunschweig, Germany
## Institute for Operating Systems and Computer Networks
##
## Permission to use, copy, modify, and distribute this
## software and its documentation for any purpose and without
## fee is hereby granted, provided that this copyright
## notice appears in all copies.  The University of Braunschweig
## makes no representations about the suitability of this
## software for any purpose.  It is provided "as is" without
## express or implied warranty.
##

##
## Reinitialise the editor. Retrieve all objects of the canvas
## and delete them (ignore INTERPRETER and MENU objects). Make
## sure that grouped objects get deleted since a delete on a group
## object just deletes the group and its members.
##

proc Command::Clear { editor } {
    set confirm [Editor::BooleanAttribute $editor confirmClear]
    if {$confirm} {
	set w [$editor toplevel] 
	set name [$editor filename]
	set res [Dialog::confirm $w.canvas "Clear view $name?" \
		[list clear cancel] ]
	if {[lindex $res 0] != "clear"} return
    }
    $editor clear
    $editor filename "noname.tki"
}

##
## Read in a saved tkined map. The file just contains a series of
## ined commands that will rebuild the map.
##

proc Command::Merge { editor {fname {}} } {

    set w [$editor toplevel]
    if {$fname == ""} {
	set dir [$editor dirname]
	set fname [Dialog::fileselect $w.canvas "Select a file to merge:" $dir]
	if {$fname == ""} return
    }

    $w.menu.file configure -state disabled; update idletasks;
    if {[catch {$editor load $fname} err]} {
	Dialog::acknowledge $w.canvas "Can not read from file $fname: $err"
    }
    $w.menu.file configure -state normal
}

proc Command::Open { editor {fname {}} } {

    set w [$editor toplevel]
    if {$fname == ""} {
	set dir [$editor dirname]
	set fname [Dialog::fileselect $w.canvas "Select a file to open:" $dir]
	if {$fname == ""} {
	    return ""
	}
    }

    # expand URL ftp syntax

    if {[string match "ftp://*" $fname] || [string match "file://*" $fname]} {
	set idx [string first "://" $fname]
	incr idx 3
	set fname [string range $fname $idx end]
	set idx [string first "/" $fname]
	set server [string range $fname 0 [expr {$idx-1}]]
	set file [string range $fname $idx end]
	set fname "/tmp/$server:[file tail $file]"

	if {[catch {tkined_ftp $server $file $fname} err]} {
	    Dialog::acknowledge $w.canvas "Can not retrieve file $fname." $err
	    return ""
	}
    }

    $editor clear
    $editor dirname  [file dirname $fname]
    $editor filename [file tail $fname]

    $w.menu.file configure -state disabled; update idletasks;
    if {[catch {$editor load [$editor dirname]/[$editor filename]} err]} {
	Dialog::acknowledge $w.canvas \
	    "Can not read from file [$editor filename]: $err"
	set fname ""
    }
    $w.menu.file configure -state normal

    return $fname
}

##
## Save the current map. The filename is found in the attribute
## named filename.
##

proc Command::Save { editor } {

    set w [$editor toplevel]
    set fname "[$editor dirname]/[$editor filename]"
    if {[file exists $fname]} {
	if {![file writable $fname]} {
	    Dialog::acknowledge $w.canvas "Sorry, $fname is not writable."
	    return
	}
	set res [Dialog::confirm $w.canvas "Replace file $fname?" \
		 [list replace cancel]]
	if {$res == "cancel"} return
    }

    if {[catch {$editor save $fname} err]} {
	if {$err == ""} {
	    Dialog::acknowledge $w.canvas \
		"Can not write to file [file tail $fname]."
	} else {
	    Dialog::acknowledge $w.canvas \
		"Can not write to file [file tail $fname]:" "" $err
	}
	return
    }
}

##
## Save the map under a new filename. Get a new filename
## and call Command::Save to do the job.
##

proc Command::SaveAs { editor } {

    set w [$editor toplevel]
    set dir   [$editor dirname]
    set fname [Dialog::fileselect $w.canvas "Save as file:" $dir]
    if {$fname == ""} return
    $editor dirname  [file dirname $fname]
    $editor filename [file tail $fname]
    Command::Save $editor
}

##
## Dump the current map in postscript format. Before we start,
## we must set the background of all bitmaps to white. Otherwise
## we would get transparent bitmaps :-<.
##

proc Command::PostScript { editor } {

    set w [$editor toplevel]
    set dir [$editor dirname]
    set fname [Dialog::fileselect $w.canvas \
	       "Write PostScript to file:" $dir]
    if {$fname == ""} return

    while {[file exists $fname] && ![file writable $fname]} {
	set fname [Dialog::fileselect $w.canvas \
		   "Write PostScript to file:" $dir]
	if {$fname == ""} return
    }
    
    if {[file exists $fname]} {
	if {![file writable $fname]} {
	    Dialog::acknowledge $w.canvas "Can not write $fname"
	    return
	}
	set res [Dialog::confirm $w.canvas "Replace file $fname?" \
		 [list replace cancel]]
	if {$res == "cancel"} return
    }

    if {[catch {open $fname w} f]} {
	Dialog::acknowledge $w.canvas "Can not open $fname: $f"
	return
    }

    if {[catch {puts $f [$editor postscript]} err]} {
	Dialog::acknowledge $w.canvas "Failed to write $fname: $err"
    }

    catch {close $f}
    return
}

##
## Print a postscript dump. Use the proc above to get a postscript dump
## and push it to the printer.
##

proc Command::Print { editor } {

    set w [$editor toplevel]
    set fname "/tmp/tkined[pid].ps"
    catch {exec /bin/rm -f $fname}

    if {[file exists $fname] && ![file writable $fname]} {
	Dialog::acknowledge $w.canvas "Can not write temporary file $fname"
	return
    }

    if {[catch {open $fname w} f]} {
	Dialog::acknowledge $w.canvas "Can not open $fname: $f"
	return
    }

    if {[catch {puts $f [$editor postscript]} err]} {
	Dialog::acknowledge $w.canvas "Failed to write $fname: $err"
    }

    catch {close $f}

    Editor::print $editor $w.canvas $fname

    catch {exec rm -f $fname}
}

##
## Import a X11 bitmap as a background image. Since tk can not
## store bitmaps as strings, we must handle the filename.
##

proc Command::Import { editor } {

    set w [$editor toplevel]
    set dir [$editor dirname]
    set fname [Dialog::fileselect $w.canvas "Import background image:" $dir]
    if {$fname!=""} {
	if {[catch {IMAGE create $fname} image]} return
	$image editor $editor
	$image canvas $w.canvas
	$image color [$editor attribute color]
    }
}

##
## Let the user select one of the tkined files saved in the history.
##

proc Command::History { editor } {

    set w [$editor toplevel]
    set history [$editor attribute history]

    if {$history == ""} {
	Dialog::acknowledge $w.canvas "Sorry, no history available."
	return
    }

    set result [Dialog::list $w.canvas \
		"History of used tkined maps:" $history [list open cancel] ]
    if {[lindex $result 0] == "cancel"} return

    Command::Open $editor [lindex $result 1]
}

##
## This should better be called kill, since we do not care
## about anything. Ok, we could check if there is something
## to save before going down ...
##

proc Command::Close { editor } {
    set confirm [Editor::BooleanAttribute $editor confirmExit]
    if {$confirm} {
	set w [$editor toplevel] 
	set name [$editor filename]
	set res [Dialog::confirm $w.canvas "Close view $name?" \
		[list close cancel] ]
	if {[lindex $res 0] != "close"} return
    }
    $editor delete
}

##
## Here are all commands used in the edit menu.
##

proc Command::Scale { editor } {

    static factor
    if {![info exists factor]} { set factor 100 }

    set w [$editor toplevel]
    set result [Dialog::request $w.canvas \
	       "Set scaling factor of graphs and charts." \
		   [list [list {Scale factor:} $factor entry 12]] \
		   [list "set factor" cancel] ]
    if {[lindex $result 0] == "cancel"} return

    # check if the answer is a positiv number

    if {[catch {
	set val [lindex $result 1]
	incr val -1
	if {$val >= 0} {
	    set factor [lindex $result 1]
	}
    }]} return
    
    foreach id [$editor selection] {
	if {[lsearch "STRIPCHART BARCHART GRAPH" [$id type]] >= 0} {
	    $id scale $factor
	}
    }
}

proc Command::Undo { editor {body ""} } {

    static undolist

    set w [$editor toplevel]
    if {![info exists undolist($w)]} { set undolist($w) "{}" }
    if {$body!=""} {
	set undolist($w) "{$body} $undolist($w)"
    } else {
	if {[llength $undolist($w)]>1} {
	    set cmd [lindex $undolist($w) 0]
	    set undolist($w) [lrange $undolist($w) 1 end]
	    if {![catch {eval [lindex $cmd 0]}]} {
		Command::Redo $editor $cmd
	    }
	}
    }
}

proc Command::Redo { editor {body ""} } {

    static redolist

    set w [$editor toplevel]
    if {![info exists redolist($w)]} { set redolist($w) "{}" }
    if {$body!=""} {
        set redolist($w) "{$body} $redolist($w)"
    } else {
        if {[llength $redolist($w)]>1} {
	    set cmd [lindex $redolist($w) 0]
            set redolist($w) [lrange $redolist($w) 1 end]
            if {![catch {eval [lindex $cmd 1]}]} {
		Command::Undo $editor $cmd
	    }
        }
    }
}

proc Command::Delete { editor } {
    foreach id [$editor selection] {
	catch { $id delete }
    }
}

##
## The attribute sub-menu contains commands create, delete or 
## edit attributes.
##

proc Command::Attribute { editor what } {
    static attname attvalue
    if {![info exists attname]}  { set attname "" }
    if {![info exists attvalue]} { set attvalue "" }
    set w [$editor toplevel]
    if {$what == "delete"} {
	set result [Dialog::request $w.canvas "Delete Attribute:" \
		[list [list Name: $attname]] [list "delete attribute" cancel] ]
	if {[lindex $result 0] == "cancel"} return
        set attname [lindex $result 1]
	set attvalue ""
    } else {
	set result [Dialog::request $w.canvas "Edit Attribute:" \
		[list [list Name: $attname] [list Value: $attvalue]] \
		[list "set value" cancel] ]
	if {[lindex $result 0] == "cancel"} return
        set attname [lindex $result 1]
	set attvalue [lindex $result 2]
    }

    foreach id [$editor selection] {
	catch {$id attribute $attname $attvalue}
    }
}

##
## The label command selects an attribute for the label.
##

proc Command::Label { editor } {
    static attname
    if {![info exists attname]} { set attname "address" }
    set w [$editor toplevel]
    set result [Dialog::request $w.canvas "Select an Attribute as a Label:" \
	    [list [list Name: $attname entry 30] ] \
	    [list accept "no label" cancel] ]
    switch [lindex $result 0] {
	cancel {
	    return
	}
	"no label" {
	    set attname clear
	}
	default {
	    set attname [lindex $result 1]
	}	
    }
    foreach id [$editor selection] {
	catch {$id label $attname}
    }
}

##
## The select menu contains a set of commands to modify 
## the current selection.
##

proc Command::SelectAll { editor } {
    foreach id [$editor retrieve] {
	if {[$id type] == "IMAGE"} continue
	catch {	$id select }
    }
}

proc Command::SelectNeighbours { editor } {
    foreach id [$editor selection] { 
	set type [$id type]
	if {($type == "NODE") || ($type == "NETWORK")} {
	    foreach link_id [$id links] {
		[$link_id src] select
		[$link_id dst] select
	    }
	}
    }
}

proc Command::SelectMember { editor } {
    foreach id [$editor selection] {
        if {[$id type] != "GROUP"} continue
        foreach m [$id member] {
	    $m select
        }
    }
}

proc Command::SelectType { editor } {

    static type
    if {![info exists type]}  { set type  "NODE" }

    set w [$editor toplevel]
    set result [Dialog::request $w.canvas "Select objects by type." \
	         [list [list Type: dummy option NODE NETWORK LINK GROUP \
		       TEXT IMAGE REFERENCE STRIPCHART BARCHART GRAPH] ] \
		 [list select cancel] ]
    if {[lindex $result 0] == "cancel"} return

    set type  [lindex $result 1]
    foreach id [$editor retrieve $type] {
	$id select
    }
}

proc Command::SelectName { editor } {

    static regex type
    if {![info exists regex]} { set regex "" }
    if {![info exists type]}  { set type  "glob" }

    set w [$editor toplevel]
    set result [Dialog::request $w.canvas "Select objects by name." \
	        [list [list Expression: $regex entry 20] \
		      [list Type: $type radio regexp glob] ] \
		[list select cancel] ]

    set regex [lindex $result 1]
    set type  [lindex $result 2]

    if {[lindex $result 0] == "cancel"} return

    # test if the regex is valid
    if {[catch {lsearch -$type foobar $regex} res]} {
	Dialog::acknowledge $w.canvas \
	    "$regex is not a valid $type expression!"
	return
    }

    foreach id [$editor retrieve] {
	# not all object types are selectable
	catch {
	    if {[lsearch -$type [$id name] $regex] >= 0} {
		$id select
	    }
	}
    }
}

proc Command::SelectAddress { editor } {

    static regex type
    if {![info exists regex]} { set regex "" }
    if {![info exists type]}  { set type  "glob" }

    set w [$editor toplevel]
    set result [Dialog::request $w.canvas "Select objects by address." \
	        [list [list Expression: $regex entry 20] \
		      [list Type: $type radio regexp glob] ] \
		[list select cancel] ]

    set regex [lindex $result 1]
    set type  [lindex $result 2]

    if {[lindex $result 0] == "cancel"} return

    # test if the regex is valid
    if {[catch {lsearch -$type foobar $regex} res]} {
	Dialog::acknowledge $w.canvas \
	    "$regex is not a valid $type expression!"
	return
    }

    foreach id [$editor retrieve] {
	# not all object types are selectable
	catch {
	    if {[lsearch -$type [$id address] $regex] >= 0} {
		$id select
	    }
	}
    }
}

proc Command::SelectLabel { editor } {

    static regex type
    if {![info exists regex]} { set regex "" }
    if {![info exists type]}  { set type  "glob" }

    set w [$editor toplevel]
    set result [Dialog::request $w.canvas "Select objects by current label." \
	        [list [list Expression: $regex entry 20] \
		      [list Type: $type radio regexp glob] ] \
		[list select cancel] ]

    set regex [lindex $result 1]
    set type  [lindex $result 2]

    if {[lindex $result 0] == "cancel"} return

    # test if the regex is valid
    if {[catch {lsearch -$type foobar $regex} res]} {
	Dialog::acknowledge $w.canvas \
	    "$regex is not a valid $type expression!"
	return
    }

    foreach id [$editor retrieve] {
	foreach item [$id items] {
	    set label [$w.canvas itemcget label$item -text]
             if {[lsearch -$type $label $regex] >= 0} {
		$id select
	    }
	}
    }
}


##
## The structure menu allows to bring object to the front/back
## and to group/ungroup objects. A group can be collapsed or
## expanded.
##

proc Command::Front { editor } {
    set redo_cmd ""
    set undo_cmd ""
    foreach id [$editor selection] {
	if {[$id type] != "IMAGE"} {
	    append redo_cmd "$id raise; "
	    append undo_cmd "$id lower; "
	    $id raise
	}
    }
    Command::Undo $editor [list $undo_cmd $redo_cmd]
}

proc Command::Back { editor } {
    set redo_cmd ""
    set undo_cmd ""
    foreach id [$editor selection] {
	append redo_cmd "$id lower; "
	append undo_cmd "$id raise; "
        $id lower
    }
    Command::Undo $editor [list $undo_cmd $redo_cmd]
}

proc Command::GridSpacing { editor } {

    global stepx stepy

    if {![info exists stepx]} { set stepx 20 }
    if {![info exists stepy]} { set stepy 20 }

    set w [$editor toplevel]
    set result [Dialog::request $w.canvas \
	       "Set grid spacing in canvas pixel:" \
		   [list [list {X spacing:} $stepx scale 2 80] \
		         [list {Y spacing:} $stepy scale 2 80]] \
		   [list "set spacing" cancel] ] 
    if {[lindex $result 0] == "cancel"} return

    set stepx [lindex $result 1]
    set stepy [lindex $result 2]
}

proc Command::Grid { editor } {

    global stepx stepy

    if {![info exists stepx]} { set stepx 20 }
    if {![info exists stepy]} { set stepy 20 }

    set redo_cmd ""
    set undo_cmd ""
    foreach id [$editor selection] {
	set xy [$id move]
	set x [lindex $xy 0]
	set y [lindex $xy 1]
	set x [expr {int (($x + $stepx / 2.0) / $stepx) * $stepx - $x}]
	set y [expr {int (($y + $stepy / 2.0) / $stepy) * $stepy - $y}]
	$id move $x $y
	append redo_cmd "$id move $x $y; "
	append undo_cmd "$id move [expr {-1*$x}] [expr {-1*$y}]; "
    }
    Command::Undo $editor [list $undo_cmd $redo_cmd]
}

proc Command::Group { editor } {

    set w [$editor toplevel]
    set idlist [$editor selection]
    if {$idlist == ""} return

    set group [eval GROUP create $idlist]
    $group editor $editor
    $group canvas $w.canvas
    $group icon  [$editor attribute groupicon]
    $group font  [$editor attribute font]
    $group color [$editor attribute color]
    $group label name

    set member [$group member]
    set ignored ""
    foreach id $idlist {
	if {[lsearch -exact $member $id] < 0} {
	    lappend ignored [format "%-12s %-12s %s" [$id type] $id [$id name]]
	}
    }

    if {$ignored != ""} {
	Dialog::browse $w.canvas \
	    "The following objects have been ignored since" \
            "they are already a member of another group:" $ignored
    }

    $group select
}

proc Command::Ungroup {editor } {
    foreach id [$editor selection] {
        if {[$id type]=="GROUP"} {
	    $id ungroup
        }
    }
}

proc Command::Collapse { editor } {
    set redo_cmd ""
    set undo_cmd ""
    foreach id [$editor selection] {
	if {[$id type]=="GROUP"} {
	    append redo_cmd "$id collapse; "
	    append undo_cmd "$id expand; "
	    $id collapse
	}
    }
    Command::Undo $editor [list $undo_cmd $redo_cmd]
}

proc Command::Expand { editor } {
    set redo_cmd ""
    set undo_cmd ""
    foreach id [$editor selection] {
	if {[$id type]=="GROUP"} {
	    append redo_cmd "$id expand; "
	    append undo_cmd "$id collapse; "
	    $id expand
	}
    }
    Command::Undo $editor [list $undo_cmd $redo_cmd]
}

proc Command::Join { editor } {
    set w [$editor toplevel]
    set joiners ""
    foreach id [$editor selection] {
	if {[$id type] == "GROUP"} {
	    lappend groups $id
	    lappend names  [list $id [$id name]]
	} else {
	    lappend joiners $id
	}
    }
    if {![info exists groups]} {
	Dialog::acknowledge $w.canvas \
		"You must select a group to join objects into!"
	return
    }
    if {[llength $groups] != 1} {
	set result [Dialog::list $w.canvas "Select the group to join into:" \
		                 $names [list join cancel] ]
	if {[lindex $result 0] == "cancel"} return
	set group [lindex [lindex $result 1] 0]
	set idx [lsearch $groups $group]
	set joiners [concat $joiners [lreplace $groups $idx $idx]]
    } else {
	set group [lindex $groups 0]
    }

    set mem [concat [$group member] $joiners]
    eval $group member $mem
}

proc Command::RemoveGroup { editor } {
    foreach id [$editor selection] {
	if {[$id parent] != ""} {
	    lappend group([$id parent]) $id
	    catch {
		foreach l [$id links] {
		    if {[$l parent] != ""} {
			lappend group([$l parent]) $l
		    }
		}
	    }
	}
    }
    foreach g [array names group] {
	set new ""
	foreach id [$g member] {
	    if {[lsearch -exact $group($g) $id] < 0} {
		lappend new $id
	    }
	}
	if {$new == ""} {
	    $g member $new
	} else {
	    eval $g member $new
	}
    }
}

##
## Command to change the icon of the selected nodes.
##

proc Command::Icon { editor type name } {
    set redo_cmd ""
    set undo_cmd ""
    set w [$editor toplevel]
    foreach id [$editor selection] {
	if {[$id type]=="$type"} {
	    append redo_cmd "$id icon \"$name\"; "
	    append undo_cmd "$id icon \"[$id icon]\"; "
	    $id icon $name
	}
    }
    Command::Undo $editor [list $undo_cmd $redo_cmd]
    # set the default icon
    switch $type {
	NODE {
	    set bm [$editor attribute NODE-icon-$name]
	    if {$bm == ""} { set bm machine }
	    $w.tools.node configure -bitmap $bm
	    $editor attribute nodeicon $name
	}
	GROUP {
	    set bm [$editor attribute GROUP-icon-$name]
	    if {$bm == ""} { set bm group }
	    $w.tools.group configure -bitmap $bm
	    $editor attribute groupicon $name
	}
	NETWORK {
	    $editor attribute network $name
	}
	REFERENCE {
	    set bm [$editor attribute REFERENCE-icon-$name]
	    if {$bm == ""} { set bm reference }
	    $w.tools.reference configure -bitmap $bm
	    $editor attribute referenceicon $name
	}
    }
}

##
## Change the font of the current selection (labels and text objects).
##

proc Command::Font { editor fontname } {
    set redo_cmd ""
    set undo_cmd ""
    set w [$editor toplevel]
    foreach id [$editor selection] {
	set type [$id type]
	if {($type != "LINK") && ($type != "IMAGE")} {
	    append redo_cmd "$id font \"$fontname\"; "
	    append undo_cmd "$id font \"[$id font]\"; "
	    $id font $fontname
	}
    }
    $editor attribute font $fontname
    Command::Undo $editor [list $undo_cmd $redo_cmd]
}

##
## Change the color of the current selection.
##

proc Command::Color { editor colorname } {
    set redo_cmd ""
    set undo_cmd ""
    set w [$editor toplevel]
    foreach id [$editor selection] {
	append redo_cmd "$id color \"$colorname\"; "
	append undo_cmd "$id color \"[$id color]\"; "
	catch {$id color $colorname}
    }
    $editor attribute color $colorname
    Command::Undo $editor [list $undo_cmd $redo_cmd]
}

##
## Here are all commands used in the options menu.
##

proc Command::ToggleToolBox { editor } {
    set w [$editor toplevel]
    global showToolbox$w newToolbox$w
    if {[set showToolbox$w]} {
	if {[set newToolbox$w]} {
	    foreach child [winfo children $w.tools] {
		pack configure $child -side left -expand true
		if {[winfo class $child] == "Label"} {pack forget $child}
	    }
	    pack $w.tools -side top -fill x -after $w.menu -padx 5 -pady 5
	} else {
	    foreach child [winfo children $w.tools] {
		pack configure $child -side top -expand false
	    }
	    pack $w.tools.dummy -side top -pady 1 -ipadx 5 \
		-before $w.tools.select 
	    pack $w.tools -side left -fill y -before $w.menu -padx 0 -pady 0
	}
    } else {
	pack forget $w.tools
        Tool::Select $editor
    }
}

proc Command::LockEditor { editor } {
    set w [$editor toplevel]
    global lockEditor$w showToolbox$w
    if {[set showToolbox$w]} {
	set showToolbox$w 0
	Command::ToggleToolBox $editor
    }
    if {[set lockEditor$w]} {
	if {[set showToolbox$w]} {
	    set showToolbox$w 0
	    Command::ToggleToolBox $editor
	}
	Editor::lock $editor
    } else {
	if {! [set showToolbox$w]} {
	    set showToolbox$w 1
	    Command::ToggleToolBox $editor
	}
	Editor::unlock $editor
    }
}
