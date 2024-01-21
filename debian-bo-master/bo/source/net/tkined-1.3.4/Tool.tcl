##
## Tool.tcl
##
## This file contains the tools of the Tkined editor. A Tool is something
## you can select and apply on the canvas. You will normally have an
## animation bind to mouse events. This is the most complicated stuff and 
## needs some more clean ups.
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
## Find the topmost item at position x and y that is of one of 
## the Tkined types given in the list types.
##

proc Tool::Find { c x y types } {
    set lowx [expr $x-5]
    set lowy [expr $y-5]
    set upx  [expr $x+5]
    set upy  [expr $y+5]
    set first [$c find closest $x $y 3]
    set item $first
    while 1 {
	set inregion [$c find overlapping $lowx $lowy $upx $upy]
	if {[lsearch $inregion $item] >= 0} {
	    set tags [$c gettags $item]
	    if {[lsearch -glob $tags label*]>=0 && [lsearch $types LABEL]>=0} {
		return $item
	    }
	    foreach type $types {
		if {[lsearch $tags $type] >= 0} {
		    return $item
		}
	    }
	}
	    
	set item [$c find closest $x $y 3 $item]
	if {$item == $first} return ""
    }
}

##
## Return the id of the object that this item belongs to. We just scan 
## through the tags for an entry with the format "id <id>".
##

proc Tool::GetId { c item } {
    foreach tag [$c gettags $item] {
	if {[lindex $tag 0]=="id"} {
	    return [lindex $tag 1]
	}
    }
    return ""
}

##
## Make a popup menu for the alter tool. 
##

proc Tool::MakePopup { popup id } {

    $popup add command -label "Create a new Attribute..." \
	    -command "Tool::CreateAttribute $id"
    if {[$id attribute] != ""} {
	$popup add command -label "Delete an Attribute..." \
		-command "Tool::DeleteAttribute $id"
    }
    if {[$id type] != "LINK" || [$id attribute] != ""} {
	$popup add command -label "Edit all Attributes..." \
		-command "Tool::EditAttribute $id"
    }
    if {[$id type] != "LINK"} {
	$popup add command -label "Label with Attribute..." \
		-command "Tool::LabelAttribute $id"
    }

    set editor [$id editor]
    set w [$editor toplevel]
    set cnt 0
    foreach menu [$editor retrieve MENU] {
	set attr [$id attribute "Tkined:[$menu name]"]
	if {$attr == ""} {
	    set attr [$editor attribute [$menu name]]
	}
	if {$attr == ""} continue
	set inter [$menu interpreter]
	set items ""
	foreach item [$menu items] {
	    lappend items [lindex [split $item :] end]
	}
	set args ""
	foreach cmd $attr {
	    if {[lsearch $items $cmd] < 0} continue
	    lappend args $cmd
	}
	if {$args == ""} continue
	if {[incr cnt] == 1} {
	    $popup add separator
	}
	$popup add cascade -label [$menu name] -menu $popup.$menu \
		-state [$w.menu.$menu cget -state]
	menu $popup.$menu -tearoff false
	foreach cmd $args {
	    $popup.$menu add command -label $cmd -command \
		    [list [$menu interpreter] send $cmd [list [$id retrieve]]]
	}
    }
}

proc Tool::CreateAttribute { id } {

    set result [Dialog::request [$id canvas] "Create an attribute for $id:" \
		[list [list Name: "" ] [list Value: "" ] ] \
		[list create cancel] ]

    if {[lindex $result 0] == "cancel"} return

    $id attribute [lindex $result 1] [lindex $result 2]
}

proc Tool::DeleteAttribute { id } {

    set result [Dialog::list [$id canvas] "Delete an attribute of $id:" \
		[lsort [$id attribute]] [list delete cancel] ]

    if {[lindex $result 0] == "cancel"} return

    $id attribute [lindex $result 1] ""
}

proc Tool::EditAttribute { id } {

    set list ""
    if {[$id type] != "LINK"} {
	lappend list [list name [$id name] entry 40]
	if {[$id type] != "GROUP"} {
	    lappend list [list address [$id address] entry 40]
	}
    }
    foreach att [lsort [$id attribute]] {
	lappend list [list $att [$id attribute $att] entry 40]
    }
    if {$list == ""} return

    set result [Dialog::request [$id canvas] "Edit attributes on $id:" \
		$list [list "set values" cancel] ]

    if {[lindex $result 0] == "cancel"} return

    set i 1
    foreach att $list {
	set att [lindex $att 0]
	if {$i == 1} { 
	    $id name [lindex $result $i]
	} elseif {$i == 2 && [$id type] != "GROUP"} {
	    $id address [lindex $result $i]
	} else {
	    $id attribute $att [lindex $result $i]
	}
	incr i
    }
}

proc Tool::LabelAttribute { id } {
    set list [concat "name address" [lsort [$id attribute]]]
    set result [Dialog::list [$id canvas] \
	    "Select an attribute as a label for $id:" $list \
	    [list accept "no label" cancel] ]
    switch [lindex $result 0] {
	cancel {
	    return
	}
	"no label" {
	    $id label clear
	}
	default {
	    set attribute [lindex $result 1]
	    if {$attribute != ""} {
		$id label $attribute
	    }
	}
    }
}

##
## This procedure should be called whenever a tool is selected.
## It sets the relief of the button to sunken after resetting the
## canvas and button states to an initialized state.
##

proc Tool::Init { editor tool } {

    set w [$editor toplevel]
    set c $w.canvas

    $c configure -cursor top_left_arrow

    $c delete area
    $c delete network
    $c delete line
    $c delete scale

    foreach child [winfo children $w.tools] {
	$child configure -relief flat
    }

    set button $w.tools.$tool
    $button configure -relief groove

    $c focus {}
    focus [winfo toplevel $c]

    bind $c <1> ""
    bind $c <B1-Motion> ""
    bind $c <ButtonRelease-1> ""
    bind $c <Shift-Button-1> ""
    bind $c <Motion> ""
    bind $c <Shift-Motion> ""
    bind $c <Shift-B1-Motion> ""
    bind $c <Shift-ButtonRelease-1> ""

    # these bindings allow drag operations on the canvas
    bind $w.canvas <Shift-2> "Tool::ScrollMark $w.canvas %x %y"
    bind $w.canvas <Shift-B2-Motion> "Tool::ScrollDrag $w.canvas %x %y"
    bind $w.canvas <Shift-ButtonRelease-2> "Tool::ScrollDone $w.canvas"
}

##
## The following procedures enable the scroll operation on the canvas.
##

proc Tool::ScrollMark {c x y} {
    catch {destroy $c.popup}
    $c configure -cursor fleur
    $c scan mark $x $y
}

proc Tool::ScrollDrag {c x y} {
    $c scan dragto $x $y
}

proc Tool::ScrollDone {c} {
    $c configure -cursor top_left_arrow
}

##
## Implementation of the select tool.
##

proc Tool::Select { editor } {

    Tool::Init $editor select

    set c [$editor toplevel].canvas
    
    bind $c <1> \
	"$editor selection clear; \
         Tool::SelectMark $editor \
         \[%W canvasx %x\] \[%W canvasy %y\]"
    bind $c <B1-Motion> \
        "Tool::SelectDrag $editor \
            \[%W canvasx %x\] \[%W canvasy %y\]"
    bind $c <ButtonRelease-1> \
        "Tool::SelectDone $editor"
    bind $c <Shift-Button-1> \
	"Tool::SelectMark $editor \
	    \[%W canvasx %x\] \[%W canvasy %y\]"
    bind $c <Shift-B1-Motion> \
        "Tool::SelectDrag $editor \
            \[%W canvasx %x\] \[%W canvasy %y\]"
    bind $c <Shift-ButtonRelease-1> \
        "Tool::SelectDone $editor"
}

proc Tool::SelectApply { editor x y } {
    set c [$editor toplevel].canvas    
    set x1 [expr {$x-5}]
    set y1 [expr {$y-5}]
    set x2 [expr {$x+5}]
    set y2 [expr {$y+5}]
    foreach item [$c find overlapping $x1 $y1 $x2 $y2] {
	set item_id [Tool::GetId $c $item]
	if {$item_id == ""} continue
	set id $item_id
    }
    if {![catch {$id selected} stat]} {
	if {$stat} { $id unselect } else { $id select }
    }
}

proc Tool::SelectMark { editor x y } {
    global tkined_areaX1 tkined_areaY1 tkined_areaX2 tkined_areaY2 \
	tkined_cursor tkined_valid
    set c [$editor toplevel].canvas    
    $c delete scale
    set x1 [expr {$x-5}]
    set y1 [expr {$y-5}]
    set x2 [expr {$x+5}]
    set y2 [expr {$y+5}]
    set tkined_valid 1
    foreach item [$c find overlapping $x1 $y1 $x2 $y2] {
	set tkined_valid 0
	set tags [$c gettags $item]
	if {[lsearch $tags IMAGE]<0} break
	set tkined_valid 1
    }
     if {$tkined_valid} {
	set tkined_areaX1 $x
	set tkined_areaY1 $y
	set tkined_areaX2 $x
	set tkined_areaY2 $y
	set tkined_cursor nope
	$c delete area
    } else {
	Tool::SelectApply $editor $x $y
    }
}

proc Tool::SelectDrag { editor x y } {
    global tkined_areaX1 tkined_areaY1 tkined_areaX2 tkined_areaY2 \
	tkined_cursor tkined_valid
    if {![info exists tkined_valid] || !$tkined_valid} return
    set c [$editor toplevel].canvas    
    if {($tkined_areaX1 != $x) && ($tkined_areaY1 != $y)} {
	$c delete area
	$c create rectangle $tkined_areaX1 $tkined_areaY1 \
	    $x $y -outline black -tags "area"
	set tkined_areaX2 $x
	set tkined_areaY2 $y
    }
    if {$tkined_areaX2>$tkined_areaX1} {
	if {$tkined_areaY2>$tkined_areaY1} {
	    set new_cursor bottom_right_corner
	} else {
	    set new_cursor top_right_corner
	}
    } else {
	if {$tkined_areaY2>$tkined_areaY1} {
	    set new_cursor bottom_left_corner
	} else {
	    set new_cursor top_left_corner
	}
    }
    if {$new_cursor!=$tkined_cursor} {
	set tkined_cursor $new_cursor
	$c configure -cursor $tkined_cursor
    }
}

proc Tool::SelectDone { editor } {
    global tkined_areaX1 tkined_areaY1 tkined_areaX2 tkined_areaY2 \
	tkined_valid
    if {![info exists tkined_valid] || !$tkined_valid} return
    set w [$editor toplevel]
    set c $w.canvas    
    $w.tools.select configure -state disabled
    update idletask
    set area [$c find withtag area]
    foreach item [$c find enclosed $tkined_areaX1 $tkined_areaY1 \
		  $tkined_areaX2 $tkined_areaY2] {
        set id [Tool::GetId $c $item]
        if {$id == ""} continue
	set table($id) ""
    }
    foreach id [array names table] {
	if {[catch {$id selected} sel]} continue
	if {$sel} { $id unselect } else { $id select }
    }
    $c delete area
    $c configure -cursor top_left_arrow
    $w.tools.select configure -state normal
}

##
## Implementation of the move tool.
##

proc Tool::Move { editor } {
    set w [$editor toplevel]
    bind $w.canvas <2> "Tool::MoveMark $editor \
                \[$w.canvas canvasx %x\] \[$w.canvas canvasy %y\]"
    bind $w.canvas <B2-Motion> "Tool::MoveDrag $editor \
                \[$w.canvas canvasx %x\] \[$w.canvas canvasy %y\]"
    bind $w.canvas <ButtonRelease-2> "Tool::MoveDone $editor \
                \[$w.canvas canvasx %x\] \[$w.canvas canvasy %y\]"
}

proc Tool::MoveMark { editor x y } {
    global tkined_startX tkined_startY tkined_oldX tkined_oldY tkined_valid

    set c [$editor toplevel].canvas    
    $c delete scale
    set x1 [expr {$x-5}]
    set y1 [expr {$y-5}]
    set x2 [expr {$x+5}]
    set y2 [expr {$y+5}]
    set unselected ""
    set tkined_valid 0
    foreach item [$c find overlapping $x1 $y1 $x2 $y2] {
	set id [Tool::GetId $c $item]
	if {$id == ""} continue
	if {[catch {$id selected} sel]} continue
        if {$sel} {
	    set tkined_valid 1
	    break
	} else {
	    set unselected $id
	}
    }
    if {! $tkined_valid && $unselected != ""} {
	$editor selection clear
	if {[catch {$unselected select}]} return
	set tkined_valid 1
    }
    if {!$tkined_valid} {
	Tool::ScrollMark $c [expr int($x)] [expr int($y)]
	bind $c <B2-Motion> "Tool::ScrollDrag $c %x %y"
	bind $c <ButtonRelease-2> "Tool::ScrollDone $c; Tool::Move $editor"
	return
    }
    set tkined_startX $x
    set tkined_startY $y
    set tkined_oldX $x
    set tkined_oldY $y
    set selection [$c find withtag selected]
    set tkined_valid [expr {$selection!=""}]
    foreach item [$c find overlapping $x $y $x $y] {
	if [expr {[lsearch [$c gettags $item] selected]>=0}] {
	    set tkined_valid 1
	    break
	}
    }
    if {$tkined_valid} {
	set area [eval $c bbox $selection]
	eval $c create rectangle $area -outline black -tags "area"
    }
}

proc Tool::MoveDrag { editor x y } {
    global tkined_startX tkined_startY tkined_oldX tkined_oldY tkined_valid
    static X Y
    if {!$tkined_valid} return
    set c [$editor toplevel].canvas    
    set dx [expr $x-$tkined_oldX]
    set dy [expr $y-$tkined_oldY]
    if {($dx != 0) && ($dy != 0)} {
	$c move area $dx $dy
	set tkined_oldX $x
	set tkined_oldY $y
    }
}

proc Tool::MoveDone { editor x y } {
    global tkined_startX tkined_startY tkined_valid
    set redo_cmd ""
    set undo_cmd ""
    if {!$tkined_valid} return
    set w [$editor toplevel]
    set c $w.canvas
    update idletask
    set dx [expr $x-$tkined_startX]
    set dy [expr $y-$tkined_startY]
    set rx [expr {-1*$dx}]
    set ry [expr {-1*$dy}]
    set idlist ""
    if {($dx != 0) && ($dy != 0)} {
	foreach item [$c find withtag selected] {
	    set id [Tool::GetId $c $item]
	    if {$id == ""} continue
	    lappend idlist $id
	    if {[$id type] == "GROUP"} {
		foreach m [$id member] {
		    set ignore($m) ""
		}
	    }
	}
    }
    foreach id $idlist {
	if {![info exists ignore($id)]} {
	    $id move $dx $dy
	    append redo_cmd "$id move $dx $dy; "
	    append undo_cmd "$id move $rx $ry; "
	}
    }
    if {$undo_cmd != ""} {
	Command::Undo $editor [list $undo_cmd $redo_cmd]
    }
    $c delete area
}

##
## Implementation of the text tool. It can be used to enter text
## on the canvas (creating a text item). We currently do not support
## the selection mechanisms.
##

proc Tool::Text { editor } {

    Tool::Init $editor text

    set c [$editor toplevel].canvas

    $c configure -cursor xterm
    
    bind $c <1> "Tool::TextMark $editor \
                \[$c canvasx %x\] \[$c canvasy %y\]"
}

proc Tool::TextBs { w item } {
    set char [expr {[$w index $item insert] - 1}]
    if {$char >= 0} {$w dchar $item $char}
}

proc Tool::TextMark { editor x y } {
    set w [$editor toplevel]
    set c $w.canvas
    set fontname [$editor attribute font]
    set x11fontname [$editor attribute font-$fontname]
    if {$x11fontname == ""} {
	set x11fontname fixed
    }
    set colorname [$editor attribute color]
    set x11colorname [$editor attribute color-$colorname]
    if {$x11colorname == ""} { set x11colorname black }  
    set item [$c create text $x $y -anchor nw -tags "text" \
	      -font $x11fontname -fill $x11colorname]
    $c bind $item <KeyPress>       "$c insert $item insert %A;  break"
    $c bind $item <Shift-KeyPress> "$c insert $item insert %A;  break"
    $c bind $item <Return>         "$c insert $item insert \\n; break"
    $c bind $item <BackSpace>      "Tool::TextBs $c $item; break"
    $c bind $item <Control-h>      "Tool::TextBs $c $item; break"
    $c bind $item <Delete>         "Tool::TextBs $c $item; break"
    # make sure that no toplevel bindings are invoked
    bind $c <KeyPress> "break"

    $c bind $item <Any-Leave>      "Tool::TextDone $editor"
##    bind $c <FocusOut>             "Tool::TextDone $editor"

    # XXX a tk4.0 hack - perhaps it will go when tk4.0 gets stable ...
    set x [expr int($x)]
    set y [expr int($y)]

    $c icursor $item @$x,$y
    $c focus $item
    focus $c
    $c select from $item @$x,$y
}

proc Tool::TextDone { editor } {
    set w [$editor toplevel]
    set c $w.canvas
    foreach item [$c find withtag text] {
	set xy [$c coords $item]
	set txt [$c itemcget $item -text]
	set text [TEXT create $txt]
	$text editor $editor
	$text canvas $c
	$text move  [lindex $xy 0] [lindex $xy 1]
	$text font  [$editor attribute font]
	$text color [$editor attribute color]
	$c delete $item
    }
    $c focus ""
    focus [winfo toplevel $c]
}

##
## The Resize tool is used to resize strip- or barcharts and to
## stretch or shrink networks.
##

proc Tool::Resize { editor } {
    Tool::Init $editor resize

    set c [$editor toplevel].canvas

    bind $c <1> "Tool::ResizeMark $editor \
                \[$c canvasx %x\] \[$c canvasy %y\] %X %Y"
}

proc Tool::ResizeMark { editor cx cy xx xy } {

    set c [$editor toplevel].canvas
    $c delete scale

    set item [Tool::Find $c $cx $cy "STRIPCHART BARCHART NETWORK"]
    if {$item == ""} return

    set x1 [expr {$cx-5}]
    set y1 [expr {$cy-5}]
    set x2 [expr {$cx+5}]
    set y2 [expr {$cy+5}]
    set id [Tool::GetId $c $item]
    set type [$id type]
    if {[lsearch -glob [$c gettags $item] label*] >= 0} {
	set type LABEL
    }
    switch $type {

	NETWORK {

	    set bb [$c coords $item]
	    set len [llength $bb]
	    incr len -1
	    set bx1 [lindex $bb 0]
	    set by1 [lindex $bb 1]
	    set bx2 [lindex $bb [expr {$len-1}]]
	    set by2 [lindex $bb $len]

	    if { ($x1<$bx1) && ($bx1<$x2) && ($y1<$by1) && ($by1<$y2)} {
		Tool::ResizeNetworkMark $editor $id $item $bx2 $by2 $bx1 $by1
		return
	    }
	    
	    if { ($x1<$bx2) && ($bx2<$x2) && ($y1<$by2) && ($by2<$y2)} {
		Tool::ResizeNetworkMark $editor $id $item $bx1 $by1 $bx2 $by2
		return
	    }
	}
	STRIPCHART -
	BARCHART {

	    set bb [$c bbox $item]
	    set bx1 [lindex $bb 0]
	    set by1 [lindex $bb 1]
	    set bx2 [lindex $bb 2]
	    set by2 [lindex $bb 3]
    
	    if { ($x1<$bx1) && ($bx1<$x2) && ($y1<$by1) && ($by1<$y2)} {
		Tool::ResizeChart $editor $id $item $bx2 $by2 $bx1 $by1
		return
	    }
	    
	    if { ($x1<$bx1) && ($bx1<$x2) && ($y1<$by2) && ($by2<$y2)} {
		Tool::ResizeChart $editor $id $item $bx2 $by1 $bx1 $by2
		return
	    }
	    
	    if { ($x1<$bx2) && ($bx2<$x2) && ($y1<$by1) && ($by1<$y2)} {
		Tool::ResizeChart $editor $id $item $bx1 $by2 $bx2 $by1
		return
	    }
	    
	    if { ($x1<$bx2) && ($bx2<$x2) && ($y1<$by2) && ($by2<$y2)} {
		Tool::ResizeChart $editor $id $item $bx1 $by1 $bx2 $by2
		return
	    }
	}
    }
		    
}

proc Tool::ResizeNetworkMark { editor id item x1 y1 x2 y2 } {
    global tkined_areaX1 tkined_areaY1 tkined_areaX2 tkined_areaY2
    set c [$editor toplevel].canvas
    set tkined_areaX1 $x1
    set tkined_areaY1 $y1
    set tkined_areaX2 $x2
    set tkined_areaY2 $y2

    $c delete area

    if {$x1 == $x2} {
	bind $c <B1-Motion> \
	    "Tool::ResizeNetworkDrag $editor $id $item $x1 \[$c canvasy %y\]"
	bind $c <ButtonRelease-1> \
	    "Tool::ResizeNetworkDone $editor $id $item $x1 \[$c canvasy %y\]"
    }

    if {$y1 == $y2} {
	bind $c <B1-Motion> \
	    "Tool::ResizeNetworkDrag $editor $id $item \[$c canvasx %x\] $y1"
	bind $c <ButtonRelease-1> \
	    "Tool::ResizeNetworkDone $editor $id $item \[$c canvasx %x\] $y1"
    }
}

proc Tool::ResizeNetworkDrag { editor id item x y } {
    global tkined_areaX1 tkined_areaY1 tkined_areaX2 tkined_areaY2
    set c [$editor toplevel].canvas

    set tkined_areaX2 $x
    set tkined_areaY2 $y
    $c coords $item $tkined_areaX1 $tkined_areaY1 $tkined_areaX2 $tkined_areaY2
}

proc Tool::ResizeNetworkDone { editor id item x y } {
    global tkined_areaX1 tkined_areaY1 tkined_areaX2 tkined_areaY2
    set c [$editor toplevel].canvas

    set sx [lindex [$id move] 0]
    set sy [lindex [$id move] 1]
    if {$sx == $tkined_areaX1 && $sy == $tkined_areaY1} {
	set tkined_areaX2 [expr $tkined_areaX2 - $tkined_areaX1]
	set tkined_areaY2 [expr $tkined_areaY2 - $tkined_areaY1]
	$id points "0 0 $tkined_areaX2 $tkined_areaY2"
    } else {
	set dx [expr {$tkined_areaX2 - $sx}]
	set dy [expr {$tkined_areaY2 - $sy}]
	$c move $item [expr {-1 * $dx}] [expr {-1 * $dy}]
	$id move $dx $dy
	set tkined_areaX2 [expr $tkined_areaX1 - $tkined_areaX2]
	set tkined_areaY2 [expr $tkined_areaY1 - $tkined_areaY2]
	$id points "0 0 $tkined_areaX2 $tkined_areaY2"
    }
    Tool::Resize $editor
}

proc Tool::ResizeChart { editor id item x1 y1 x2 y2 } {
    global tkined_areaX1 tkined_areaY1 tkined_areaX2 tkined_areaY2
    set c [$editor toplevel].canvas
    set tkined_areaX1 $x1
    set tkined_areaY1 $y1
    set tkined_areaX2 $x2
    set tkined_areaY2 $y2
    $c delete area
    bind $c <B1-Motion> \
	"Tool::ResizeChartDrag $editor $id $item \
            \[$c canvasx %x\] \[$c canvasy %y\]"
    bind $c <ButtonRelease-1> \
	"Tool::ResizeChartDone $editor $id $item \
            \[$c canvasx %x\] \[$c canvasy %y\]"
}

proc Tool::ResizeChartDrag { editor id item x y } {
    global tkined_areaX1 tkined_areaY1 tkined_areaX2 tkined_areaY2
    set c [$editor toplevel].canvas
    if {($tkined_areaX1 != $x) && ($tkined_areaY1 != $y)} {
	set tkined_areaX2 $x
	set tkined_areaY2 $y
	$c delete area
	$c create rectangle \
	    $tkined_areaX1 $tkined_areaY1 $tkined_areaX2 $tkined_areaY2 \
	    -outline black -tags "area"
    }
}

proc Tool::ResizeChartDone { editor id item x y } {
    global tkined_areaX1 tkined_areaY1 tkined_areaX2 tkined_areaY2
    set c [$editor toplevel].canvas
    $c delete area
    $id size $tkined_areaX1 $tkined_areaY1 $tkined_areaX2 $tkined_areaY2
    Tool::Resize $editor
}

##
## The Alter tool can be used to get and set informations about
## an object. The various attributes are shown in the label of
## the object.
##

proc Tool::Alter { editor } {

    Tool::Init $editor alter

    set c [$editor toplevel].canvas

    bind $c <1> "Tool::AlterMark $editor \
                \[$c canvasx %x\] \[$c canvasy %y\] %X %Y"
}

proc Tool::AlterMark { editor cx cy xx xy } {

    set c [$editor toplevel].canvas
    $c delete scale

    # XXX a tk4.0 hack - perhaps it will go when tk4.0 gets stable ...
    set cx [expr int($cx)]
    set cy [expr int($cy)]

    set item [Tool::Find $c $cx $cy \
	      "NODE GROUP NETWORK TEXT REFERENCE STRIPCHART BARCHART LINK"]
    if {$item == ""} return
    set x1 [expr $cx - 5]
    set y1 [expr $cy - 5]
    set x2 [expr $cx + 5]
    set y2 [expr $cy + 5]
    set id [Tool::GetId $c $item]
    set type [$id type]
    switch $type {
	STRIPCHART -
	BARCHART {
	    Tool::AlterChart $editor $id $item $xx $xy $x1 $y1 $x2 $y2
	}
	NODE      { Tool::AlterNode      $editor $id $item $xx $xy }
	NETWORK   { Tool::AlterNetwork   $editor $id $item $xx $xy }
	GROUP     { Tool::AlterGroup     $editor $id $item $xx $xy }
	REFERENCE { Tool::AlterReference $editor $id $item $xx $xy }
	LINK	  { Tool::AlterLink	 $editor $id $item $xx $xy }
	TEXT      { Tool::AlterText      $editor $id $item $cx $cy }
    }
}

proc Tool::AlterChart { editor id item x y x1 y1 x2 y2 } {
    set c [$editor toplevel].canvas
    catch {destroy $c.popup}
    menu $c.popup -tearoff false

    Tool::MakePopup $c.popup $id 

    set chart [string tolower [$id type]]
    $c.popup add separator
    $c.popup add command -label "Scale $chart" \
	-command "Tool::AlterChartScale $editor $id $item $x $y; \
                      destroy $c.popup"
    if {[$id type] == "STRIPCHART"} {
	$c.popup add command -label "Jump $chart" \
	    -command "Tool::AlterChartJump $editor $id $item $x $y; \
                      destroy $c.popup"
    }
    $c.popup add command -label "Clear $chart" \
	-command "$id clear; destroy $c.popup"

    tk_popup $c.popup $x $y
}

proc Tool::AlterChartScale { editor id item x y } {
    set c [$editor toplevel].canvas
    $c delete scale
    catch {destroy $c.scale}

    set bb [$c bbox $item]
    set height [expr {[lindex $bb 3] - [lindex $bb 1]}]
    set px [expr {[lindex $bb 0]}]
    set py [lindex $bb 3]

    set max 100
    foreach val [$c itemcget $item -values] {
	set max [expr {($max < $val) ? int(ceil($val/100)*100) : $max}]
    }
    set old [$id scale]
    set max [expr {($max < $old) ? int(ceil($old/100)*100) : $max}]

    scale $c.scale -borderwidth 1 -sliderlength 5 -font fixed \
	-from 1 -to $max \
	-command "$id scale"

    $c.scale set [expr {round([$id scale])}]
    $c create window $px $py -window $c.scale \
	-tags "scale" -anchor se -height $height
}

proc Tool::AlterChartJump { editor id item x y } {
    set c [$editor toplevel].canvas
    $c delete scale
    catch {destroy $c.scale}

    set bb [$c bbox $item]
    set height [expr {[lindex $bb 3] - [lindex $bb 1]}]
    set px [expr {[lindex $bb 0]}]
    set py [lindex $bb 3]

    set max [$c bbox $item]
    set max [expr {[lindex $max 2] - [lindex $max 0] - 6}]
    set val [$c itemcget $item -jump]

    scale $c.scale -borderwidth 1 -sliderlength 5 -font fixed \
	-from 1 -to $max \
	-command "$id jump"

    $c.scale set $val
    $c create window $px $py -window $c.scale \
	-tags "scale" -anchor se -height $height
}

proc Tool::AlterNode { editor id item x y } {
    set c [$editor toplevel].canvas
    catch {destroy $c.popup}
    menu $c.popup -tearoff false

    Tool::MakePopup $c.popup $id

    tk_popup $c.popup $x $y
}

proc Tool::AlterLink { editor id item x y } {
    set c [$editor toplevel].canvas
    catch {destroy $c.popup}
    menu $c.popup -tearoff false

    Tool::MakePopup $c.popup $id

    tk_popup $c.popup $x $y
}

proc Tool::AlterGroup { editor id item x y } {
    set c [$editor toplevel].canvas
    catch {destroy $c.popup}
    menu $c.popup -tearoff false

    Tool::MakePopup $c.popup $id 

    $c.popup add separator
    if {[$id collapsed]} {
	$c.popup add command -label "Expand group" \
	    -command "destroy $c.popup; $id expand"
    } else {
	$c.popup add command -label "Collapse group" \
	    -command "destroy $c.popup; $id collapse"
    }

    tk_popup $c.popup $x $y
}

proc Tool::AlterReference { editor id item x y } {
    set c [$editor toplevel].canvas
    catch {destroy $c.popup}
    menu $c.popup -tearoff false

    Tool::MakePopup $c.popup $id 

    $c.popup add separator
    $c.popup add command -label "Open (this view)" \
	-command "destroy $c.popup; REFERENCE::open $id"
    $c.popup add command -label "Open (new view)" \
	-command "destroy $c.popup; REFERENCE::load $id"

    tk_popup $c.popup $x $y
}

proc Tool::AlterNetwork { editor id item x y } {
    set c [$editor toplevel].canvas
    catch {destroy $c.popup}
    menu $c.popup -tearoff false

    Tool::MakePopup $c.popup $id 

    tk_popup $c.popup $x $y
}

proc Tool::AlterText { editor id item x y } {
    set c [$editor toplevel].canvas
    $c bind $item <Return>         "$c insert $item insert \\n; break"
    $c bind $item <KeyPress>       "$c insert $item insert %A;  break"
    $c bind $item <Shift-KeyPress> "$c insert $item insert %A;  break"
    $c bind $item <BackSpace>      "Tool::TextBs $c $item; break"
    $c bind $item <Control-h>      "Tool::TextBs $c $item; break"
    $c bind $item <Control-a>      "$c icursor $item 0; break"
    $c bind $item <Control-e>      "$c icursor $item end;  break"
    $c bind $item <Delete>         "Tool::TextBs $c $item; break"
    # make sure that no toplevel bindings are invoked
    bind $c <KeyPress> "break"
    # save the new text when we leave the current focus
    bind $c <FocusOut>             "Tool::AlterTextDone $editor $id $item"
    $c icursor $item @$x,$y
    $c focus $item
    focus $c
}

proc Tool::AlterTextDone { editor id item } {
    set c [$editor toplevel].canvas
    bind $c <FocusOut> ""
    catch {
	$c bind $item <2> ""
	set txt [$c itemcget $item -text]
	$id text $txt
    }
}

##
## Implementation of the node tool which instantiates a new
## node object.
##

proc Tool::Node { editor } {

    Tool::Init $editor node

    set c [$editor toplevel].canvas
    
    $c configure -cursor plus

    bind $c <1> "Tool::NodeDone $editor \[$c canvasx %x\] \[$c canvasy %y\]"
}

proc Tool::NodeDone { editor x y } {

    set w [$editor toplevel]
    set c $w.canvas

    set node [NODE create]
    $node editor $editor
    $node canvas $c
    $node move $x $y
    $node icon  [$editor attribute nodeicon]
    $node font  [$editor attribute font]
    $node color [$editor attribute color]
    $node label name
}

##
## Implementation of the network tool.
##
 
proc Tool::Network { editor } {

    Tool::Init $editor network

    set c [$editor toplevel].canvas
    
    $c configure -cursor plus
    
    bind $c <1> "Tool::NetworkMark $editor \
                \[$c canvasx %x\] \[$c canvasy %y\]"
}

proc Tool::NetworkMark { editor x y } {
    global tkined_points
    set c [$editor toplevel].canvas
    set tkined_points "$x $y"
    bind $c <1> "Tool::NetworkDone $editor \
                \[$c canvasx %x\] \[$c canvasy %y\]"
    bind $c <Shift-Button-1> "Tool::NetworkAddMark $editor \
                \[$c canvasx %x\] \[$c canvasy %y\]"
    bind $c <Motion> "Tool::NetworkDrag $editor \
                \[$c canvasx %x\] \[$c canvasy %y\]"
    bind $c <Shift-Motion> "Tool::NetworkDrag $editor \
                \[$c canvasx %x\] \[$c canvasy %y\]"
}

proc Tool::NetworkDrag { editor x y } {
    global tkined_points
    if ![info exists tkined_points] return
    set c [$editor toplevel].canvas
    if {$tkined_points==""} return
    $c delete network
    set len [llength $tkined_points]
    incr len -1
    set lasty [lindex $tkined_points $len]
    incr len -1
    set lastx [lindex $tkined_points $len]
    set dx [expr {$x>$lastx ? $x-$lastx : $lastx-$x}]
    set dy [expr {$y>$lasty ? $y-$lasty : $lasty-$y}]
    if {$dx>$dy} {set y $lasty} else {set x $lastx}
    eval $c create line "$tkined_points $x $y" -fill black -tags "network"
}

proc Tool::NetworkAddMark { editor x y } {
    global tkined_points
    if {$tkined_points==""} return
    set c [$editor toplevel].canvas
    $c delete network
    set len [llength $tkined_points]
    incr len -1
    set lasty [lindex $tkined_points $len]
    incr len -1
    set lastx [lindex $tkined_points $len]
    set dx [expr {$x>$lastx ? $x-$lastx : $lastx-$x}]
    set dy [expr {$y>$lasty ? $y-$lasty : $lasty-$y}]
    if {$dx>$dy} {set y $lasty} else {set x $lastx}
    append tkined_points " $x $y"
    eval $c create line $tkined_points -fill black -tags "network"
}

proc Tool::NetworkDone { editor x y } {
    global tkined_points

    if {$tkined_points==""} return
    set w [$editor toplevel]
    set c $w.canvas
    $c delete network
    set len [llength $tkined_points]
    incr len -1
    set lasty [lindex $tkined_points $len]
    incr len -1
    set lastx [lindex $tkined_points $len]
    set dx [expr {$x>$lastx ? $x-$lastx : $lastx-$x}]
    set dy [expr {$y>$lasty ? $y-$lasty : $lasty-$y}]
    if {$dx>$dy} {set y $lasty} else {set x $lastx}
    append tkined_points " $x $y"

    set network [eval NETWORK create $tkined_points]
    $network editor $editor
    $network canvas $c
    $network icon  [$editor attribute network]
    $network font  [$editor attribute font]
    $network color [$editor attribute color]
    $network label name

    set tkined_points ""
    bind $c <1> "Tool::NetworkMark $editor \
                \[$c canvasx %x\] \[$c canvasy %y\]"
    bind $c <Motion> ""
    bind $c <Shift-Motion> ""
}

##
## Implementation of the link tool. It can be used to link two node
## objects or to link a node object and a network object.
##

proc Tool::Link { editor } {

    Tool::Init $editor link

    set c [$editor toplevel].canvas
    
    $c configure -cursor plus
    
    bind $c <1> "Tool::LinkMark $editor \
                \[$c canvasx %x\] \[$c canvasy %y\]"
}

proc Tool::LinkMark { editor x y } {
    global tkined_points
    set c [$editor toplevel].canvas
    set tkined_points ""

    set item  [Tool::Find $c $x $y "NODE NETWORK"]
    if {$item == ""} return

    set tkined_points "$x $y"
    bind $c <1> "Tool::LinkDone $editor \
                \[$c canvasx %x\] \[$c canvasy %y\]"
    bind $c <Shift-Button-1> "Tool::LinkAddMark $editor \
                \[$c canvasx %x\] \[$c canvasy %y\]"
    bind $c <Motion> "Tool::LinkDrag $editor \
                \[$c canvasx %x\] \[$c canvasy %y\]"
    bind $c <Shift-Motion> "Tool::LinkShiftDrag $editor \
                \[$c canvasx %x\] \[$c canvasy %y\]"
}

proc Tool::LinkDrag { editor x y } {
    global tkined_points
    if ![info exists tkined_points] return
    if {$tkined_points==""} return
    set c [$editor toplevel].canvas
    $c delete line
    eval $c create line "$tkined_points $x $y" -fill black -tags "line"
}

proc Tool::LinkShiftDrag { editor x y } {
    global tkined_points
    if ![info exists tkined_points] return
    if {$tkined_points==""} return
    set c [$editor toplevel].canvas
    set len [llength $tkined_points]
    if {$len>2} {
	incr len -1
	set lasty [lindex $tkined_points $len]
	incr len -1
	set lastx [lindex $tkined_points $len]
	set dx [expr {$x>$lastx ? $x-$lastx : $lastx-$x}]
	set dy [expr {$y>$lasty ? $y-$lasty : $lasty-$y}]
	if {$dx>$dy} {set y $lasty} else {set x $lastx}
    }
    Tool::LinkDrag $editor $x $y
}

proc Tool::LinkAddMark { editor x y } {
    global tkined_points
    if {$tkined_points==""} return
    set c [$editor toplevel].canvas
    set len [llength $tkined_points]
    if {$len>2} {
	incr len -1
	set lasty [lindex $tkined_points $len]
	incr len -1
	set lastx [lindex $tkined_points $len]
	set dx [expr {$x>$lastx ? $x-$lastx : $lastx-$x}]
	set dy [expr {$y>$lasty ? $y-$lasty : $lasty-$y}]
	if {$dx>$dy} {set y $lasty} else {set x $lastx}
    }
    $c delete line
    append tkined_points " $x $y"
    eval $c create line $tkined_points -fill black -tags "line"
}

proc Tool::LinkDone { editor x y } {
    global tkined_points
    if {$tkined_points==""} return
    set c [$editor toplevel].canvas
    $c delete line

    set sx [lindex $tkined_points 0]
    set sy [lindex $tkined_points 1]
    set dst_item [Tool::Find $c $x  $y  "NODE NETWORK"]
    set src_item [Tool::Find $c $sx $sy "NODE NETWORK"]

    if {$dst_item != "" && $src_item != ""} {
	set dst_id   [Tool::GetId $c $dst_item]
	set dst_type [$dst_id type]
	append tkined_points " [$dst_id move]"
	set src_id   [Tool::GetId $c $src_item]
	set src_type [$src_id type]
	if {($dst_type=="NODE") || 
	    (($dst_type=="NETWORK") && ($src_type=="NODE"))} {
		set len [llength $tkined_points]
		incr len -3
		set link [eval LINK create $src_id $dst_id \
			  [lrange $tkined_points 2 $len]]
		$link editor $editor
		$link canvas $c
		$link color [$editor attribute color]
	    }
    }
    set tkined_points ""
    bind $c <1> "Tool::LinkMark $editor \
                \[$c canvasx %x\] \[$c canvasy %y\]"
    bind $c <Motion> ""
    bind $c <Shift-Motion> ""
}

##
## Create a new group object that does not have any members.
##

proc Tool::Group { editor } {

    Tool::Init $editor group

    set c [$editor toplevel].canvas

    $c configure -cursor plus

    bind $c <1> "Tool::GroupDone $editor \[$c canvasx %x\] \[$c canvasy %y\]"
}

proc Tool::GroupDone { editor x y } {

    set w [$editor toplevel]
    set c $w.canvas

    set group [GROUP create]
    $group editor $editor
    $group canvas $c
    $group move $x $y
    $group icon  [$editor attribute groupicon]
    $group font  [$editor attribute font]
    $group color [$editor attribute color]
    $group label name
}

##
## Create a new reference object.
##

proc Tool::Reference { editor } {

    Tool::Init $editor reference

    set c [$editor toplevel].canvas

    $c configure -cursor plus

    bind $c <1> \
	"Tool::ReferenceDone $editor \[$c canvasx %x\] \[$c canvasy %y\]"
}

proc Tool::ReferenceDone { editor x y } {

    set w [$editor toplevel]
    set c $w.canvas

    set reference [REFERENCE create]
    $reference editor $editor
    $reference canvas $c
    $reference move $x $y
    $reference icon  [$editor attribute referenceicon]
    $reference font  [$editor attribute font]
    $reference color [$editor attribute color]
    $reference label name
}
