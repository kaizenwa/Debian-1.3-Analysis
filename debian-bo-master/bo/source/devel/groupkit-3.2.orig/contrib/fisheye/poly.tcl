# Creates the widget for adjusting your own fisheye controls. Ugly.

set g(points) {{30 10} {30 20} {30 30} {30 40} {30 50} {30 60} {30 70} {10 70} {10 10}}

set g(color,inactive) SkyBlue2
set g(color,active) red
set g(lastX) 0
set g(lastY) 0

set g(xbaseline) 40
set g(ybaseline) 10
set g(xgap) 20
set g(ygap) 15
set g(fontscale) 2
set g(xgridcoords) {}
set g(fontlist) [lsort  -integer {60 50 40 30 24 20 18 16 14 12 10 8 6 2}]
set g(extent) 21

set g(top) .top1
set g(label) $g(top).l


# Window and Widget Creation
#===========================

# The Top Level Window
#----------------------
proc MakeTopLevel {} {
    global g fish
    set top $g(top)
    if {[winfo exists $top]} {wm deiconify $top; return}

    # Configure the top level window
    toplevel $top
    wm title $top "Fisheye Controls"
    wm iconname $top "Fisheye"
    wm protocol $top WM_DELETE_WINDOW "wm withdraw $top"

    set mb $top.mb
    set m $top.mb.m

    menubutton $mb -menu $mb.m -indicator yes -relief ridge -bd 2
    SetLocalMenuLabel $mb
    menu $m 
    set pairlist { {1 Invisible} {2 Tiny} {6 Small} {8 "Just readable"}}
    foreach pair $pairlist {
	$m add radio \
		-label [lindex $pair 1] \
		-value [lindex $pair 0] \
		-variable fish(default_fontsize) \
		-command "UpdateDefaultFontSize $mb" 
    }
    pack $mb

    # This label will display feedback to the user
    label $top.l 
    Relabel [lindex $g(fontlist) 0] 0
    pack $top.l
    canvas $top.c -bd 3 -relief ridge 

    # Make the grid and the polygon, getting the canvas size as a side effect
    set width [MkGrid $top.c $g(fontlist)]
    set height [mkHandledPolygon $top.c [MkPolygonCoords $g(extent)]]

    # Set the canvas size, and pack it in
    $top.c configure -width [expr $width + $g(xgap)] -height [expr $height + $g(ygap)]
    pack $top.c 

    # Add a done button
    frame $top.fb
    pack $top.fb
    button $top.fb.d -text Done -command "wm withdraw $top"
    pack $top.fb.d -side left

}

# The Grid
#---------

# Make the grid representing the center line and the font lines
# Return the rightmost x value of the grid
proc MkGrid {c grid} {
    global g

    set y1  [expr $g(ybaseline) - 5]
    set y2  700
    set x1 $g(xbaseline)
    foreach increment $grid {
	set line [$c create line $x1 $y1 $x1 $y2 -width 1 -fill grey75] 
	$c addtag font$increment withtag $line
	set g(xgridcoords) [lappend g(xgridcoords) $x1]
	set rightmost_x $x1
	set x1 [expr $g(fontscale) * $increment + $g(xbaseline)]

    }
    set x1 $g(xbaseline)
    set y1 [expr $g(ybaseline) +  ($g(extent) / 2 * $g(ygap))]
    set line [$c create line $x1 $y1 $rightmost_x $y1 -width 1 -fill grey75]
    return $rightmost_x
}

# The Handled Polygon
#--------------------

# First Calculate the coordinates that we will use to create the polygon
proc MkPolygonCoords {scale} {
    global g
    # Coords for Handle
    for {set i 0} {$i < $scale} {incr i} {
	lappend coords [list $g(xbaseline) [expr $i * $g(ygap) + $g(ybaseline)]]  
    }
    lappend coords [list [expr $g(xbaseline) - $g(xgap)]  [expr ($i-1) * $g(ygap) + $g(ybaseline)]]  
    lappend coords [list [expr $g(xbaseline) - $g(xgap)]  [expr $g(ybaseline)]]  
    return $coords
}

# Make the Polygon and its Handles. Return the lowest Y point on the polygon
proc mkHandledPolygon {c coords} {
    global g
    # Create the polygon, and make it the lowest object in the canvas
    set poly [eval $c create polygon [join $coords]  -fill grey10]
    $c lower $poly

    # We don't want handles on the last two x,y coordinate pairs, so remove them
    set l [llength $coords]
    set coords [lreplace $coords [expr $l - 2] [expr $l - 1]]

    set idx 0
    foreach point $coords {
	set coords_handle [HandleCoordsFromCenter $point]
	set handle [eval $c create oval \
		$coords_handle -fill $g(color,inactive) ]
	$c raise $handle 
	$c addtag handle withtag $handle
	$c addtag index.$idx withtag $handle
	$c bind handle <Any-Enter> "$c itemconfigure current -fill $g(color,active)"
        $c bind handle <Any-Leave> "$c itemconfigure current -fill $g(color,inactive)"
        $c bind handle <1> "HandleDown $c %x %y; ShowInfo $c %x %y"
        $c bind handle <ButtonRelease-1> "$c dtag selected"
        $c bind handle <B1-Motion> "HandleMove $c %x %y  $poly"
        bind $c <B1-Motion> "ShowInfo $c %x %y"
	incr idx
    }
    return [GetY $point]
}

# Binding Callbacks
#==================

# Handles 
#---------

# The handle has been selected
proc HandleDown {c x y} {
    global g
    $c dtag selected
    $c addtag selected withtag current
    $c raise current
    set g(lastX) $x
    set g(lastY) $y
}

# The handle has been moved
proc HandleMove {c x y polygon}  {
    global g
    set x [SetXToGrid $x]
    if {[expr $x-$g(lastX)] == 0} {return}  #Nothings changed
    $c move selected [expr $x-$g(lastX)] 0
    set idx [GetIndexFromTags [$c gettags current]]
    if {$idx == -1} {puts Oops!; return}
    set coords [RecalculatePolygonCoords [$c coords $polygon] $x $idx]
    eval $c coords $polygon $coords
    set g(lastX) $x
    RepositionHandles $c $polygon $coords
    update idletasks
    UpdateFisheyeView $c
}

# Whenever the handle moves, we have to calculate the new position
# of the polygon
proc  RecalculatePolygonCoords {coords new_point idx } {
    set x [GetX $new_point] 
    set y [GetY $new_point] 

    set length [llength $coords]
    set lastx [expr $length - 8]

    # Make it symmetrical
    if {$idx > [expr ($lastx + 1) / 2] } {
	set idx [expr $lastx - $idx]
    }   
    # Calculate new coordinates when a point is moved
    for {set i 0} {$i <= $idx} {incr i 2} {
	if {[lindex $coords $i] > $x} {
	    set coords [lreplace $coords $i $i $x ]
	}
    }

    for {set i $idx} {$i <= [expr $lastx - $idx]} {incr i 2} {
	if {[lindex $coords $i] < $x} {
	    set coords [lreplace $coords $i $i $x ]
	}
    }
    for {set i [expr $lastx - $idx]} {$i <= $lastx} {incr i 2} {
	if {[lindex $coords $i] > $x} {
	    set coords [lreplace $coords $i $i $x ]
	}
    }
    set coords [lreplace $coords [expr $length - 2] [expr $length - 1] ]
    return $coords
}

# When a movement occurs, display the current font and line offset
proc ShowInfo {c x y} {
    global g

    set x1 [lindex $g(xgridcoords) 0]
    if {$x <  $x1} {
	$g(label) configure -text [lindex $g(fontlist) 0]
    }
    set newidx 0
    set max [llength $g(xgridcoords)]
    for {set idx 1} {$idx < $max} {incr idx} {
	set x2 [lindex $g(xgridcoords) $idx]
	set halfway [expr $x1 + (($x2 -$x1)/2)]
	if {$x >  $halfway} {set newidx $idx}
	set x1 $x2
    }
    set sub_extent [GetIndexFromTags [$c gettags current]]
    set sub_extent [expr $sub_extent / 2]
    set midpoint [expr $g(extent) / 2]
    set sub_extent [expr abs ($midpoint - $sub_extent)] 	
    Relabel [lindex $g(fontlist) $newidx] $sub_extent
}


#================
proc SetXToGrid {x} {
    global g

    set x1 [lindex $g(xgridcoords) 0]
    if {$x <  $x1} {set x $x1; return $x}

    set newx $x1
    set max [llength $g(xgridcoords)]
    for {set idx 1} {$idx < $max} {incr idx} {
	set x2 [lindex $g(xgridcoords) $idx]
	set halfway [expr $x1 + (($x2 -$x1)/2)]
	if {$x >  $halfway} {set newx $x2}
	set x1 $x2
    }
    return $newx
}

# Update the fisheye view from the current polygon state
# ======================================================

# These routines are really grotty

# First Query each handle and figure out what font and line its on
proc UpdateFisheyeView {c} {
    ClearFontExtentList
    foreach handle [$c find withtag handle] {
	set coords [concat [GetHandleCenter $c $handle]]
	set coords [concat $coords $coords]
	set objects [eval $c find overlapping $coords]
	foreach object $objects {
	    set tags [$c gettags $object]
	    if {[string first "font" $tags] == 0} {
		set font [string range $tags 4 [string length $tags]]
		set line [GetIndexFromTags [$c gettags $handle]]
		AddInfo $font $line
		break
	    }
	}
    }
    ActuallyUpdateFisheyeView
}

# Clear the internal data structure
proc ClearFontExtentList {} {
    global g
    for {set i 1} {$i<= $g(extent)} {incr i} {
	set g($i) 0
    }
}

# Now, translate the internal data tructure to one that can 
# be used by the fisheyeviewer, and set it.
proc ActuallyUpdateFisheyeView {} {
    global g fish
    # Find the midpoint
    set idx [expr $g(extent)/2 + 1] 
    foreach font [lsort -decreasing -integer $g(fontlist)] {
	set extent 0
	if {$idx != 0} {
	    while { $g($idx) == $font} {
		incr extent; incr idx -1
		if {$idx == 0}  break 
	    } 
	}
	lappend list [list $font $extent]
    }

    # NOTE! THIS ACTUALLY UPDATES FISHEYE VIEWER
    set fish(mag_factor) $list
    CenterView $fish(text_widget) $fish(global_focus) true
}   

# Add the font/line information to the internal data structure
proc AddInfo {font line} {
    global g
    set line [expr $line / 2 + 1]
    set g($line) $font
}

#====================

proc RepositionHandles {c polygon coords} {
    set max [expr [llength $coords] - 6]
    for {set i 0} {$i <= $max} {incr i 2} {
	set x [lindex $coords $i]
	set y [lindex $coords [expr $i +1]]
	set coords_handle [HandleCoordsFromCenter [list $x $y]]
        set idx [expr $i / 2]
	eval $c coords index.$idx $coords_handle
    }
}

# Utilities for calcualtion
#===========================

# Given a center point, generate some coordinates for a handle
proc HandleCoordsFromCenter {point} {
    set offset 3
    set x1 [expr [GetX $point] - $offset]
    set x2 [expr [GetX $point] + $offset]
    set y1 [expr [GetY $point] - $offset]
    set y2 [expr [GetY $point] + $offset]
    return "$x1 $y1 $x2 $y2 "
}
# Given a handle, find its center point
proc GetHandleCenter {c tag} {
    set offset 3
    set coords [$c coords $tag]
    set x [expr [lindex $coords 0] + $offset]
    set y [expr [lindex $coords 1] + $offset]
    return [list $x $y]
}

# Return x or y from an {x y} list
proc GetX {point} { return [lindex $point 0] }
proc GetY {point} {return [lindex $point 1] }

# Given a tag list, find a tag that contains a "." and
# return the number after it
proc GetIndexFromTags {taglist} {
    foreach tag $taglist {
	set idx [string first  "." $tag]
	if {$idx == -1} continue
        return [expr 2 * [string range $tag [expr $idx + 1 ] [string length $tag]]]
    }
    return -1
}

# Insert feedback of size and extent in the label widget
proc Relabel {size extent } {
    global g
    $g(label) configure -text "Font Size: $size \nCenter +/- $extent"
}

proc SetLocalMenuLabel {m} {
   global fish 
   $m configure -text "Default Font Size: $fish(default_fontsize)"
   
}

proc UpdateDefaultFontSize {mb} {
    global fish
    SetLocalMenuLabel $mb; 
    $fish(text_widget) configure -font [FontName $fish(default_fontsize)]
} 
