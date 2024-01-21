#
# GroupSketch Conference Application
# ----------------------------------
#
# Here is a generic whiteboard like application, allowing the user to do
# freehand drawing.  The drawings are kept as lines
# on a canvas widget.  Gesturing is supported through the use of multiple
# cursors on all displays.
# B1: create a contiguous line
# B2: select and move an object. If you select on an intersection of lines,
#     all intersected lines will move
# B3: erase any line the cursor touches

# Note on globals:
# virtual_object_array: an array indexed by virtual object ids, 
#                       each element contains the canvas' id for that object

# Still to do:
# -add a menu that will let contain the following items:
#  each item indicates how a stroke" ie lines created by a B1 press + motions
#  are handled
#   segments: treats a stroke as a bunch of independent line segments with a
#             common tag
#   ragged stroke:   on B1 release, all lines are coalesced into a single multi-line
#             (doing this means we have to save the coordinates
#   smooth stroke:  stroke is smoothed 
#   patterns: series of bitmap patterns which are used to draw the line
#
# -raise the cursor every time it is moved to guarantee it remains on top

# -add a bounding box function so that all lines within a rubber-band
#  bounding box are moved
# -add a pallete for selecting, drawing, erasing (as in MacDraw) instead of
#  using dedicated point presses...
#
# -because of the way lines are created, I expect 
#  wierd things will happen if someone moves an object as it is being created! 
#  ie I suspect the part created will separate from the other parts
#  do anything about this???

# Probably should clean up the code a bit, I thing there are a few variables
# that are unused or not necessary; hangovers from previous iterations.


# To ensure virtual_object_array is an array with no elements.
set virtual_object_array(5) 4
unset virtual_object_array(5)

# ----------------------------------------------------------------
# Selection management.
# ----------------------------------------------------------------

# 
# A list of strokes in the current selection.
# 
set current_object ""

# 
# new_strokes is a list of strokes to add to the current selection.
# 
proc SelectStrokes {canvas new_strokes} {
    global current_object
    foreach new_stroke $new_strokes {
        if {[lsearch $current_object $new_stroke] == -1} {
            lappend current_object $new_stroke
        }
    }
    foreach stroke $new_strokes {
        set start_coords [StrokeStart $canvas $stroke]
        set end_coords [StrokeEnd $canvas $stroke]
        $canvas create rectangle [expr [lindex $start_coords 0]-5] [expr [lindex $start_coords 1]-5] [expr [lindex $start_coords 0]+5] [expr [lindex $start_coords 1]+5] -tag ${stroke}_start -fill black
        $canvas create rectangle [expr [lindex $end_coords 0]-5] [expr [lindex $end_coords 1]-5] [expr [lindex $end_coords 0]+5] [expr [lindex $end_coords 1]+5] -tag ${stroke}_end -fill black
    }
}


# 
# Sets the current selection to the empty set and removes all the
# handles.
# 
proc UnselectAll {canvas} {
    global current_object
    foreach stroke $current_object {
        $canvas delete ${stroke}_start
        $canvas delete ${stroke}_end
    }
    set current_object {}
}


proc UnselectStroke {canvas stroke} {
    global current_object
    $canvas delete ${stroke}_start
    $canvas delete ${stroke}_end
    set Pos [lsearch $current_object $stroke]
    set $current_object [lreplace $current_object $Pos $Pos]
}


# 
# Re-draws all the handles on canvas.  Must be called whenever a
# selected object moves.
# 
proc UpdateHandles {canvas} {
    global current_object
    foreach stroke $current_object {
        set x1 [expr [lindex [StrokeStart $canvas $stroke] 0]-5]
        set y1 [expr [lindex [StrokeStart $canvas $stroke] 1]-5]
        set x2 [expr $x1+10]
        set y2 [expr $y1+10]
        $canvas coords ${stroke}_start $x1 $y1 $x2 $y2
        set x1 [expr [lindex [StrokeEnd $canvas $stroke] 0]-5]
        set y1 [expr [lindex [StrokeEnd $canvas $stroke] 1]-5]
        set x2 [expr $x1+10]
        set y2 [expr $y1+10]
        $canvas coords ${stroke}_end $x1 $y1 $x2 $y2
    }
}


# ----------------------------------------------------------------
# Bindings for particular objects
# ----------------------------------------------------------------

proc bindingsSketch {canvas} {

    bind $canvas <ButtonPress-1> {
        set lastX %x; set lastY %y; 
        UnselectAll $canvas
        SelectStrokes $canvas [lineNew $canvas %x %y $color $width]
    }

    bind $canvas <B1-Motion> {
	lineAddStroke $canvas $current_object %x %y $color $width
    }

    bind $canvas <ButtonRelease-1> {
        UpdateHandles $canvas
    }


    bind $canvas <ButtonPress-2> {
        UnselectAll $canvas
        SelectStrokes $canvas [virtualObjectTouched $canvas %x %y]
	objectStartDrag $canvas %x %y
    }

    bind $canvas <Shift-ButtonPress-2> {
        SelectStrokes $canvas [virtualObjectTouched $canvas %x %y]
        objectStartDrag $canvas %x %y
    }
        
    bind $canvas <Any-B2-Motion> {
	objectDrag $canvas $current_object %x %y;
    }

    bind $canvas <ButtonPress-3> {
	objectDelete $canvas %x %y; 
    }

    bind $canvas <B3-Motion> {
	objectDelete $canvas %x %y; 
    }
}

#======================================
# Finding Objects

# return the tag of the objects immediately under the cursor
proc virtualObjectTouched {c x y} {
    set cX [$c canvasx $x]
    set cY [$c canvasy $y]
    set canvas_object_ids [$c find overlapping $cX $cY $cX $cY]
    set result ""
    foreach canvas_object_id $canvas_object_ids {
	set tags [$c gettags $canvas_object_id]
	foreach tag $tags {
	    if { [lsearch $result $tag] == -1 && $tag != "current" }  {
		lappend result $tag 
	    }
	}
    }
    return $result
}


#======================================
# Creating and modifying lines

# Get ready to create a new line. Assign it a unique virtual object_id
proc lineNew {canvas x y color width} {

    set cX1 [$canvas canvasx $x]
    set cY1 [$canvas canvasy $y]
    
    set virtual_object_id [getUniqueId]
    gk_toAll lineDoNew $canvas $virtual_object_id $cX1 $cY1 $color $width
    return $virtual_object_id
}

# Actually create a new line. Save the object in the object array indexed
# by the object_id, and give the line a tag (the $virtual_object_id)
proc lineDoNew {canvas virtual_object_id x y color width} {
    global virtual_object_array
    set virtual_object_array($virtual_object_id) \
	[$canvas create line $x $y $x $y \
	 -fill $color -width $width -tag $virtual_object_id]
}

# Add a line to the stroke
proc lineAddStroke {canvas virtual_object_id x y color width} {
    global lastX lastY 
    set cX1 [$canvas canvasx $lastX]
    set cY1 [$canvas canvasy $lastY]
    set cX2 [$canvas canvasx $x]
    set cY2 [$canvas canvasy $y]
    gk_toAll lineDoAddStroke $canvas $virtual_object_id $cX1 $cY1 $cX2 $cY2 $color $width
    set lastX $x
    set lastY $y
}

# Add to the current "stroke". This is actually just a line segment with
# a tag that is common to the other lines created with this line press.
proc lineDoAddStroke {canvas virtual_object_id x1 y1 x2 y2 color width} {
    global virtual_object_array
    if [info exists virtual_object_array($virtual_object_id)] {
	$canvas create line $x1 $y1 $x2 $y2 \
	    -fill $color -width $width -tag $virtual_object_id
    }
}


#======================================
# Dragging

proc objectStartDrag {c x y} {
    global lastX lastY
    set lastX $x
    set lastY $y
}

proc objectDrag {c virtual_object_id x y} {
    global lastX lastY
    if {$virtual_object_id == ""} {return}

    set diffX [expr $x-$lastX]
    set diffY [expr $y-$lastY]

    gk_toAll objectDoDrag $c $virtual_object_id $diffX $diffY

    set lastX $x
    set lastY $y
}

proc objectDoDrag {canvas virtual_object_id diffX diffY} {
    foreach tag $virtual_object_id {
	$canvas move $tag $diffX $diffY
    }
    UpdateHandles $canvas
    update idletasks

#    update
}

# Delete the objects(s) under the cursor
proc objectDelete {canvas x y} {
    set virtual_object_ids [virtualObjectTouched $canvas $x $y]
    if {$virtual_object_ids == ""} {return}

    gk_toAll objectDoDelete $canvas $virtual_object_ids
}

# Actually delete the object from the canvas and remove it from the object list
proc objectDoDelete {canvas virtual_object_ids} {global virtual_object_array
    global current_object
    foreach tag $virtual_object_ids {
        if {[lsearch $current_object $tag] >= 0} {
            UnselectStroke $canvas $tag
        }
	$canvas delete $tag
	unset virtual_object_array($tag)
    }
}

#======================================
# Menu construction

# Color menu
proc menuColor {} {
    global color

    .menu add colors 1 \
	-text Colors 

    set colors {white yellow red green blue black}
    foreach item $colors {
	.menu itemcommand 1 add radiobutton \
	    -label $item \
	    -background $item \
	    -activebackground $item \
	    -variable color
    }
}

# Width menu
proc menuWidth {} {
    global width

    .menu add width 2 \
	-text Width \
        -width 10 \
	-underline 0

    set widths {1 2 3 4 6 8 10}
    foreach item $widths {
	.menu itemcommand 2 add radiobutton \
	    -label $item \
	    -variable width
    }
}

#======================================
# Utilities

proc getUniqueId {} {
    global _uniqueId
    if {[info exists _uniqueId]} {
	incr _uniqueId
    } else {
	set _uniqueId 0
    }
    set uid [users local.usernum]
    set seperator "ID"
    return [concat $seperator$uid$seperator$_uniqueId]
}


# 
# Returns the first point of stroke on canvas.
# 
proc StrokeStart {canvas stroke} {
    set line_ids [$canvas find withtag $stroke]
    set Ans [lrange [$canvas coords [min $line_ids]] 0 1]
    return $Ans
}


# 
# Returns the last point of stroke on canvas.
# 
proc StrokeEnd {canvas stroke} {
    set line_ids [$canvas find withtag $stroke]
    set Ans [lrange [$canvas coords [max $line_ids]] 2 3]
    return $Ans
}


# 
# Returns the minimum number in the set.  Set must be non-empty and
# contain only numbers.
# 
proc min set {
    set ans [lindex $set 0]
    foreach num $set {
        if {$ans > $num} {set ans $num}
    }
    return $ans
}


# 
# Returns the maximum number in the set.  Set must be non-empty and
# contain only numbers.
# 
proc max set {
    set ans [lindex $set 0]
    foreach num $set {
        if {$ans < $num} {set ans $num}
    }
    return $ans
}


#======================================
# Top Level Program

gk_initConf $argv

gk_defaultMenu .menu

menuColor
menuWidth

set color blue
set width 4

set canvas .c

canvas $canvas -scrollregion {0c 0c 50c 20c}
scrollbar .s1 -command "$canvas yview" -relief sunk
scrollbar .s2 -orient horiz -command "$canvas xview" -relief sunk
pack append . \
    .menu {top fillx} \
    .s2 {bottom fillx} \
    .s1 {right filly}  \
    $canvas {expand fill}
$canvas config -xscrollcommand ".s2 set" -yscrollcommand ".s1 set"


# FILE STUFF
# Add clear, save, and get buttons.
.menu itemcommand 0 insert 1 command -label "Open" -command "Open $canvas" -underline 0
.menu itemcommand 0 insert 2 command -label "Save" -command "Save $canvas" -underline 0
.menu itemcommand 0 insert 3 command -label "Clear" -command "Clear $canvas" -underline 0
.menu itemcommand 0 insert 4 separator

# Clear canvas
proc Clear {canvas} {
    gk_toAll UnselectAll $canvas
    foreach i [$canvas find all] {
	gk_toAll $canvas delete [$canvas gettags $i]
    }
}

# Save canvas in file as specified in FSBox.
proc Save {canvas} {
    set fileName [FSBox]
    if {$fileName == ""} {return}
    set saveFile [open $fileName w+]

    global virtual_object_array
    foreach stroke [array name virtual_object_array] {
        foreach line [selectionSort [$canvas find withtag $stroke]] {
    	    set fill [lindex [$canvas itemconfig $line -fill] 4]
	    set width [lindex [$canvas itemconfig $line -width] 4]
	    set mytag [$canvas gettags $line]
	    set mycoords [$canvas coords $line]
	    set x1 [lindex $mycoords 0]
	    set y1 [lindex $mycoords 1]
	    set x2 [lindex $mycoords 2]
	    set y2 [lindex $mycoords 3]
	    puts $saveFile "\$canvas create line $x1 $y1 $x2 $y2 -fill $fill -width $width -tag $mytag"
        }
    }
    close $saveFile
}


proc selectionSort list {
    set result {}
    while {[llength $list] > 0} {
        set Min [min $list]
        Insert result $Min
        Remove list $Min
    }
    return $result
}


# Get canvas from file specified in FSBox.
proc Open {canvas} {
    set fileName [FSBox]
    if {$fileName == ""} {return}
    Clear $canvas
    gk_toAll source $fileName
}

# lastX and lastY are the last X & Y cursor positions
set lastX 0
set lastY 0
set cursors(0) ""

# Always remember the last cursor position
bind $canvas <1> "global lastX lastY; set lastX %x; set lastY %y"

# Set up the bindings for manipulating the objects on the display
bindingsSketch $canvas

# USE REMOTE CURSORS
gk_initializeTelepointers
gk_specializeWidgetTreeTelepointer .c

gk_bind updateEntrant "updateEntrant $canvas %U"

# Update entrants on what's been goin' on.
proc updateEntrant {canvas usernum} {
    global virtual_object_array
    foreach stroke [array names virtual_object_array] {
        foreach line [selectionSort [$canvas find withtag $stroke]] {
    	    set fill [lindex [$canvas itemconfig $line -fill] 4]
	    set width [lindex [$canvas itemconfig $line -width] 4]
	    set mytag [$canvas gettags $line]
	    set mycoords [$canvas coords $line]
	    set x1 [lindex $mycoords 0]
	    set y1 [lindex $mycoords 1]
	    set x2 [lindex $mycoords 2]
	    set y2 [lindex $mycoords 3]
	    gk_toUserNum $usernum $canvas create line $x1 $y1 $x2 $y2 \
	      -fill $fill -width $width -tag $mytag
        }
    }
}

wm title . "GroupSketch"
wm maxsize . 1000 1000















