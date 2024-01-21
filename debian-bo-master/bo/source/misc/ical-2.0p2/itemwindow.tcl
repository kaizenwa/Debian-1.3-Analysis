# Copyright (c) 1993 by Sanjay Ghemawat
##############################################################################
# ItemWindow
#
#	Displays item contents as canvas item.
#
# Description
# ===========
# An ItemWindow displays item contents and allows editing.  An item
# window can be "selected".  At most one item window is selected in
# the entire application at any point in time.  Item selections
# and unselections result in the firing of triggers.
#
#	trigger fire select
#
# The following user-level hooks are also executed:
#
#	item-select   <item> <date>
#	item-unselect <item> <date>
#
# Implementation Notes
# ====================
#
# A mapping from <toplevel,item,date> to ItemWindow objects is
# maintained.  This map is used to find the item window responsible
# for displaying an occurrence of a specific item.

# effects - Create ItemWindow in canvas.
class ItemWindow {canvas font item date} {
    set slot(canvas) $canvas
    set slot(item) $item
    set slot(date) $date
    set slot(x) -100
    set slot(y) -100
    set slot(clickx) 0
    set slot(clicky) 0
    set slot(icon) {}
    set slot(link) {}
    set slot(width) 1
    set slot(height) 1
    set slot(sel) 0

    $canvas create rectangle -100 -100 -101 -101\
	-fill [pref itemOverflowColor]\
	-stipple [pref itemOverflowStipple]\
	-width 1\
	-tags [list item $self back back.$self click.$self]

    $canvas create rectangle -100 -100 -101 -101\
	-fill [pref itemBg]\
	-width 0\
	-tags [list item $self rect.$self click.$self]

    $canvas create bitmap -100 -100\
	-anchor nw\
	-foreground [pref itemFg]\
	-background [pref itemBg]\
	-tags [list item $self icon.$self]

    $canvas create bitmap -100 -100\
	-anchor ne\
	-foreground [pref itemFg]\
	-background [pref itemBg]\
	-tags [list item $self link.$self]

    set slot(text) [$canvas create text -100 -100\
			-anchor nw\
			-fill [pref itemFg]\
			-font $font\
			-width 0\
			-text ""\
			-tags [list item $self text.$self click.$self]]

    $canvas bind click.$self <Button-1> [list $self click %x %y]
    $canvas bind click.$self <B1-Motion> [list $self select_to %x %y]
    $canvas bind icon.$self  <Button-1> [list $self toggle_done]
    $canvas bind link.$self  <Button-1> [list $self follow_link]
    $canvas bind click.$self <Double-Button-1> {ical_edit_item}

    global item_map
    set item_map([winfo toplevel $canvas],$item,$date) $self

    $self read
}

# effects - Destroy item
method ItemWindow destructor {} {
    # Clear selection if this is the selected item
    global item_map last_sel
    if ![string compare $last_sel $self] ical_unselect
    unset item_map([winfo toplevel $slot(canvas)],$slot(item),$slot(date))

    $slot(canvas) delete $self
}

# effects - Set item geometry
method ItemWindow geometry {x y w h} {
    set slot(x) $x
    set slot(y) $y
    set slot(width) $w
    set slot(height) $h

    $self place
}

# effects - Raise associated window
method ItemWindow raise {} {
    $slot(canvas) raise $self
}

method ItemWindow save {} {
    set item $slot(item)
    set new [lindex [$slot(canvas) itemconfig text.$self -text] 4]
    set old [$item text]

    if {[string compare $new $old] == 0} {return}

    $item text $new
}

method ItemWindow read {} {
    set slot(icon) {}
    set slot(link) {}
    if [$slot(item) todo]    {set slot(icon) todo_box}
    if [$slot(item) is_done] {set slot(icon) done_box}

    if ![catch {$slot(item) option Link}] {
	set slot(link) right_arrow
    }

    $slot(canvas) itemconfigure text.$self -text [$slot(item) text]
    $self place
}

method ItemWindow editable {} {
    # Check for read-only calendar
    set i $slot(item)
    set r 1
    catch {set r [cal readonly [$i calendar]]}
    if $r {return 0}

    # Check for repeating items
    set result [repeat_check [winfo toplevel $slot(canvas)] $i $slot(date)]
    if ![string compare $result cancel] {return 0}
    return 1
}

method ItemWindow insert {str} {
    if [$self editable] {
	$slot(canvas) insert text.$self insert $str
	$self save
    }
}

method ItemWindow select {} {
    set slot(sel) 1
    focus $slot(canvas)
    $slot(canvas) itemconfig text.$self -fill [pref itemSelectFg]
    $slot(canvas) itemconfig icon.$self -background [pref itemSelectBg]
    $slot(canvas) itemconfig link.$self -background [pref itemSelectBg]
    $slot(canvas) itemconfig rect.$self\
	-fill [pref itemSelectBg]\
	-width [pref itemSelectWidth]

    $slot(canvas) focus text.$self
    $self raise
}

method ItemWindow unselect {} {
    set slot(sel) 0
    focus [winfo toplevel $slot(canvas)]
    $slot(canvas) itemconfig text.$self -fill [pref itemFg]
    $slot(canvas) itemconfig icon.$self -background [pref itemBg]
    $slot(canvas) itemconfig link.$self -background [pref itemBg]
    $slot(canvas) itemconfig rect.$self\
	-fill [pref itemBg]\
	-width 0
    $slot(canvas) focus ""
}

method ItemWindow click {x y} {
    ical_select $slot(item) $slot(date)
    if !$slot(sel) return

    set x [expr int([$slot(canvas) canvasx $x])]
    set y [expr int([$slot(canvas) canvasy $y])]
    $slot(canvas) icursor text.$self @$x,$y
    $slot(canvas) select from text.$self @$x,$y

    set slot(clickx) $x
    set slot(clicky) $y
}

method ItemWindow select_to {x y} {
    set x [expr int([$slot(canvas) canvasx $x])]
    set y [expr int([$slot(canvas) canvasy $y])]

    if {(abs($x - $slot(clickx)) > 5) || (abs($y - $slot(clicky)) > 5)} {
	$slot(canvas) select to text.$self @$x,$y
    }
}

# effects - Return bounding box for specified item
method ItemWindow bbox {} {
    return [$slot(canvas) bbox $self]
}

# effects - Toggle done status of item
method ItemWindow toggle_done {} {
    ical_select $slot(item) $slot(date)
    if !$slot(sel) return
    ical_toggle_done
}

# effects - Follow item link
method ItemWindow follow_link {} {
    ical_select $slot(item) $slot(date)
    if !$slot(sel) return
    ical_follow_link
}

# Internal Operations

# effects - Just position the text and any icon correctly
method ItemWindow place_text_and_icon {} {
    if ![string compare $slot(icon) {}] {
	set iw 0
	$slot(canvas) coords icon.$self -100 -100
    } else {
	set iw 18
	$slot(canvas) coords icon.$self [expr $slot(x)+2] [expr $slot(y)+2]
    }
    $slot(canvas) itemconfig icon.$self -bitmap $slot(icon)

    if ![string compare $slot(link) {}] {
	set lw 0
	$slot(canvas) coords link.$self -100 -100
    } else {
	set lw 18
	set pos [expr $slot(x) + $slot(width)]
	$slot(canvas) coords link.$self [expr $pos - 2] [expr $slot(y)+2]
    }
    $slot(canvas) itemconfig link.$self -bitmap $slot(link)

    set tx [expr $slot(x) + $iw + 2]
    $slot(canvas) coords text.$self $tx [expr $slot(y)+1]
    $slot(canvas) itemconfig text.$self -width [expr $slot(width) - $iw - $lw]
}

# effects - Place window at appropriate position in canvas
method ItemWindow place {} {
    $self place_text_and_icon

    set x1 $slot(x)
    set y1 $slot(y)
    set x2 [expr $x1 + $slot(width)]
    set y2 [expr $y1 + $slot(height)]

    # Auto size item.  Check for text overflow
    set bbox [$slot(canvas) bbox text.$self]
    set yt [lindex $bbox 3]
    if {$yt > $y2} {set y2 $yt}

    $slot(canvas) coords rect.$self $x1 $y1 $x2 $y2
}

method ItemWindow canvas {} {
    return $slot(canvas)
}

method ItemWindow item {} {
    return $slot(item)
}

method ItemWindow date {} {
    return $slot(date)
}

##############################################################################
# ApptItemWindow
#
#	Displays appointment contents as canvas item.
#
# Description
# ===========
# An ApptItemWindow displays appt contents and allows editing.
# The ApptItemWindow is always maintained in a canvas.
#
# An ApptItemWindow is a subclass of ItemWindow that maintains
# the following extra slots --
#
#	move_callback
#		Execute {move_callback $item <y-coord>} when dragging
#		an itemwindow around.  <y-coord> is coordinate for top
#		of window (within parent canvas).
#		When drag is finished, {move_callback $item done} is called.
#
#	resize_callback
#		Execute {resize_callback $item <top> <bot>} when resizing
#		an itemwindow with the mouse.  <top>/<bot> are coordinates
#		for the top and bottom of the window (within parent canvas).
#		When resize is finished, {resize_callback $item done done}
#		is called.

# effects - Create ApptItemWindow in canvas.
subclass ApptItemWindow ItemWindow {canvas font item date m r} {
    super constructor $canvas $font $item $date

    set slot(move_callback) $m
    set slot(resize_callback) $r

    set slot(dragy) 0
    set slot(dragtop) 0
    set slot(dragbot) 0
    set slot(moving) 0
    set slot(resizing) 0
    set slot(minheight) 0

    $canvas bind $self <ButtonPress-2>		[list $self move_start %y]
    $canvas bind $self <B2-Motion>	 	[list $self move_continue %y]
    $canvas bind $self <ButtonRelease-2>	[list $self move_finish %y]

    $canvas bind $self <ButtonPress-3>		[list $self size_start %y]
    $canvas bind $self <B3-Motion>	 	[list $self size_continue %y]
    $canvas bind $self <ButtonRelease-3>	[list $self size_finish %y]
}

method ApptItemWindow save {} {
    set item $slot(item)
    set new [lindex [$slot(canvas) itemconfig text.$self -text] 4]
    set old [$item text]

    if {[string compare $new $old] == 0} {return}

    if ![cal option AllowOverflow] {
	# Prevent text from overflowing appointment
	if {[string length $new] > [string length $old]} {
	    set bbox [$slot(canvas) bbox text.$self]
	    if {[lindex $bbox 3] > ($slot(y) + $slot(height))} {
		# Refuse to enlarge text if it does not fit
		$self read
		return
	    }
	}
    }

    $item text $new
}

method ApptItemWindow unselect {} {
    set slot(moving) 0
    set slot(resizing) 0
    super unselect
}

# Internal Operations

# effects - Place window at appropriate position in canvas
method ApptItemWindow place {} {
    set text [$slot(item) text]
    $slot(canvas) itemconfig text.$self -text $text

    $self place_text_and_icon

    set x1 $slot(x)
    set y1 $slot(y)
    set x2 [expr $x1 + $slot(width)]
    set y2 [expr $y1 + $slot(height)]

    $slot(canvas) coords rect.$self $x1 $y1 $x2 $y2

    if $slot(sel) {
	# Auto size item to avoid text overflow
	set bbox [$slot(canvas) bbox text.$self]
	set yt [lindex $bbox 3]
	if {$yt > $y2} {set y2 $yt}
    } else {
	# Truncate text to fit inside boundary
	set i [$slot(canvas) index text.$self @0,$y2]
	set text2 [string range $text 0 [expr $i-1]]
	if [string compare $text $text2] {
	    set text2 [string range $text 0 [expr $i-4]]...
	    $slot(canvas) itemconfigure text.$self -text $text2
	}
    }

    $slot(canvas) coords back.$self $x1 $y1 $x2 $y2
}

method ApptItemWindow move_start {y} {
    ical_select $slot(item) $slot(date)
    if [cal readonly [$slot(item) calendar]] {return}

    set r [repeat_check [winfo toplevel $slot(canvas)] $slot(item) $slot(date)]
    if {$r != "unnecessary"} return

    set slot(moving) 1
    set slot(dragy) [expr $y-$slot(y)]
}

method ApptItemWindow move_continue {y} {
    if !$slot(moving) {return}

    set new_y [expr $y-$slot(dragy)]
    eval $slot(move_callback) $slot(item) $new_y
}

method ApptItemWindow move_finish {y} {
    if !$slot(moving) {return}
    $self move_continue $y
    set slot(moving) 0
    eval $slot(move_callback) $slot(item) done
}

method ApptItemWindow size_start {y} {
    ical_select $slot(item) $slot(date)
    if [cal readonly [$slot(item) calendar]] {return}

    set r [repeat_check [winfo toplevel $slot(canvas)] $slot(item) $slot(date)]
    if {$r != "unnecessary"} return

    set slot(resizing) 1
    set slot(dragtop) 0
    set slot(dragbot) 0
    set slot(minheight) 1

    if ![cal option AllowOverflow] {
	set bbox [$slot(canvas) bbox text.$self]
	set slot(minheight) [expr [lindex $bbox 3] - [lindex $bbox 1]]
    }
}

method ApptItemWindow size_continue {y} {
    if !$slot(resizing) {return}

    set new_y [$slot(canvas) canvasy $y]
    set top $slot(y)
    set bot [expr $slot(y)+$slot(height)]

    if {$new_y < $top} {
	set slot(dragtop) 1
	set slot(dragbot) 0
	eval $slot(resize_callback) $slot(item) $new_y $bot
	return
    }

    if {$new_y > $bot} {
	set slot(dragtop) 0
	set slot(dragbot) 1
	eval $slot(resize_callback) $slot(item) $top $new_y
	return
    }

    if $slot(dragtop) {
	set top $new_y
    }

    if $slot(dragbot) {
	set bot $new_y
    }

    if {($bot - $top) >= $slot(minheight)} {
	eval $slot(resize_callback) $slot(item) $top $bot
    }
}

method ApptItemWindow size_finish {y} {
    if !$slot(resizing) {return}
    $self size_continue $y
    set slot(resizing) 0
    eval $slot(resize_callback) $slot(item) done done
}

#### Selection support ####

set last_sel {}
set last_focus {}
set dv_state(state:remind)	-1
set dv_state(state:hilite)	""
set dv_state(state:todo)	0

proc ical_focus_on {w} {
    global last_focus
    set last_focus [winfo toplevel $w]
}

proc ical_focus {} {
    global last_focus
    return $last_focus
}

proc ical_find_selection {} {
    global last_sel last_focus
    if [catch {set can [$last_sel canvas]}] {error "no selection"}
    if [string compare [winfo toplevel $can] [ical_focus]] {
	# Current focus is not on window with selected item
	error "no selection"
    }

    # Current focus is on window with selected item
    return [$last_sel item]
}

proc ical_select {item date} {
    global item_map last_focus dv_state
    if ![info exists item_map($last_focus,$item,$date)] return
    set sel $item_map($last_focus,$item,$date)

    # Remove any previous selection and then make new selection
    global last_sel
    if [string compare $sel $last_sel] {
	# Remove last selection (if any)
	catch {
	    $last_sel unselect
	    run-hook item-unselect [$last_sel item] [$last_sel date]
	}

	# Create new selection
	set last_sel $sel
	$sel select
	set dv_state(state:remind)	[$item earlywarning]
	set dv_state(state:hilite)	[$item hilite]
	set dv_state(state:todo)	[$item todo]
	trigger fire select
	run-hook item-select $item $date
    }
}

proc ical_unselect {} {
    global last_sel dv_state
    if ![string compare $last_sel {}] return

    # Clear trace of current selection
    set sel $last_sel
    set last_sel {}

    # Now undo the selection
    catch {
	catch {set dv_state(state:remind) -1}
	catch {set dv_state(state:hilite) ""}
	catch {set dv_state(state:todo)   0}

	$sel unselect
	trigger fire select
	run-hook item-unselect [$sel item] [$sel date]
    }
}

#### Editing Support ####

method ItemWindow mvcursor {n} {
    puts stderr "mvcursor $n"
}

method ItemWindow mvline {n} {
    puts stderr "mvline $n"
}

method ItemWindow mvword {n} {
    puts stderr "mvword $n"
}

method ItemWindow mvbob {} {}
method ItemWindow mveob {} {}
method ItemWindow deletekey {} {}

proc make_itemwindow_bindings {tag} {
    tkCanvasBindings $tag

    # Disable the following bindings to let the general keymap handle them
    bind $tag <Escape> {}
    bind $tag <Tab>    {}

    # Special binding for key press
    bind $tag <KeyPress> {
	if {[string compare %A {}] && [$last_sel editable]} {
	    tkCanvasInsert %W %A
	    $last_sel save
	}
    }

    # Fix bindings so that items are saved when necessary
    foreach key {
	<Control-i>
	<Return>
	<Delete>
	<BackSpace>
	<Control-d>
	<Control-k>
	<Control-o>
	<Control-t>
	<Meta-d>
	<Meta-BackSpace>
	<Control-w>
	<Control-y>
    } {
	itemwindow_fix_binding $tag $key
    }
}

# Augment a generic canvas binding so that it works for items
proc itemwindow_fix_binding {tag key} {
    set oldbinding [bind $tag $key]
    if [string compare $oldbinding {}] {
	# Old binding exists
	regsub -all -- %old {
	    if [$last_sel editable] {
		%old
		$last_sel save
	    }
	} $oldbinding newbinding
	bind $tag $key $newbinding
    }
}
