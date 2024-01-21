# Copyright (c) 1993 by Sanjay Ghemawat
#
# Drag and Drop interface
#
# This one does not require any special compiled in support.  It uses
# "send" to communicate with other "wish" instances.
#
# \begin{verbatim}
#	drag start	<type> <x> <y>
#	drag continue	<x> <y>
#	drag finish	<x> <y> <data>
#
#	drag activate <type> <window> <drop cmd>
#		[-enter	 <cmd>]
#		[-leave	 <cmd>]
#		[-motion <cmd>]
#
#	drag deactivate <type> <window>
# \end{verbatim}

# We defer checking for drag targets with the "after" command.
# Each "after" command is tacked with a unique value of the "id"
# variable so that we can outdate unnecessary checks.

proc drag_initialize {} {
    global drag

    set drag(id) 0
    set drag(x)  -1
    set drag(y)  -1

    # Currently selected application and window.
    set drag(app) ""
    set drag(win) ""

    # Current list of active applications
    set drag(applist) ""

    # Type of current drag data
    set drag(type) ""

    # My application id
    set drag(myname) [winfo name .]
}

proc drag {cmd args} {
    # effects	Dispatch drag operation to the right procedure

    global drag
    if ![info exists drag] {drag_initialize}

    return [eval drag::$cmd $args]
}

proc drag::activate {type window dropcmd args} {
    # effects	Set-up "dropcmd" to be executed whenever object of
    #		type "type" is dropped on "window".  Extra "args"
    #		can be used to setup other commands to be executed
    #		when dragged data enters, leaves, or moves over
    #		"window".
    global drag

    # Parse options
    set len [llength $args]
    if {($len % 2) != 0} {error {invalid options to "drag activate"}}
    for {set i 0} {$i < $len} {incr i 2} {
	switch -exact -- [lindex $args $i] {
	    -enter	{set enter  [lindex $args [expr $i+1]]}
	    -leave	{set leave  [lindex $args [expr $i+1]]}
	    -motion	{set motion [lindex $args [expr $i+1]]}
	    default	{error {invalid option to "drag activate"}}
	}
    }

    # Set-up callbacks
    set drop $dropcmd
    foreach o {drop enter leave motion} {
	if [info exists $o] {set drag($window,$type,$o) [set $o]}
    }
}

proc drag::deactivate {type window} {
    # effects	Cancel any existing drag callbacks for specified "type"
    #		on "window".

    global drag
    foreach o {drop enter leave motion} {
	catch {unset drag($type,$window,$o)}
    }
}

proc drag::find {t x y} {
    # effects	Return drag target that contains "x", "y".
    #		Raise error if no such target.  This operation is
    #		typically called via "send" by other applications.

    global drag

    set w [winfo containing $x $y]
    if {($w != "") && [winfo ismapped $w]} {
	# Find closest ancestor that is a valid target
	if [info exists drag($w,$t,drop)] {return $w}

	while {[string compare $w "."]} {
	    if [info exists drag($w,$t,drop)] {return $w}
	    set w [winfo parent $w]
	}
    }
    error "no drag target"
}

proc drag::start {t x y} {
    # effects	Start dragging at "x","y".
    #		We accomplish this by moving "window" to the right spot
    #		and checking for drag targets.
    #
    #		This code is usually called by application code in
    #		response to an event that signals the initiation of a drag.

    global drag

    set drag(id)   0
    set drag(x)    -1
    set drag(y)    -1
    set drag(win)  ""
    set drag(app)  ""
    set drag(type) $t

    # Get list of apps that are responding correctly
    set applist ""
    foreach a [winfo interps] {
	# Ignore auxiliary processes
	if [string match *-bg* $a] {continue}

	catch {
	    if {($a != $drag(myname)) && [send $a {info procs drag}] != ""} {
		lappend applist $a
	    }
	}
    }
    set drag(applist) $applist

    drag::continue $x $y
}

proc drag::continue {x y} {
    # effects	Continue dragging to "x", "y".
    #
    #		This code is usually called by application code in response to
    #		a motion event while a drag is active.

    global drag

    set drag(x) $x
    set drag(y) $y

    after 30 drag::_check $drag(id)
}

proc drag::finish {x y data} {
    # effects	Finish dragging.  Return true iff drop was successfully sent
    #		to an application.
    #
    #		This code is usually called by application code in response to
    #		an event that signals the end of a drag.

    global drag

    set drag(x) $x
    set drag(y) $y
    drag::_check $drag(id)
    if {$drag(app) != ""} {
	catch {
	    send $drag(app) [list drag::_drop $drag(win) $drag(type)\
			     $x $y $data]
	}
	return 1
    }
    return 0
}

# Internal Operations

proc drag::_check {id} {
    global drag

    # Check for outdated notify
    if {$id != $drag(id)} return

    # Place a mark to warn outdated notifies
    incr drag(id)

    # Look for target in my application
    if ![catch {set w [drag::find $drag(type) $drag(x) $drag(y)]}] {
	drag::_inside $drag(myname) $w
	return
    }

    foreach app $drag(applist) {
	if ![catch {
	    set w [send $app [list drag::find $drag(type) $drag(x) $drag(y)]]
	}] {
	    drag::_inside $app $w
	    return
	}
    }

    if {$drag(app) != ""} {
	drag::_left
	set drag(app) ""
	set drag(win) ""
    }
}

proc drag::_inside {app win} {
    global drag

    if {($drag(app) != $app) || ($drag(win) != $win)} {
	if {$drag(app) != ""} {drag::_left}

	set drag(app) $app
	set drag(win) $win
	catch {
	    send $drag(app) [list drag::_enter $drag(win) $drag(type)\
			     $drag(x) $drag(y)]
	}
    } else {
	# Moved?
	catch {
	    send $drag(app) [list drag::_motion $drag(win) $drag(type)\
			     $drag(x) $drag(y)]
	}
    }
}

proc drag::_left {} {
    global drag

    catch {send $drag(app) [list drag::_leave $drag(win) $drag(type)]}
}

# Callbacks from other applications

proc drag::_enter {w t x y} {
    global drag

    if [info exists drag($w,$t,enter)] {
	set x [expr $x - [winfo rootx $w]]
	set y [expr $y - [winfo rooty $w]]
	eval $drag($w,$t,enter) [list $x $y]
    }
}

proc drag::_leave {w t} {
    global drag

    if [info exists drag($w,$t,leave)] {eval $drag($w,$t,leave)}
}

proc drag::_motion {w t x y} {
    global drag

    if [info exists drag($w,$t,motion)] {
	set x [expr $x - [winfo rootx $w]]
	set y [expr $y - [winfo rooty $w]]
	eval $drag($w,$t,motion) [list $x $y]
    }
}

proc drag::_drop {w t x y data} {
    global drag

    # Send leave event before dropping
    if [info exists drag($w,$t,leave)] {eval $drag($w,$t,leave)}

    if [info exists drag($w,$t,drop)] {
	set x [expr $x - [winfo rootx $w]]
	set y [expr $y - [winfo rooty $w]]
	eval $drag($w,$t,drop) [list $x $y $data]
    }
}
