proc gk_choice {w args} {
    eval gkInt_CreateWidget $w gkChoice gkChoiceClass $args
    return $w
}

proc gkChoice_CreateClassRec {} {
    global gkChoice

    # set-up inheritance and what the root window should be
    set gkChoice(inherit) {button}
    set gkChoice(rootWindow) toplevel

    # add and define new options
    set gkChoice(options) {-choices -msg -title}
    set gkChoice(-choices) {-choices choices Choices {} }   
    set gkChoice(-msg) {-msg message Message {}} 
    set gkChoice(-title) {-title title Title {} }
}

proc gkChoice_ConstructWidget {w} {
    upvar #0 $w gkChoice

    if { $gkChoice(-msg) != ""} { eval $gkChoice(-msg) $w }
    pack [frame $w.button -bd 2 -relief groove] \
	    -fill both -expand y -side bottom
}


proc gkChoice_Config {w option args} {

    set args [lindex $args 0]
    switch -regexp -- $option {
	-choices {
	    # First destroy all existing buttons
	    foreach button [winfo children $w.button] {	destroy $button }

	    # Now create all of the new ones - making sure that each button
	    # will destroy the top-level widget
	    set i 0
	    foreach choice $args {
		set text [lindex $choice 0]
		set command [lindex $choice 1]
		button $w.button.b$i -text $text \
			-command "if [catch \"destroy $w\"] {}; $command"
		pack $w.button.b$i -expand y -padx 5 -pady 5 -side left
		incr i
	    }
	    # needed so that the toplevel widget gets drawn.
	    update
	}
	-msg { if { $args != ""} { eval $args $w ; update }}
	-title { wm title $w $args }
	default {
	    # default is to attempt to apply the options to all buttons
	    # WARNING!  this will catch some legitimate errors.
            if [catch {eval $w.button configure $option $args}] {}
	    set i 0
	    foreach button [winfo children $w.button] {
		if [catch {eval $w.button.b$i configure $option $args}] {}
		incr i
	    }
	}
    }
}

proc gkChoice_Methods {w command args} {

    set args [lindex $args 0]
    if { $command == "components" } {
	# The sorted list will ensure that the first entry in the
	# list is the leftmost button
	return [lsort [winfo children $w.button]]
    } else {
	# default is to attempt the command on all buttons and the
	# frame around them.  
	# WARNING!  this will catch some legitimate errors.
	catch [eval $w.button $command $args]
	set i 0
	foreach button [winfo children $w.button] {
	    catch [eval $w.button.b$i $command $args]
	    incr i
	}
    }
}


