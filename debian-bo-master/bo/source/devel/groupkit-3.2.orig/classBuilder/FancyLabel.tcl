##############################################################################
# fancyLabel is an example of how special features to an existing widget class.
# fancyLabel is essentially a label with the additional option (-reverse) and 
# additional command (flash).  The option (-reverse) allows the label to be 
# displayed in reverse video or not, ie. the background and foreground colors 
# switched.  The command flash makes the label "flash".
#
# To create a fancyLabel just type the following:
# 	fancyLabel .fl
#	pack .fl
# Now to make the label flash just type ".fl flash"
#
# Note that this new widget .fl can be used in the same manner as
# the standard tcl widgets are.
##############################################################################

proc fancyLabel {w args} { 
    # This procedure constructs a new widget of the class "fancyLabel"
    # This is the only place where any of the gkClassBuilder procedures
    # get called.
    
    eval gkInt_CreateWidget $w fancyLabel fancyLabelClass $args
    return $w
}

proc fancyLabel_CreateClassRec {} {
    global fancyLabel
    
    # This procedure defines the default values for class of "fancyLabel"
    # widgets. Note that it inherits all of the commands/options from
    # the widget myLabel.  It also has a new option of its own, reverse,
    # and a new command, flash.
    
    set fancyLabel(inherit) {myLabel}
    set fancyLabel(rootWindow) myLabel
    set fancyLabel(options) {-reverse}
    set fancyLabel(-reverse) { -reverse reverse Reverse false}
    set fancyLabel(methods) {flash}
}

proc fancyLabel_Config {w option args} { 
    upvar #0 $w data
    
    # This procedure applies the option values to the widget.
    # If the option is NOT reverse then call configure on the
    # myLabel widget.  Otherwise if the option is reverse and
    # args are valid then switch the foreground and background
    # of the myLabel widget.
    
    if { [string match $option "-reverse"] } {
	if { [evaluateReverse $w $args] } { 
	    $w config -background $data(-foreground) \
		    -foreground $data(-background)
	}
    } else { 
	# An error causes the option to be applied to the root window,
	# in this case the myLabel widget.
	error
    }
}

proc fancyLabel_Methods {w command args} { 
    upvar #0 $w data
    # This procedure causes the label to flash by reversing the 
    # foreground and background colors every 100 ms.
    
    if { [string match $command flash] } { 
	if { [evaluateReverse $w true] } { 
	    $w config -reverse true
	} else { 
	    $w config -reverse false
	}
	after 100 "$w flash"
    } else { 
	error
    }
}

proc evaluateReverse {w args} {
    upvar #0 $w data
    # Returns true if the value in args is different then the value in
    # $data(-reverse)

    set flag 0
    set old [$w cget -reverse]

    switch -regexp $args {
	t|true|1  {  
	    if { ($old == 0) || [string match f* $old] } {
		set flag 1
	    }
	}
	f|false|0 { 
	    if { ($old == 1) || [string match t* $old] } {
		set flag 1
	    }
	}
	default {
	    error "bad option value \"$args\": must be a boolean value"
	}
    }
    return $flag
}   

