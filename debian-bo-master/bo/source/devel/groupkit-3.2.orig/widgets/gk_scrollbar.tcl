# NOTE: Coordinates for gk_views are stored as x0 y0 x1 y1.  They
#       are all stored as percentages.  The y0 and y1 coordinates
#       are used by the vertical scrollbar; the horizontal scrollbar
#       uses x0 and x1.  The other two coordinates required to display
#       the multi-user scrollbar are NOT stored anywhere - they will 
#       be adjusted if need by in the repack and the "canvas coords"
#       command is used to retrieve them when needed.
#
# NOTE: $w is used to find the total height of the scrollbar
#       because for some reason $w.remotes height is not updated
#       when the window gets resized
#
# BIG NOTE: The manner in which the gk_views environment is may cause
#       some strange behavior.  It is possible to have a vertical 
#       scrollbar attached to one item and a horizontal scrollbar 
#       attached to another - they will not interfere with each other.
#       in other words in a window hierarchy if there is one horizontal 
#       and/or one vertical scrollbar there will be no problems but as 
#       soon as there are two of either one there will be interference
#       unless they have different "scrollid's"

###################  Routines for gkClassBuilder #################

proc gk_scrollbar {w args} {
    eval gkInt_CreateWidget $w gkGroupScroll gkGroupScroll $args
    return $w
}

proc gkGroupScroll_CreateClassRec {} {
    global gkGroupScroll
    set gkGroupScroll(inherit) {scrollbar canvas}
    set gkGroupScroll(methods) {adduser attributechanged component deleteuser \
	    moveuser set}
    set gkGroupScroll(options) {-scrollid -multiuser -command -color -barwidth}
    set gkGroupScroll(-scrollid) {-scrollid scrollId ScrollId gkGroupScroll}
    set gkGroupScroll(-multiuser) {-multiuser multiUser MultiUser yes}
    set gkGroupScroll(-color) [list -color color Color [gk_getMyColour]]
    set gkGroupScroll(-command) {-command command Command {}}
    set gkGroupScroll(-barwidth) {-barwidth barWidth BarWidth 30}
}

proc gkGroupScroll_ConstructWidget {w} {
    upvar #0 $w gkScroll
    
    set gkScroll(local) [users local.usernum]
    set gkScroll(syncuser) ""

    # depending on whether the scrollbar is horizontal or vertical the
    # values for packing will change.  These are stored in variables to
    # make the configure routine nicer.
    if ![string first $gkScroll(-orient) horizontal] {
	set gkScroll(side) bottom;  set gkScroll(fill) x
    } elseif ![string first $gkScroll(-orient) vertical] {
	set gkScroll(side) right; set gkScroll(fill) y
    }

    # Make and pack the widget   
    pack [scrollbar $w.local] -side $gkScroll(side) -fill $gkScroll(fill)
    pack [canvas $w.remote -width $gkScroll(-barwidth) \
		-borderwidth 1 -relief ridge -highlightthickness 0] \
		-side $gkScroll(side) -fill $gkScroll(fill)

    # use the gk_views widget (environment) and initializes this users
    # coordinates
    gk_views $w $gkScroll(-scrollid)
    gk_viewsSetCoords $gkScroll(-scrollid) $gkScroll(local) [list 0 0 1 1]
    gk_viewsSetAttribute $gkScroll(-scrollid) $gkScroll(local) color \
	    $gkScroll(-color)
}

proc gkGroupScroll_Config {w option args} {
    upvar #0 $w gkScroll

    # Default configure is to apply the option and it's value to
    # the canvas - may not be what is wanted.  For details on the
    # options see the man page.
    # 
    # Note: that the options must be "exact" a better approach would
    #       be to make it consistent with Tk, allow abbreviations.

    set args [lindex $args 0]
    switch -exact -- $option {
	-barwidth { $w.remote config -width $args }
	-color {gk_viewsSetAttribute $gkScroll(-scrollid) color $args}
	-command { $w.local configure -command $args }
	-orient { 
	    if ![string first $args horizontal] {
		$w.local config -orient horizontal
		$w.remote config -height $gkScroll(-barwidth)
		set gkScroll(fill) x; set gkScroll(side) bottom
	    } elseif ![string first $args vertical] {
		$w.local config -orient vertical
		$w.remote config -width $gkScroll(-barwidth)
		set gkScroll(fill) y; set gkScroll(side) right
	    } else { 
		error "bad orientation \"$gkScroll(-orient)\": must be \
			vertical or horizontal"
	    }
	    set gkScroll(-orient) $args
	    pack forget $w.local $w.remote
	    pack $w.local -side $gkScroll(side) -fill $gkScroll(fill)
	    if [isTrue $gkScroll(-multiuser)] {
		pack $w.remote -side $gkScroll(side) -fill $gkScroll(fill)
		gkGroupScroll_repack $w
	    }
	}
        -multiuser {
	    if [isTrue $args] {
		pack $w.remote -side $gkScroll(side) -fill $gkScroll(fill)
	    } elseif [isFalse $args] {
		pack forget $w.remote
	    } else {
		error "expected boolean value but got \"$args\""
	    }
	}
	-scrollid { 
	    if ![catch "gk_viewsUsers $gkScroll(-scrollid)"] {
		gk_viewsCopy $w $gkScroll(-scrollid) $args
#		gk_viewsDelete $gkScroll(-scrollid)
	    }
	}
	default { 
	    if [catch "$w.local config $option $args" err1] {}
	    if [catch "$w.remote config $option $args" err2] {}
	    if { ($err1 != "") && ($err2 != "")} { error }
	}
    }
}   

proc gkGroupScroll_Methods {w command args} {
    upvar #0 $w gkScroll
    set args [lindex $args 0]

    # NOTE: that adduser, attributechanged, deleteuser, and moveuser are
    #       subcommands expected by the gk_views widget, therefore,
    #       we need to define them!

    switch -exact $command {
	adduser { 
	    set args [lreplace $args 0 0]
	    eval gkGroupScroll_newbar $w "$args"
	}
	attributechanged { eval gkGroupScroll_changeAttribute $w $args }
	component {gkGroupScroll_component $w $args}
	deleteuser {
	    set who [lindex $args 1]
	    if { [$w.remote find withtag tag$who]!=""} { 
		$w.remote delete tag$who 
	    }
	    gkGroupScroll_repack $w
	    if {$who==$gkScroll(syncuser)} {
		set gkScroll(syncuser) ""
	    }
	}
	moveuser {eval gkGroupScroll_changeCoords $w $args}
	set {eval gkGroupScroll_set $w $args}
	default {
	    if [catch "$w.local $command $args" err1] {}
	    if [catch "$w.remote $command $args" err2] {}
	    if { ($err1 != "") && ($err2 != "") } { error }
	}
    }
}

################### Subcommand/Method Handling  #################

proc gkGroupScroll_set {w first last} {
    upvar #0 $w gkScroll

    #  if the scroll bar doesn't exist no point in moving it
    if {[$w.remote find withtag tag$gkScroll(local)]==""} {return}

    # Now lets get tho se coordinates and send in the new ones!!
    scan [gk_viewsGetCoords $gkScroll(-scrollid) $gkScroll(local)]] \
	    "%f %f %f %f" x0 y0 x1 y1

    if ![string first $gkScroll(-orient) horizontal] {
	gk_viewsSetCoords $gkScroll(-scrollid) $gkScroll(local) \
		[list $first $y0 $last $y1]
    } else {
	gk_viewsSetCoords $gkScroll(-scrollid) $gkScroll(local) \
		[list $x0 $first $x1 $last]
    }
}

# return the window path name for the appropriate component
proc gkGroupScroll_component {w stuff} {
    upvar #0 $w gkScroll

    switch [lindex $stuff 0] {
	local {return $w.local}
	remote {return $w.remote}
	default {error "no such component"}
    }
}

# an attribute has changed so handle it.
proc gkGroupScroll_changeAttribute {w id who attr color} {

    if { [$w.remote find withtag tag$who]!=""} { 
	# since the scrollbar exists just changed its color
	$w.remote itemconfigure tag$who -fill $color
    } else {
	# What to do if the scrollbar hasn't been created yet!
	if { [set coords [gk_viewsGetCoords $id $who]]=="" } {
	    set coords {0 0 1 1}
	}
	gkGroupScroll_newbar $w $who $coords
    }
}

# the user's coordinates have changed so move the scrollbar.
proc gkGroupScroll_changeCoords {w id who coords} {
    upvar #0 $w gkScroll

    # What to do if the scrollbar hasn't been created yet!
    if { [$w.remote find withtag tag$who]=="" } {
	gkGroupScroll_newbar $w $who $coords
	return
    }

    # if it is the local guy we better move the real scroll bar.
    # Otherwise find the new coordinates.
    scan [$w.remote coords tag$who] "%f %f %f %f" x0 y0 x1 y1
    if ![string first $gkScroll(-orient) horizontal] {
	if {$who==$gkScroll(local)} {
	    $w.local set [lindex $coords 0] [lindex $coords 2]
	}
	set coords [lreplace $coords 1 1 $y0]
	set coords [lreplace $coords 3 3 $y1]
    } else {
	if {$who==$gkScroll(local)} {
	    $w.local set [lindex $coords 1] [lindex $coords 3]
	}
	set coords [lreplace $coords 0 0 $x0]
	set coords [lreplace $coords 2 2 $x1]
    }
    # Move the remote scrollbar
    eval $w.remote coords tag$who [eval gkGroupScroll_adjustCoords $w $coords]
    # see if we're synced to them
    if {$who==$gkScroll(syncuser)} {
	if ![string first $gkScroll(-orient) horizontal] {
	    set posn [lindex $coords 0]
	} else {
	    set posn [lindex $coords 1]
	}
	uplevel #0 eval $gkScroll(-command) moveto $posn
    }
}

########################## Support Routines  ###########################

# Create a new scrollbar
proc gkGroupScroll_newbar {w who coords} {
    upvar #0 $w gkScroll

    # if it has been created then don't create it again.
    if { [$w.remote gettags tag$who] != "" } { return }

    # if the window isn't around wait until it is before creating
    # the new scrollbar
    if ![winfo exists $w.remote] { 
	# if the window doesn't exist then we better wait and try
	# again later.
	after 500 gkGroupScroll_newbar $w $who [list $coords]
	return
    }

    # if we don't know the user's color yet, wait until we find out.
    set color [gk_viewsGetAttribute $gkScroll(-scrollid) $who color]
    if { $color == "" } {
	after 500 gkGroupScroll_newbar $w $who [list $coords]
	return
    }

    # create the scrollbar
    scan [eval gkGroupScroll_adjustCoords $w $coords] "%f %f %f %f" x0 y0 x1 y1
    $w.remote create rectangle $x0 $y0 $x1 $y1 -fill $color -tag tag$who
    
    # make the mouse button 1 pop-up window.
    ##    gk_popup $w.remote $who $gkScroll(local) canvas
    _gk_scrollPopup $w $who

    # repack the scrollbars since once more has been added to the 
    # existing ones.
    gkGroupScroll_repack $w
}

proc gkGroupScroll_repack {w} {
    upvar #0 $w gkScroll

    # found out how many and who all the users are.
    set users [gk_viewsUsers $gkScroll(-scrollid)]
    set numbars [llength $users]
    if {$numbars==0} {return}

    # initialize the various variables
    set offset 0.0
    set total $gkScroll(-barwidth)
    set barwidth [expr $total/$numbars]

    # if not all of the bars have been created then return.
    # the bar will be created soon and repacked so this will
    # be done - don't worry.
    if { [llength [$w.remote find all]] < $numbars } { return }

    if ![string first $gkScroll(-orient) horizontal] {	 
	# now find the new y coordinates for everyone.
	foreach user $users {
	    scan [$w.remote coords tag$user] "%f %f %f %f" x0 y0 x1 y1
	    set y1 [expr $total-$offset]
	    set y0 [expr $y1-$barwidth]
            eval $w.remote coords tag$user [list $x0 $y0 $x1 $y1]
	    set offset [expr $offset + $barwidth]
	}
    } else {	
	# now find the new x coordinates for everyone.
	foreach user $users {
	    scan [$w.remote coords tag$user] "%f %f %f %f" x0 y0 x1 y1
	    set x1 [expr $total-$offset]
	    set x0 [expr $x1-$barwidth]
            eval $w.remote coords tag$user [list $x0 $y0 $x1 $y1]
	    set offset [expr $offset + $barwidth]
	}
    }
}

# A little routine that will take coordinates with percentages and
# return decimal coordinates or converts the decimal coordinates
# to percentages.  Of cource it only converts the expected ones!
proc gkGroupScroll_adjustCoords {w x0 y0 x1 y1} {
    upvar #0 $w gkScroll

    if ![string first $gkScroll(-orient) horizontal] {
	if { [set total [winfo width $w]] == 0 } {set total 1}
	if { $x0 > 1 } {
	    set x0 [expr $x0 / $total]
	    set x1 [expr $x1 / $total]
	} else {
	    set x0 [expr $x0 * $total]
	    set x1 [expr $x1 * $total]
	}
    } else {
    	if { [set total [winfo height $w]] == 0 } {set total 1}
	if { $y0 > 1 } {
	    set y0 [expr $y0 / $total]
	    set y1 [expr $y1 / $total]
	} else {
	    set y0 [expr $y0 * $total]
	    set y1 [expr $y1 * $total]
	}
    }
    return [list $x0 $y0 $x1 $y1]
}

### popup to handle synchronized scrolling...
proc _gk_scrollPopup {w user} {
    $w.remote bind tag$user <1> "_gk_scrollmenu $w $user %X %Y"
    $w.remote bind tag$user <B1-ButtonRelease> "catch \{destroy $w.remote.popup\}"
}

proc _gk_scrollmenu {w user x y} {
    upvar #0 $w gkScroll
    catch {destroy $w.remote.popup}
    menu $w.remote.popup -tearoff 0
    if {$user==[users local.usernum]} {
	$w.remote.popup add command -label "This is you"
	if {$gkScroll(syncuser)!=""} {
	    $w.remote.popup add command -label "Scroll independently" \
		    -command "_gk_syncUser $w {}"
	}
    } else {
	$w.remote.popup add command -label [users remote.$user.username]
	if {$gkScroll(syncuser)==$user} {
	    $w.remote.popup add command -label "Stop following" \
		    -command "_gk_syncUser $w {}"
	} else {
	    $w.remote.popup add command -label "Follow this user" \
		    -command "_gk_syncUser $w $user"
	}
    }
    $w.remote.popup post $x $y
    grab $w.remote.popup
}

proc _gk_syncUser {w who} {
    upvar #0 $w gkScroll
    set gkScroll(syncuser) $who
}
