############################################################
#
# A little note about tags and the way things look.  tags in
# the tk text widget do not change their background color if
# their is no text from the start to end of that line.  
# therefore, the highlighted sections have gaps in them where
# their is no text.  
#
# Another little problem with text widgets.  For some reason
# a tab is calculated as one character instead of "n" spaces.
# This means that highlighting will not be accurate for lines
# with tabs in them.  To correct this all input should have
# tabs replaced with the correct number of spaces.
#
# The coordinates are stored in percentages and they are
# in the format "x0 y0 x1 y1" where
#        x0 = left coordinate
#        y0 = top coordinate
#        x1 = right coordinate
#        y1 = bottom coordinate 
############################################################


############################################################
# 
# The usual procedures to work with the class builder.
#
############################################################

proc gk_miniText {w args} {
    eval gkInt_CreateWidget $w gkMiniText GkMiniText $args
    return $w
}

proc gkMiniText_CreateClassRec {} {
    global gkMiniText

    set gkMiniText(inherit) {text}
    set gkMiniText(rootWindow) text    
    set gkMiniText(methods) {adduser attributechanged deleteuser miniyview \
	    minixview moveuser}
    set gkMiniText(options) {-adjust -color -textid -viewscroll \
	-xviewscrollcommand -yviewscrollcommand}
    set gkMiniText(-adjust) {-adjust adjust Adjust true}
    set gkMiniText(-color) [list -color color Color [gk_getMyColour]]
    set gkMiniText(-textid) {-textid textId TextId gkMiniText}
    set gkMiniText(-viewscroll) {-viewscroll viewScroll ViewScroll true}
    set gkMiniText(-xviewscrollcommand) {-xviewscrollcommand xScrollCommand\
	    ScrollCommand {}}
    set gkMiniText(-yviewscrollcommand) {-yviewscrollcommand yScrollCommand \
	    ScrollCommand {}}
}

proc gkMiniText_InitWidgetRec {w class className args} {
    upvar #0 $w gkMiniText

    set gkMiniText(-font) nil2
    set gkMiniText(-state) disabled
    set gkMiniText(-wrap) none
}

proc gkMiniText_ConstructWidget {w  args} {
    upvar #0 $w gkMiniText

    # A variable that keeps track of the longest line, initially it
    # is 1.  Also another variable to determine the width of a typical
    # character for the nil2 font.
    set gkMiniText(longest) 1
    $w insert 1.0 0
    set gkMiniText(charWidth) [lindex [$w bbox 1.0] 2]
    $w delete 1.0

    set user [users local.usernum] 
    # Set-up the gk_views widget.
    if [info exists gkMiniText(tmp-textid)] {
	gk_views $w $gkMiniText(tmp-textid)
    } else {
	gk_views $w $gkMiniText(-textid)
	# Initialize the local user Information.
	gk_viewsSetAttribute $gkMiniText(-textid) $user color \
		$gkMiniText(-color)
	gk_viewsSetCoords $gkMiniText(-textid) $user [list 0 0 1 1]
    }
}

proc gkMiniText_Config {w option args} {
    upvar #0 $w gkMiniText

    set args [lindex $args 0]
    switch -exact -- $option {
	-adjust { 
	    if ![isBoolean $args] {
		error "expected boolean value but got \"$args\""
	    }
	}
	-color {gk_viewsSetAttribute $gkMinitext(-textid) color $args}
	-font { $gkMiniText(rootCmd) configure -font $gkMiniText(-font) }
	-textid {            
	    if ![catch "gk_viewsUsers $gkMiniText(-textid)"] {
		gk_viewsCopy $w $gkMiniText(-textid) $args
		gk_viewsDelete $gkMiniText(-textid)
	    }
	}
	-xviewscrollcommand { #Do nothing }
	-yviewscrollcommand { #Do nothing }
	default { 
	    # If an error is caused here then the class builder attempts
	    # to apply the option to the root window - which is what we
	    # want.
	    error 
	}
    }
}

proc gkMiniText_Methods {w command args} {
    upvar #0 $w gkMiniText

    set args [lindex $args 0]
    set local [users local.usernum]

    switch -exact $command {
	adduser { eval gkMiniText_addUser $w $args }
	attributechanged { eval gkMiniText_changeAttribute $w $args}
	delete { 
	    # since the minitext is not editable we need to change the
	    # state in order to do insertions and deletions.
	    # update the longest line if need be.
	    $w configure -state normal
	    eval $gkMiniText(rootCmd) $command $args
	    $w configure -state disabled
	    gkMiniText_checkLongest $w $command $args
	}
	deleteuser { set who [lindex $args 1]; $w tag delete tag$who }
	insert { gkMiniText_insert $w $args }
	miniyview {
	    # change the yview of the local user's highlighted section.
	    set current [gk_viewsGetCoords $gkMiniText(-textid) $local]
            if { $current == "" } { set current [list 0 0 1 1] }
	    if { $args == "" && $current != "" } { 
		# no arguments so return the current value.
		return [list [lindex $current 1] [lindex $current 3]]
	    } else {
		# new value so change the coordinates.
		set current [lreplace $current 1 1 [lindex $args 0]]
		if { [lindex $args 1] == 0 } {
		    set current [lreplace $current 3 3 1]
		} else {
		    set current [lreplace $current 3 3 [lindex $args 1]]
		}
		gk_viewsSetCoords $gkMiniText(-textid) $local $current
	    }
	}
	minixview { 
	    # change the xview of the local user's highlighted section.
	    set current [gk_viewsGetCoords $gkMiniText(-textid) $local]
            if { $current == "" } { set current [list 0 0 1 1] }

	    if { $args == "" } { 
		# no arguments so return the current value.
		return [list [lindex $current 0] [lindex $current 2]]
	    } else {
		# new value so change the coordinates.
		set current [lreplace $current 0 0 [lindex $args 0]]
		set current [lreplace $current 2 2 [lindex $args 1]]
		gk_viewsSetCoords $gkMiniText(-textid) $local $current
	    }
	}   
	moveuser { eval gkMiniText_changeCoords $w $args }
	see {
	    # since see doesn't work as needed for the text widget
	    # this is a variation of it.
	    gkMiniText_see $args
	}
	default { 
	    # If an error is caused here then the class builder attempts
	    # to apply the command to the root window - which is what we
	    # want.
	    error 
	}
    }
}

############################################################
#
#       Routines expected by the gk_views widget.
#       They handle changes in the environment.
#
############################################################

# Add a new user, highlighted section, to the miniText 
proc gkMiniText_addUser {w id who coords} {
    upvar #0 $w gkMiniText

    # We already have a section highlighted for this user so return
    if {[lsearch [$w tag names] tag$who]>=0} { return }

    # wait until the window is drawn before adding the user to
    # prevent errors.
    if ![winfo viewable $w] {
	after 500 gkMiniText_addUser $w $id $who [list $coords]
	return
    }     

    # since we need the color before proceeding we better wait until
    # it is there.
    set color [gk_viewsGetAttribute $gkMiniText(-textid) $who color]
    if { $color == "" } {
	after 500 gkMiniText_addUser $w $id $who [list $coords]
        return
    }

    # add a new highlighted section for the new user.
    scan $coords "%f %f %f %f" x0 y0 x1 y1
    set lastLine [lindex [split [$w index end] .] 0]
    set x0 [expr $x0 * $gkMiniText(longest)]
    set x1 [expr $x1 * $gkMiniText(longest)]; 
    set y0 [expr $y0 * $lastLine]; set y1 [expr $y1 * $lastLine]
    scan [eval gkMiniText_adjustCoords [list $x0 $y0 $x1 $y1]] \
	    "%d %d %d %d" x0 y0 x1 y1
    for {set idx $y0} { $idx <= $y1 } { incr idx } {
	$w tag add tag$who ${idx}.$x0 ${idx}.$x1
    }   
    $w tag lower tag$who sel
    $w tag configure tag$who -background $color

    # Create the pop-up window - mouse button 1 action
    gk_popup $w $who [users local.usernum] text

    # set-up bindings if scrolling is enabled and it is the local user
    if { $who == [users local.usernum] && [isTrue $gkMiniText(-viewscroll)]} {
        $w tag bind tag$who <3> "_gkMiniText_startScroll $w %x %y"
        $w tag bind tag$who <B3-Motion> "_gkMiniText_Scroll $w %x %y"
    }

    # if it is the local user and adjusting is enabled then change the view
    # of the miniText so we see the local users entire highlighted portion.
    if { $who == [users local.usernum] && [isTrue $gkMiniText(-adjust)] } {
	$w see $x0.$x0 ; $w see $x1.$y1 
    }
}

# An attribute has changed so we need to change the appropriate
proc gkMiniText_changeAttribute {w id who attr arg} {

    if { [lsearch [$w tag names] tag$who] < 0 } {
	if { [set coords [gk_viewsGetCoords $id $who]] == "" } { 
	    set coords {0 0 1 1} 
	} 
	gkMiniText_addUser $w $id $who $coords
	return
    } else {
	set who [lindex $args 2]
	$w tag configure tag$who -background [lindex $args 3]
    }
}

# The coordinates have changed for the highlighted section so
# we need to move it.
proc gkMiniText_changeCoords {w id who coords} {
    upvar #0 $w gkMiniText

    # if tag hasn't been created then we must!
    if { [lsearch [$w tag names] tag$who] < 0 } {
	gkMiniText_addUser $w $id $who $coords
        return
    }

    if {([info exists gkMiniText(coords$who)]) && \
	    ($gkMiniText(coords$who)==$coords)} {
	return
    }
    # change the coordinates from percentages into decimals
    scan $coords "%f %f %f %f" x0 y0 x1 y1
    set lastLine [lindex [split [$w index end] .] 0]
    set x0 [expr $x0 * $gkMiniText(longest)]
    set x1 [expr $x1 * $gkMiniText(longest)]; 
    set y0 [expr $y0 * $lastLine]; set y1 [expr $y1 * $lastLine]
    scan [eval gkMiniText_adjustCoords [list $x0 $y0 $x1 $y1]] \
	    "%d %d %d %d" x0 y0 x1 y1

    # if the tags are not done line by line, ie. just the start and
    # end coordinates are used then the highlighted section will contain
    # entire lines instead of portions of lines.  Also note that the
    # "tag remove" command is used instead of "tag delete".  This means
    # that we don't have to create the tag and we don't have to redefine
    # the tag bindings - in theory it should be faster.
    $w tag remove tag$who 0.0 end
    for {set idx $y0} { $idx <= $y1 } { incr idx } {
	$w tag add tag$who ${idx}.$x0 ${idx}.$x1
    }   

    # now make sure we can see our own highlighted section
    # if and only if -adjust is set to true
    if { $who == [users local.usernum] && [isTrue $gkMiniText(-adjust)] } {
	gkMiniText_see $w ${y0}.${x0}
	gkMiniText_see $w ${y1}.${x1}
    }

    set gkMiniText(coords$who) $coords
}

# miniText insert Routine
proc gkMiniText_insert {w args} {
    upvar #0 $w gkMiniText

    # store the current tag positions before we insert any of the text.
    set args [lindex $args 0]
    set tagPositions {}
    set start [$w index "[lindex $args 0] linestart"]
    set end [$w index "[lindex $args 0] lineend"]
    foreach tag [$w tag names [lindex $args 0]] {
	lappend tagPositions [list $tag [$w tag nextrange $tag $start $end]]
    }

    # since the minitext is not editable we need to change the
    # state in order to do insertions and deletions.
    $w configure -state normal
    eval $gkMiniText(rootCmd) insert $args
    $w configure -state disabled

    # Now reposition the tags as they were before the insertion.
    foreach tagInfo $tagPositions {
	set tag [lindex $tagInfo 0]
	set start [lindex [lindex $tagInfo 1] 0]
	set end [lindex [lindex $tagInfo 1] 1]
	$w tag remove $tag "$start linestart" "$end lineend"
	$w tag add $tag $start $end
    }
    
    # update the longest line if need be.
    gkMiniText_checkLongest $w insert [lindex $args 0]
}

# a variation of the see subcomand for the text widget.  The see command
# for the text widget moves the view so that any text close to the 
# coordinates given is viewable.  for example if you give the coordinate
# for a blank line the view will be moved to the very left since their
# is nothing to see.  this routine makes sure that the given coordinates
# are viewable - probably the way it should be implemented anyway.
proc gkMiniText_see {w position} {
    upvar #0 $w gkMiniText

    # lets find all those numbers we need.
    set y [lindex [split $position .] 0]
    set x [lindex [split $position .] 1]
    set last [$w index end]
    set top [expr [lindex [$w yview] 0] * $last]
    set bottom [expr [lindex [$w yview] 1] * $last]
    set left [expr [lindex [$w xview] 0] * $gkMiniText(longest)]
    set right [expr [lindex [$w xview] 1] * $gkMiniText(longest)]

    if { $y < $top } {
	# position wanted is above current viewing area.
	$w yview moveto [expr $y.0 / $last]
    } elseif {$y > $bottom} {
	# position wanted is below current viewing area.
	$w yview moveto [expr ( $top + $y.0 - $bottom ) / $last ]
    }

    if { $x < $left } {
	# position wanted is left of the current viewing area.
	$w xview moveto [expr $x.0 / $gkMiniText(longest)]
    } elseif {$x > $right} {
	# position wanted is right of the current viewing area.
	$w xview moveto [expr ( $left + $x.0 - $right ) / $gkMiniText(longest)]
    }
}

############################################################
#
#      Internal procedures 
#
############################################################

# when mouse button 3 is pressed this routine is invoked.
# it just stores the current mouse position
proc _gkMiniText_startScroll {w x y} {
    upvar #0 $w gkMiniText

    set gkMiniText(startx) [expr $x / $gkMiniText(charWidth)]
    set gkMiniText(starty) [lindex [split [$w index @$x,$y] .] 0]
}

# after mouse button 3 has been pressed this routine is used to
# handle any mouse movement
proc _gkMiniText_Scroll {w x y} { 
    upvar #0 $w gkMiniText

    # calculate the new position and the change in position, and store
    # store the new position
    set who [users local.usernum]
    set lastLine [lindex [split [$w index end] .] 0]
    set newX [expr $x / $gkMiniText(charWidth)]
    set newY [lindex [split [$w index @$x,$y] .] 0]
    set deltaX [expr ($newX.0 - $gkMiniText(startx).0) / $gkMiniText(longest).0]
    set deltaY [expr ($newY.0 - $gkMiniText(starty).0) / $lastLine.0]
    set gkMiniText(startx) $newX; set gkMiniText(starty) $newY

    # get the old coordinates and calculate the new ones
    scan [gk_viewsGetCoords $gkMiniText(-textid) $who] "%f %f %f %f" x0 y0 x1 y1
    set x0 [expr $x0 + $deltaX];     set y0 [expr $y0 + $deltaY]
    set x1 [expr $x1 + $deltaX];     set y1 [expr $y1 + $deltaY]

    # Make sure indicies are within the text area 
    if {$x0 < 0.0} { set x1 [expr $x1 - $x0] ; set x0 0.0}
    if {$y0 < 0.0} { set y1 [expr $y1 - $y0] ; set y0 0.0}
    if {$y1 > 1.0} { set y0 [expr 1.0 - ($y1 - $y0)]; set y1 1.0 }
    if {$x1 > 1.0} { set x0 [expr 1.0 - ($x1 - $x0)] ; set x1 1.0 }

    # now register the new coordinates
    gk_viewsSetCoords $gkMiniText(-textid) $who [list $x0 $y0 $x1 $y1]

    # NOTE: moveto moves to the char closest to the position not the
    #       actual position.
    if { $gkMiniText(-yviewscrollcommand) != "" } {
	eval $gkMiniText(-yviewscrollcommand) moveto $y0
    }
    if { $gkMiniText(-xviewscrollcommand) != "" } {
	eval $gkMiniText(-xviewscrollcommand) moveto $x0
    }
}

# a little routine that hopefully efficently determines what the
# longest line of text is.
proc gkMiniText_checkLongest {w command args} {
    upvar #0 $w gkMiniText

    # determine the last line of text
    set end [lindex [split [expr [$w index end] - 2] .] 0]

    # find what two indicies we should be checking between.
    set first [lindex [split [lindex $args 0] .] 0]
    set last [lindex [split [lindex $args 1] .] 0]
    if { $first == "end"} { set first $end }
    if { $last == "" } { set last $first
    } elseif {$last == "end"} { set last $end }

    # slightly different action is required if we are inserting text
    # or removing text.
    switch -exact $command {
	insert {
	    for {} {$first <= $last} {set first [expr $first + 1]} {
		set lineLength [lindex [split [$w index "$first.0 lineend"] .] 1]
		if {$lineLength > $gkMiniText(longest) } {
		    set gkMiniText(longest) $lineLength
		}
	    }
	}
	delete {
	    set longest 1
	    for {set line 0} {$line <= $end} {incr line} {
		if { [$w get $line.0 "$line.0 lineend"] != "" } {
		    set current [lindex [split [$w index \
			    "$line.0 lineend"] .] 1]
		    if { $current > $longestIdx} { set longestIdx $current}
		}
	    }
	    set gkMiniText(longest) $longest
	}
	default {
	    error "Invalid command: $command"
	}
    }
}

# round a set of floating pointing coordinates and return the result
proc gkMiniText_adjustCoords {x0 x1 y0 y1} {
    set x0 [expr round($x0)];     set x1 [expr round($x1)];
    set y0 [expr round($y0)];     set y1 [expr round($y1)];
    return [list $x0 $x1 $y0 $y1]
}
