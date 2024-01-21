#================================================================
# Telepointers Widget  UNDER CONSTRUCTION
# Description:
# Usage:
#================================================================

#------------------------------------------
# Internal Routines, Variables, and Actions
#------------------------------------------

#------------------------------------------
# External Routines
#------------------------------------------



# Initialize our telepointer and tell the other participants
# about it
proc gk_initializeTelepointers {} {
    gk_newenv telepointers

    # When a new user asks for an update, send him all the telepointers
    # we know about
    gk_bind updateEntrant "gk_sendTelepointerInfo %U"

    # When a user leaves, remove their telepointer
    gk_bind userDeleted "gk_endATelepointer %U"

    # Let's initialize our own telepointer
    set self [users local.usernum] 

    telepointers frame_suffix .telepointer

    telepointers users.$self.show 1
    telepointers users.$self.main_window .
    telepointers users.$self.bitmap \
	library/bitmaps/remote_cursor2.xbm
    telepointers users.$self.color [gk_getMyColour]

    # Now lets tell everyone about ourselves - this is badly done
    after 1000 "gk_sendAllTelepointerInfo $self"
}

proc gk_sendTelepointerInfo usernum { 
    foreach person [users keys remote] {
        if {$person != $usernum} {
            gk_sendTelepointerInfoTo $usernum $person
        }
    }
    gk_sendTelepointerInfoTo $usernum  [users local.usernum]
}

# Tell all others about a certain telepointer
proc gk_sendAllTelepointerInfo {who} {
    gk_toOthers \
	gk_receiveTelepointerInfo $who \
	[telepointers users.$who.show] \
	[telepointers users.$who.main_window] \
	[telepointers users.$who.bitmap] \
	[telepointers users.$who.color] 
}

# Tell one other about a certain telepointer
proc gk_sendTelepointerInfoTo {send_to who} {

    gk_toUserNum $send_to \
	gk_receiveTelepointerInfo $who \
	[telepointers users.$who.show] \
	[telepointers users.$who.main_window] \
	[telepointers users.$who.bitmap] \
	[telepointers users.$who.color] 
}


# We received information about another participant's telepointer.
# if it doesn't exist, create it.
proc gk_receiveTelepointerInfo {who show window bitmap_path color} {

    telepointers users.$who.show $show
    telepointers users.$who.main_window $window
    telepointers users.$who.bitmap $bitmap_path
    telepointers users.$who.color $color

    if {[telepointers users.$who.frame]==""} {
	gk_createTelepointer $who $window
    }
}

# Create the widget that represents a telepointer
proc gk_createTelepointer {who window} {
    global gk_library
    telepointers users.$who.main_window $window
    set suffix [telepointers frame_suffix]
    if {$window == "."} {
        telepointers users.$who.frame "$suffix$who"
    } else {
        telepointers users.$who.frame "$window$suffix$who"
    }
    label [telepointers users.$who.frame] \
	-bitmap @$gk_library/[telepointers users.$who.bitmap] \
	-borderwidth 0 \
	-anchor center \
	-foreground [telepointers users.$who.color]
}


# Whenever the local pointer moves, move the telepointers as well
# Since it does wierd things on Menus, de-activate it.
proc gk_specializeWidgetTelepointer {widget} {
    if {[info commands telepointer]==""} {
	gk_initializeTelepointers
    }

    if { [winfo class $widget] == "Menu"} return

    if { [winfo class $widget] == "Canvas"} {
	bind $widget <Motion> "+gk_moveCanvasTelepointer %X %Y %W" 
	bind $widget <B1-Motion> "+gk_moveCanvasTelepointer %X %Y %W"
	bind $widget <B2-Motion> "+gk_moveCanvasTelepointer %X %Y %W"
	bind $widget <B3-Motion> "+gk_moveCanvasTelepointer %X %Y %W"
	return
    }
 
    if { [winfo class $widget] == "Text"} {
        bind $widget <Motion> "+gk_moveTextTelepointer %X %Y %W"
        bind $widget <B1-Motion> "+gk_moveTextTelepointer %X %Y %W"
        bind $widget <B2-Motion> "+gk_moveTextTelepointer %X %Y %W"
        bind $widget <B3-Motion> "+gk_moveTextTelepointer %X %Y %W"
        return
    }
    
    bind $widget <Motion> \
	"+gk_moveTelepointer %X %Y %W"
    bind $widget <B1-Motion> \
	"+gk_moveTelepointer %X %Y %W"
    bind $widget <B2-Motion> \
	"+gk_moveTelepointer %X %Y %W"
    bind $widget <B3-Motion> \
	"+gk_moveTelepointer %X %Y %W"
}

proc gk_specializeWidgetTreeTelepointer {widget} {
    if {$widget == ""} return
    gk_specializeWidgetTelepointer $widget

    foreach child [winfo children $widget] {
	gk_specializeWidgetTreeTelepointer $child
    }
}

proc gk_moveCanvasTelepointer {abs_x abs_y widget} {
    
    set abs_wid_x [winfo rootx $widget]
    set abs_wid_y [winfo rooty $widget]

    set curs_rel_wid_x [$widget canvasx [expr $abs_x-$abs_wid_x]]
    set curs_rel_wid_y [$widget canvasy [expr $abs_y-$abs_wid_y]]

    set self [users local.usernum] 
    if {[telepointers users.$self.show] == 0} {
	return
    }

    gk_toOthers gk_reallyMoveCanvasTelepointer \
        $curs_rel_wid_x $curs_rel_wid_y $self $widget

}
 
proc gk_moveTextTelepointer {abs_x abs_y widget} {
    # find the widget's upper corner in root window coords
    set abs_wid_x [winfo rootx $widget]
    set abs_wid_y [winfo rooty $widget]
 
    # find the cursor's relative coordinates in the widget
    set curs_rel_wid_x [expr $abs_x-$abs_wid_x]
    set curs_rel_wid_y [expr $abs_y-$abs_wid_y]
 
    # find the character under the cursor (just use current for this?)
    set curs_char [$widget index current]
#    set curs_char [$widget index @$curs_rel_wid_x,$curs_rel_wid_y]
 
    # now calculate the offset of cursor within the character
    set bbox [$widget bbox $curs_char]
    set curs_rel_char_x [expr $curs_rel_wid_x - [lindex $bbox 0]]
    set curs_rel_char_y [expr $curs_rel_wid_y - [lindex $bbox 1]]
 
    set self [users local.usernum]
    if {[telepointers users.$self.show] == 0} {
        return
    }
 
    gk_toOthers gk_reallyMoveTextTelepointer \
        $curs_char $curs_rel_char_x $curs_rel_char_y $self $widget
}

# If the local pointer has moved, tell the others
proc gk_moveTelepointer {abs_x abs_y widget} {

    set abs_wid_x [winfo rootx $widget]
    set abs_wid_y [winfo rooty $widget]

    set curs_rel_wid_x [expr $abs_x-$abs_wid_x]
    set curs_rel_wid_y [expr $abs_y-$abs_wid_y]

    set self [users local.usernum] 
    if {[telepointers users.$self.show] == 0} {
	return
    }

    gk_toOthers gk_reallyMoveTelepointer \
        $curs_rel_wid_x $curs_rel_wid_y $self $widget
}

# Actually move the telepointer
proc gk_reallyMoveTelepointer {curs_rel_wid_x curs_rel_wid_y who widget} {
    gk_placeTelepointer $curs_rel_wid_x $curs_rel_wid_y $who $widget
}

proc gk_reallyMoveCanvasTelepointer {curs_rel_wid_x curs_rel_wid_y who widget} {
    set newx [expr $curs_rel_wid_x-[$widget canvasx 0]]
    set newy [expr $curs_rel_wid_y-[$widget canvasy 0]]
    gk_placeTelepointer $newx $newy $who $widget
}
 
proc gk_reallyMoveTextTelepointer {curs_char curs_rel_char_x curs_rel_char_y who
 widget} {
    # calculate the cursor position based on the character and offet
    set bbox [$widget bbox $curs_char]
    if {$bbox==""} {return}
    set newx [expr [lindex $bbox 0] + $curs_rel_char_x]
    set newy [expr [lindex $bbox 1] + $curs_rel_char_y]
    gk_placeTelepointer $newx $newy $who $widget
}

proc gk_placeTelepointer {x y who widget} {
    if {[telepointers users.$who.frame]!=""} {
        set widgetToplevel [winfo toplevel $widget]
        set telepointerToplevel [winfo toplevel \
		[telepointers users.$who.frame]]
        if {$widgetToplevel != $telepointerToplevel} {
            telepointers users.$who.main_window $widgetToplevel
            catch {destroy [telepointers users.$who.frame]}
	    gk_createTelepointer $who $widgetToplevel
        }

        place configure [telepointers users.$who.frame] \
            -x $x -y $y \
            -anchor center \
            -in $widget
    } else {
	# we don't have telepointer info; ask the user for it
	gk_toUserNum $who gk_sendTelepointerInfo [users local.usernum]
    }
}


# Get rid of the deleted user's telepointer and all the associated info
proc gk_endATelepointer who {

    if {[telepointers users.$who.frame]!=""} {
	catch {destroy [telepointers users.$who.frame]}
	telepointers delete users.$who
    }
}
