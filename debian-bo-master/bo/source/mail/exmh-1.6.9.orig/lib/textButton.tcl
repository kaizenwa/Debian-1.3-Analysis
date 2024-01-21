#
# $Id: textButton.tcl,v 1.5 1996/03/22 18:52:20 bwelch Exp $
#
# A text widget button that acts like a Button widget.
# - John Robert LoVerso
#
proc TextButton_Init { {t {}} } {
    global tb_priv

    set tb_priv(seed) 0
    if {[tk colormodel .] == "color"} {
	Preferences_Resource tb_priv(background)	c_uri thistle
	Preferences_Resource tb_priv(foreground)	c_uriFg black
	Preferences_Resource tb_priv(activebackground)	c_uriAbg white
	Preferences_Resource tb_priv(activeforeground)	c_uriAfg black
    } else {
	Preferences_Resource tb_priv(background)	c_uri black
	Preferences_Resource tb_priv(foreground)	c_uriFg white
	Preferences_Resource tb_priv(activebackground)	c_uriAbg white
	Preferences_Resource tb_priv(activeforeground)	c_uriAfg black
    }
    if {$t != {}} {
	# Hack - we know tags with names hdrlook=* are preserved.
	# These tags just serve to pre-allocate our colors
	$t tag configure hdrlook=TextButton1 -foreground $tb_priv(foreground) \
		-background $tb_priv(background)
	$t tag configure hdrlook=TextButton2 \
		-foreground $tb_priv(activeforeground) \
		-background $tb_priv(activebackground)
    }
}

proc TextButton { w text cmd } {

    $w insert insert { }
    set start [$w index insert]
    $w insert insert $text
    set end [$w index insert]
    $w insert insert { }

    set tag [TextButtonRange $w $start $end $cmd]
    global tk_version
    if {$tk_version < 4.0} {
	$w insert insert { }
	$w tag remove $tag $end insert
    }
}

proc TextButtonRange { w start end cmd } {
    global tb_priv tk_version

    incr tb_priv(seed)
    set id tb_priv$tb_priv(seed)
    $w tag add $id $start "$end +1 char"
    $w tag bind $id <Any-Enter> [concat TextButtonEnter $w $id]
    $w tag bind $id <Any-Leave> [concat TextButtonLeave $w $id]
    $w tag bind $id <1> [concat TextButtonDown $w $id]
    $w tag bind $id <ButtonRelease-1> [concat TextButtonUp $w $id [list $cmd]]
    if {$tk_version < 4.0} {
	$w tag bind $id <Any-ButtonRelease-1> [concat TextButtonUp $w $id]
    }
    $w tag configure $id -relief raised -borderwidth 2 \
	     -background $tb_priv(background) -foreground $tb_priv(foreground)
    return $id
}

#
#
# from button.tcl --
#

# The procedure below is invoked when the mouse pointer enters a
# button widget.  It records the button we're in and changes the
# state of the button to active unless the button is disabled.

proc TextButtonEnter {w id} {
    global tb_priv
    $w tag configure $id -background $tb_priv(activebackground) \
			-foreground $tb_priv(activeforeground)
    $w configure -cursor cross
    set tb_priv(window) $w
    set tb_priv(id) $id
}

# The procedure below is invoked when the mouse pointer leaves a
# button widget.  It changes the state of the button back to
# inactive.

proc TextButtonLeave {w id} {
    global tb_priv
    #puts "Leave"
    $w tag configure $id -background $tb_priv(background) \
			-foreground $tb_priv(foreground)
    $w configure -cursor xterm
    set tb_priv(window) ""
    set tb_priv(id) ""
    set tb_priv(cmd) ""
}

# The procedure below is invoked when the mouse button is pressed in
# a button/radiobutton/checkbutton widget.  It records information
# (a) to indicate that the mouse is in the button, and
# (b) to save the button's relief so it can be restored later.

proc TextButtonDown {w id} {
    global tb_priv
    set tb_priv(relief) [lindex [$w tag config $id -relief] 4]
    set tb_priv(buttonWindow) $w
    set tb_priv(buttonId) $id
    $w tag configure $id -relief sunken
}

# The procedure below is invoked when the mouse button is released
# for a button/radiobutton/checkbutton widget.  It restores the
# button's relief and invokes the command as long as the mouse
# hasn't left the button.

proc TextButtonUp {w id {cmd {}}} {
    global tb_priv
    #puts "Up"
    if {$w == $tb_priv(buttonWindow) && $id == $tb_priv(buttonId)} {
	$w tag config $id -relief $tb_priv(relief)
	if {$w == $tb_priv(window) && $id == $tb_priv(id)} {
	    set tb_priv(cmd) $cmd
	    #puts "Primed"
	    after 1 TextButtonActivate $w $id
	}
	set tk_priv(buttonWindow) ""
	set tk_priv(buttonId) ""
    }
}
proc TextButtonActivate {w id} {
    global tb_priv
    #puts "Activate cmd=$tb_priv(cmd)"
    eval $tb_priv(cmd)
}
