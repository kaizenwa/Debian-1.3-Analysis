#----------------------------------------------------------------
# Miscellaneous procedures
# Copyright (c) 1996,1997  Takashi Iwai
#----------------------------------------------------------------

#----------------------------------------------------------------
# tk easy programming
#----------------------------------------------------------------

#
# get root file name
#
proc retrieve-filename {path} {
    set divs [split $path /]
    return [lindex $divs [expr [llength $divs] - 1]]
}


#
# sec to time string
#
proc sec2time {sec} {
    if {$sec >= 0} {
	return [format "%02d:%02d" [expr $sec / 60] [expr $sec % 60]]
    } else {
	set sec [expr -$sec]
	return [format "-%02d:%02d" [expr $sec / 60] [expr $sec % 60]]
    }
}

#
# numeric binding:
# only numerical key and some controls are available for input.
#
proc numeric-bind {w} {
    bind $w <Any-Key> {
	if {"%A" != "" && [regexp "\[0-9\]+" %A]} {
	    %W insert insert %A
	    tk_entrySeeCaret %W
	} elseif {"%K" == "Return"} {
	    global tk_priv
	    focus none
	}
    }
    bind $w <Key-Delete> {tk_entryBackspace %W; tk_entrySeeCaret %W}
    bind $w <Key-BackSpace> {tk_entryBackspace %W; tk_entrySeeCaret %W}
    bind $w <Control-Key-h> {tk_entryBackspace %W; tk_entrySeeCaret %W}
    bind $w <Control-Key-d> {%W delete sel.first sel.last; tk_entrySeeCaret %W}
    bind $w <Control-Key-u> {%W delete 0 end}
}

#
# make a listbox
#
proc my-listbox {w title size {dohoriz 1} {multiple 0}} {
    global tk_priv
    frame $w
    label $w.label -text $title -relief flat
    pack $w.label -side top -fill x -anchor w
    scrollbar $w.yscr -command "$w.list yview"
    pack $w.yscr -side right -fill y
    regexp "(\[0-9\]+)x(\[0-9\])" $size foo width height
    set lopt [list -width $width -height $height]
    if {$multiple} {
	lappend lopt -selectmode multiple
    }
    if {$dohoriz} {
	scrollbar $w.xscr -command "$w.list xview" -orient horizontal
	pack $w.xscr -side bottom -fill x
	eval listbox $w.list -relief sunken -setgrid yes $lopt\
		[list -yscroll "$w.yscr set"]\
		[list -xscroll "$w.xscr set"]
    } else {
	eval listbox $w.list -relief sunken -setgrid yes $lopt\
	    [list -yscroll "$w.yscr set"]
    }
    pack $w.list -side left -fill both -expand yes
    return $w.list
}

#----------------------------------------------------------------
# dialog pop-up
#----------------------------------------------------------------

proc my-dialog {w title defbtn canbtn buttons} {
    toplevel $w -class Dialog
    wm title $w $title
    wm iconname $w $title

    label $w.title -text $title -relief raised -bd 1
    pack $w.title -side top -fill x
    
    frame $w.f -relief raised -bd 1
    pack $w.f -side top -fill both

    frame $w.buttons -relief raised -bd 1
    pack $w.buttons -side bottom -fill both
    set i 0
    foreach but $buttons {
	button $w.buttons.c$i -text [lindex $but 0] -command [lindex $but 1]
	if {$defbtn != "" && $i == $defbtn} {
	    frame $w.buttons.default -relief sunken -bd 1
	    raise $w.buttons.c$i $w.buttons.default
	    pack $w.buttons.default -side left -expand 1\
		    -padx 3m -pady 2m
	    pack $w.buttons.c$i -in $w.buttons.default -padx 2m -pady 2m \
		    -ipadx 2m -ipady 1m
	    bind $w <Return> "$w.buttons.c$i flash; $w.buttons.c$i invoke"
	} else {
	    pack $w.buttons.c$i -side left -expand 1 \
		    -padx 3m -pady 3m -ipadx 2m -ipady 1m
	    if {$canbtn != "" && $i == $canbtn} {
		bind $w <Key-Escape> "$w.buttons.c$i flash; $w.buttons.c$i invoke"
	    }
	}
	incr i
    }

    return $w.f
}

#----------------------------------------------------------------
#  warning/question dialog
#----------------------------------------------------------------

proc my-message-dialog {w title text bitmap defbtn canbtn args} {
    #puts stderr $text
    return [eval tk_dialog [list $w $title $text $bitmap $defbtn] $args]
}

proc warning {message} {
    my-message-dialog .warning "Warning" $message warning 0 0 {  OK  }
}

proc error {message} {
    my-message-dialog .error "Error" $message error 0 0 {  OK  }
}
    
proc information {message} {
    my-message-dialog .info "Information" $message info 0 0 {  OK  }
}
    
proc question {message {defrc 1}} {
    global tk_priv
    if {$defrc} {
	set defbtn 0
	set canbtn 1
    } else {
	set defbtn 1
	set canbtn 0
    }
    return [expr ![my-message-dialog .yesno "Question" $message question\
	    $defbtn $canbtn "Yes" "No"]]
}

#----------------------------------------------------------------
# get the root file name from full path
#----------------------------------------------------------------

proc rootname {path} {
    if {$path == "/"} {
	return $path
    } elseif [regexp "\[^/\]+$" $path base] {
	return $base
    } elseif [regexp "(\[^/\]+)/$" $path rest base] {
	return $base
    } else {
	return $path
    }
}

#----------------------------------------------------------------
# pseudo random routine without TclX
#----------------------------------------------------------------

set pseudo_random [catch {random 1}]
set pseudo_random_next -1
proc my-random {max} {
    global pseudo_random pseudo_random_next
    if {$pseudo_random} {
	set pseudo_random_next [expr $pseudo_random_next * 1103515245 + 12345]
	return [expr ($pseudo_random_next/65536) % $max]
	# Or, use bash's random routine instead...
	# return [expr [exec bash -c {echo $RANDOM}] % $max]
    } else {
	return [random $max]
    }
}
proc init-random {num} {
    global pseudo_random pseudo_random_next
    if {$pseudo_random} {
	set pseudo_random_next $num
    } else {
	random seed $num
    }
}
