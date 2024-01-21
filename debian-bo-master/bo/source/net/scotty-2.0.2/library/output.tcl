##
## output.tcl
##
## This file contains the definition of an output window, which is basically
## a text widget with a scrollbars and a menu bar. The menu contains a set
## of standard commands to send email or to save/load the text to/from files.
## This is again derived from the Tkined sources.
##
## Copyright (c) 1995
##
## J. Schoenwaelder
## TU Braunschweig, Germany
## Institute for Operating Systems and Computer Networks
##
## Permission to use, copy, modify, and distribute this
## software and its documentation for any purpose and without
## fee is hereby granted, provided that this copyright
## notice appears in all copies.  The University of Braunschweig
## makes no representations about the suitability of this
## software for any purpose.  It is provided "as is" without
## express or implied warranty.
##


# Output_Create --
#
# Create an output window and define a proc named writeln to write
# to the text widget in this window. A hack, but a good one. :-)
#
# Arguments:
# w -		The output window that should be created.

proc Output_Create w {

    if {![winfo exists $w]} {
	toplevel $w
	frame $w.menu -borderwidth 1 -relief raised
	pack $w.menu -side top -fill x

	text $w.t -wrap none -highlightthickness 0 -setgrid true \
		-relief sunken -borderwidth 2 \
		-yscrollcommand "$w.yscroll set" \
		-xscrollcommand "$w.xscroll set"
	scrollbar $w.yscroll -orient vertical \
		-command "$w.t yview" -relief sunken
	scrollbar $w.xscroll -orient horizontal \
		-command "$w.t xview" -relief sunken
	pack $w.t -side left -padx 2 -pady 2 -fill both -expand true 

	menubutton $w.menu.file -text "File" -menu $w.menu.file.m
	menu $w.menu.file.m

	$w.menu.file.m add command -label "Clear" \
		-accelerator "  Alt+C" \
		-command "Output_Clear $w"
	bind $w <Alt-c>  "$w.menu.file.m invoke Clear"
	bind $w <Alt-C>  "$w.menu.file.m invoke Clear"
	bind $w <Meta-c> "$w.menu.file.m invoke Clear"
	bind $w <Meta-C> "$w.menu.file.m invoke Clear"

	$w.menu.file.m add command -label "Open..." \
		-accelerator "  Alt+O" \
		-command "Output_Load $w"
	bind $w <Alt-o>  "$w.menu.file.m invoke Open..."
	bind $w <Alt-O>  "$w.menu.file.m invoke Open..."
	bind $w <Meta-o> "$w.menu.file.m invoke Open..."
	bind $w <Meta-O> "$w.menu.file.m invoke Open..."
	$w.menu.file.m add command -label "Save As..." \
		-accelerator "  Alt+A" \
		-command "Output_Save $w"
	bind $w <Alt-a>  "$w.menu.file.m invoke {Save As...}"
	bind $w <Alt-A>  "$w.menu.file.m invoke {Save As...}"
	bind $w <Meta-a> "$w.menu.file.m invoke {Save As...}"
	bind $w <Meta-A> "$w.menu.file.m invoke {Save As...}"
	$w.menu.file.m add sep
	$w.menu.file.m add command -label "Print..." \
		-accelerator "  Alt+P" \
		-command "Output_Print $w"
	bind $w <Alt-p>  "$w.menu.file.m invoke Print..."
	bind $w <Alt-P>  "$w.menu.file.m invoke Print..."
	bind $w <Meta-p> "$w.menu.file.m invoke Print..."
	bind $w <Meta-P> "$w.menu.file.m invoke Print..."
	$w.menu.file.m add command -label "Email..." \
		-accelerator "  Alt+E" \
		-command "Output_EMail $w"
	bind $w <Alt-e>  "$w.menu.file.m invoke Email..."
	bind $w <Alt-E>  "$w.menu.file.m invoke Email..."
	bind $w <Meta-e> "$w.menu.file.m invoke Email..."
	bind $w <Meta-E> "$w.menu.file.m invoke Email..."
	$w.menu.file.m add sep
	$w.menu.file.m add command -label "New View" \
		-accelerator "  Alt+N" \
		-command "Output_NewView $w"
	bind $w <Alt-n>  "$w.menu.file.m invoke {New View}"
	bind $w <Alt-N>  "$w.menu.file.m invoke {New View}"
	bind $w <Meta-n> "$w.menu.file.m invoke {New View}"
	bind $w <Meta-N> "$w.menu.file.m invoke {New View}"
	$w.menu.file.m add command -label "Close View" \
		-accelerator "  Alt+W" \
		-command "Output_CloseView $w"
	bind $w <Alt-w>  "$w.menu.file.m invoke {Close View}"
	bind $w <Alt-W>  "$w.menu.file.m invoke {Close View}"
	bind $w <Meta-w> "$w.menu.file.m invoke {Close View}"
	bind $w <Meta-W> "$w.menu.file.m invoke {Close View}"
	pack $w.menu.file -side left

	menubutton $w.menu.find -text "Find" -menu $w.menu.find.m
	menu $w.menu.find.m
	$w.menu.find.m add command -label "Find..." \
		-accelerator "  Alt+F" \
		-command "Output_Find $w"
	bind $w <Alt-f>  "$w.menu.find.m invoke {Find...}"
        bind $w <Alt-F>  "$w.menu.find.m invoke {Find...}"
        bind $w <Meta-f> "$w.menu.find.m invoke {Find...}"
        bind $w <Meta-F> "$w.menu.find.m invoke {Find...}"
if {0} {
	$w.menu.find.m add separator
	$w.menu.find.m add command -label "Filter..." \
		-accelerator "  Alt+T" \
		-command "Output_EditFilter $w"
	bind $w <Alt-t>  "$w.menu.find.m invoke {Filter...}"
        bind $w <Alt-T>  "$w.menu.find.m invoke {Filter...}"
        bind $w <Meta-t> "$w.menu.find.m invoke {Filter...}"
        bind $w <Meta-T> "$w.menu.find.m invoke {Filter...}"
}
	pack $w.menu.find -side left

	menubutton $w.menu.options -text "Options" -menu $w.menu.options.m
	menu $w.menu.options.m
	$w.menu.options.m add checkbutton -label "Freeze" \
		-offvalue 0 -onvalue 1 -variable _output(freeze,$w) \
		-accelerator "  Alt+Z"
	$w.menu.options.m invoke Freeze
	$w.menu.options.m invoke Freeze
	bind $w <Alt-z>  "$w.menu.options.m invoke Freeze"
	bind $w <Alt-Z>  "$w.menu.options.m invoke Freeze"
	bind $w <Meta-z> "$w.menu.options.m invoke Freeze"
	bind $w <Meta-Z> "$w.menu.options.m invoke Freeze"
	$w.menu.options.m add checkbutton -label "Wrap" \
		-offvalue none -onvalue word -variable _output(wrap,$w) \
		-accelerator "  Alt+V" -command "Output_Wrap $w"
	bind $w <Alt-v>  "$w.menu.options.m invoke Wrap"
	bind $w <Alt-V>  "$w.menu.options.m invoke Wrap"
	bind $w <Meta-v> "$w.menu.options.m invoke Wrap"
	bind $w <Meta-V> "$w.menu.options.m invoke Wrap"
	$w.menu.options.m add separator
	$w.menu.options.m add checkbutton -label "X Scroll" \
		-offvalue 0 -onvalue 1 -variable _output(xscroll,$w) \
		-accelerator "  Alt+X" -command "Output_XScroll $w"
	bind $w <Alt-x>  "$w.menu.options.m invoke {X Scroll}"
	bind $w <Alt-X>  "$w.menu.options.m invoke {X Scroll}"
	bind $w <Meta-x> "$w.menu.options.m invoke {X Scroll}"
	bind $w <Meta-X> "$w.menu.options.m invoke {X Scroll}"
	$w.menu.options.m add checkbutton -label "Y Scroll" \
		-offvalue 0 -onvalue 1 -variable _output(yscroll,$w) \
		-accelerator "  Alt+Y" -command "Output_YScroll $w"
	$w.menu.options.m invoke "Y Scroll"
	bind $w <Alt-y>  "$w.menu.options.m invoke {Y Scroll}"
	bind $w <Alt-Y>  "$w.menu.options.m invoke {Y Scroll}"
	bind $w <Meta-y> "$w.menu.options.m invoke {Y Scroll}"
	bind $w <Meta-Y> "$w.menu.options.m invoke {Y Scroll}"
	pack $w.menu.options -side left

	static offset
	if {![info exists offset]} {
	    set offset 80
	} else {
	    incr offset 10
	    if {$offset > 180} {set offset 80}
	}

	wm withdraw $w
	update idletasks
	set top [winfo toplevel [winfo parent $w]]
	
	set rx [expr {[winfo rootx $top]}]
	set ry [expr {[winfo rooty $top]}]
	
	set cx [expr $rx+[winfo width $top]/4]
	set cy [expr $ry+[winfo height $top]/4]
	
	set x  [expr $cx+$offset]
	set y  [expr $cy+$offset]
	
	if {$x < 0} { set x 0 }
	if {$y < 0} { set y 0 }
	
	wm geometry $w +$x+$y
	wm deiconify $w
	
	update
    }

    Output_Writeln $w
}


# Output_NewView --
#
# Create a new output window (aka a new view).
#
# Arguments:
# w -		The currently existing output window.

proc Output_NewView w {
    set parent [winfo parent $w]
    for {set i 1} 1 {incr i} {
        set view $parent.view$i
        if ![winfo exists $view] {
            break
        }
    }
    Output_Create $view
}


# Output_CloseView --
#
# Close an output window.
#
# Arguments:
# w -		The output window that should be closed.

proc Output_CloseView w {
    destroy $w
}


# Output_Clear --
#
# Clear the contents of the output window.
#
# Arguments:
# w -		The output window that should get cleared.

proc Output_Clear w {
    $w.t delete 0.0 end
}


# Output_Writeln --
#
# Define a writeln proc that will write to the output window.
#
# Arguments:
# w -		The output window we should write to.

proc Output_Writeln w {
    proc writeln {{txt ""}} \
        "global _output; \
        $w.t insert end \$txt\\n; \
	if !\$_output(freeze,$w) \" $w.t yview -pickplace end\" \
	"
}


# Output_SetName --
#
# Set the name of the output window, which is displayed in the window
# decorations as well as in the icon.
#
# Arguments:
# w -		The output window that should get a new name.
# top -		The name to be used.

proc Output_SetName { w name } {
    wm title $w $name
    wm iconname $w $name
}


# Output_SetIcon --
#
# Set the icon of the output window.
#
# Arguments:
# w -		The output window that should get a new icon.
# bitmap -	The bitmap that whould appear in the icon.
#

proc Output_SetIcon { w bitmap } {
    wm iconbitmap $w $bitmap
}


# Output_Freeze --
#
# Toggle the freeze state of the output window.
#
# Arguments:
# w -		The output window that should be frozen/melted.

proc Output_Freeze w {
    if {[$w.menu.freeze cget -text] == "freeze"} {
	$w.menu.freeze configure -text melt
    } else {
	$w.menu.freeze configure -text freeze
    }
}


# Output_Wrap --
#
# Toggle the text wrap feature of the output window.
#
# Arguments:
# w -		The output window that should be wrapped/unwrapped.

proc Output_Wrap w {
    global _output
    $w.t configure -wrap $_output(wrap,$w)
}


# Output_XScroll --
#
# Toggle the horizontal scrollbar.
#
# Arguments:
# w -		The output window that should be toggled.

proc Output_XScroll w {
    global _output
    if $_output(xscroll,$w) {
	pack $w.xscroll -before $w.t -side bottom -fill x
    } else {
	pack forget $w.xscroll
    }
}


# Output_YScroll --
#
# Toggle the vertical scrollbar.
#
# Arguments:
# w -		The output window that should be toggled.

proc Output_YScroll w {
    global _output
    if $_output(yscroll,$w) {
	pack $w.yscroll -side right -fill y
    } else {
	pack forget $w.yscroll
    }
}


# Output_Save --
#
# Write the contents of the text widget into a file.
#
# Arguments:
# w -		The output window that should be saved.

proc Output_Save w {
    set fname [Dialog_FileSelect $w.r "Write to file:"]
    if {$fname==""} return
    set mode "w"
    if {[file exists $fname]} {
	set result [Dialog_Confirm $w.r info \
		"File $fname already exists!" [list replace append cancel]]
	switch [lindex $result 0] {
	    "cancel" {
		return
	    }
	    "replace" {
		set mode "w"
	    }
	    "append" {
		set mode "a"
	    }
	}
    }
    if {[catch {open $fname $mode} file]} {
	Dialog_Confirm $w.r error "Unable to open $fname." ok
	return
    }
    puts $file [$w.t get 1.0 end]
    close $file
}


# Output_Load --
#
# Read the contents of the output window from a file.
#
# Arguments:
# w -		The output window that should be read.

proc Output_Load w {
    set fname [Dialog_FileSelect $w.r "Read from file:"]
    if {$fname == ""} return
    if {[catch {open $fname r} file]} {
	Dialog_Confirm $w.r error "Unable to read from $fname" ok
	return
    }
    $w.t delete 0.0 end
    $w.t insert end [read $file]
    $w.t yview 1.0
    close $file
}


# Output_Print --
#
# Send the contents of the output window to the printer.
# 
# Arguments:
# w -		The output window that should be printed.

proc Output_Print { w } {
    global env
    set fname "/tmp/output-[pid]"
    catch {exec /bin/rm -f $fname}
    if {[file exists $fname] && ![file writable $fname]} {
	Dialog_Confirm $w.r error "Can not write temporary file $fname." ok
	return
    }
    if {[catch {open $fname w} file]} {
	Dialog_Confirm $w.r error "Can not open $fname: $file" ok
	return
    }

    if {[catch {puts $file [$w.t get 1.0 end]} err]} {
	Dialog_Confirm $w.r error "Failed to write $fname: $err" ok
    }
    close $file
    foreach dir [split $env(PATH) ":"] {
	if {[file executable $dir/lpr]} {
	    set lpr $dir/lpr
	    break
	}
    }
    set res [Dialog_Request $w.r questhead \
	    "Saved to temporary file $fname.\n\nEnter print command:" \
	    $lpr "print cancel" ]
    if {[lindex $res 0] == "print"} {
	set cmd [lindex $res 1]
	if {[catch {eval exec $cmd $fname} err]} {
	    Dialog_Confirm $w.r error "$lpr $fname failed:\n$err" ok
	}
    }
    catch {exec /bin/rm -f $fname}
}


# Output_EMail --
#
# Send the contents of the output window as an email message.
#
# Arguments:
# w -		The output window that should be mailed.

proc Output_EMail w {
    global env
    set res [Dialog_Request $w.r questhead \
	    "Please enter the email address:" "" "ok cancel"]
    if {[lindex $res 0] == "cancel"} return
    set to [lindex $res 1]
    if {$to == ""} {
	Dialog_Confirm $w.r warning "Ignoring empty email address." ok
        return
    }
    set res [Dialog_Request $w.r questhead \
	    "Please enter the subject of this email:" \
	    "mibtree output" "ok none cancel"]
    switch [lindex $res 0] {
	cancel return
	none {
	    set subject ""
	}
	ok {
	    set subject [lindex $res 1]
	}
    }
    if {[catch {split $env(PATH) :} path]} {
	set path "/usr/bin /bin /usr/ucb /usr/local/bin"
    }
    set mailer ""
    foreach m "Mail mail" {
	foreach dir $path {
	    set fname $dir/$m
	    if {[file executable $fname] && [file isfile $fname]} {
		set mailer $fname
		break
	    }
	}
	if {$mailer != ""} break
    }
    if {$mailer == ""} {
	Dialog_Confirm $w.r error "Sorry, can not find mail program." ok
	return
    }
    if {[catch {open "|$mailer -s \"$subject\" $to" w} file]} {
        Dialog_Confirm $w.r error "Unable to write to $mailer $to." ok
        return
    }
    puts $file [$w.t get 1.0 end]
    close $file
}


# Output_Find --
#
# Find a regular expression in the text of the output window.
#
# Arguments:
# w -		The output window where we should find a regular expression.

proc Output_Find w {
    global _output
    if {! [info exists _output(find,$w)]} {
	set _output(find,$w) ""
    }
    set result [Dialog_Request $w.find questhead \
	    "Find the following regular expression:" \
	    $_output(find,$w) "find clear cancel" ]
    if {[lindex $result 0] == "cancel"} return
    $w.t tag delete found
    if {[lindex $result 0] == "clear"} return
    set regexp [lindex $result 1]
    set _output(find,$w) $regexp
    set index "0.0"
    while {[set index [$w.t search -nocase -regexp $regexp $index end]] != ""} {
	$w.t tag add found "$index linestart" "$index lineend"
	set index [$w.t index "$index + 1 line linestart"]
    }
    $w.t tag configure found -relief raised -underline true
}


# Output_EditFilter --
#
# Edit the filter expression that defines which text is displayed in 
# the text widget of an output window.
#
# Arguments:
# w -		The output window that should get a new filter.

proc Output_EditFilter w {
    global _output
    if {! [info exists _output(filter,$w)]} {
	set _output(filter,$w) .
    }
    set result [Dialog_Request $w.filter questhead \
	    "Edit the regular filter expression:" \
	    $_output(filter,$w) "accept cancel" ]
    if {[lindex $result 0] == "cancel"} return
    set _output(filter,$w) [lindex $result 1]
}
