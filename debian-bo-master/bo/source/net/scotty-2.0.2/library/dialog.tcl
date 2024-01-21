##
## dialog.tcl
##
## This file contains some useful dialogs that I use frequently to
## implement some nice scwish scripts. Most of them are derived from
## the Tkined sources.
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


# Dialog_Center --
#
# Compute the geometry of a dialog window w based on the geometry
# of the toplevel window top. Unfortunately we can not get the
# size before a window is mapped.
#
# Arguments:
# w -		The dialog window that should be centered.
# top -		The optional toplevel over which the dialog is centered.

proc Dialog_Center {w {top ""}} {

    if {$top == ""} {
	set top [winfo parent $w]
    }
    set top [winfo toplevel $top]
    wm withdraw $w
    update idletasks

    if {$top == "."} {
	set x [expr [winfo screenwidth $w]/2 - [winfo reqwidth $w]/2 \
		- [winfo vrootx [winfo parent $w]]]
	set y [expr [winfo screenheight $w]/2 - [winfo reqheight $w]/2 \
		- [winfo vrooty [winfo parent $w]]]
    } else {
	set rx [expr {[winfo rootx $top]+[winfo vrootx $top]}]
	set ry [expr {[winfo rooty $top]+[winfo vrooty $top]}]
	
	set cx [expr $rx+[winfo width $top]/2]
	set cy [expr $ry+[winfo height $top]/2]
	
	set x  [expr $cx-[winfo reqwidth $w]/2]
	set y  [expr $cy-[winfo reqheight $w]/2]
	
	if {$x < 0} { set x 0 }
	if {$y < 0} { set y 0 }
    }
	
    wm geometry $w +$x+$y
    wm deiconify $w    
}


# Dialog_Wait --
#
# Wait until the dialog window is destroyed. This proc makes sure
# to re-install the grab and focus.
#
# Arguments:
# w -		The toplevel window of the dialog.

proc Dialog_Wait w {
    set oldFocus [focus]
    set oldGrab [grab current $w]
    if {$oldGrab != ""} {
        set grabStatus [grab status $oldGrab]
    }
    grab $w
    tkwait visibility $w
    focus $w
    tkwait window $w
    catch {focus $oldFocus}
    if {$oldGrab != ""} { 
	if {$grabStatus == "global"} {
	    grab -global $oldGrab
	} else {
	    grab $oldGrab
	}
    }
}


# Dialog_Toplevel --
#
# Create a toplevel window that can be used to build dialogs.
#
# Arguments:
# w -		The toplevel window to create.

proc Dialog_Toplevel w {
    catch {destroy $w}
    toplevel $w
    wm title $w [winfo name $w]
    wm protocol $w WM_DELETE_WINDOW { }
    wm transient $w [winfo toplevel [winfo parent $w]]
}


# Dialog_Text --
#
# Create the text describing the dialog.
#
# Arguments:
# w -		The dialog window.
# bitmap -	The bitmap to show on the left side of the text.
# text -	The text to display.

proc Dialog_Text {w bitmap text} {
    frame $w.top -relief raised -bd 1
    catch {
	label $w.top.b -bitmap $bitmap
	pack $w.top.b -side left -padx 3m -pady 3m
    }
    label $w.top.l -text $text
    pack $w.top.l -fill x -padx 2m -pady 2m
    pack $w.top -fill x
}


# Dialog_Buttons --
#
# Most dialogs have a list of buttons in the bottom. This proc
# creates these buttons and makes the first one the default.
#
# Arguments:
# w -		The dialog window.
# buttons -	The list of button names.

proc Dialog_Buttons {w buttons} {
    frame $w.bot -relief raised -bd 1
    frame $w.bot.0 -relief sunken -border 1
    pack $w.bot.0 -side left -expand yes -padx 8 -pady 8
    set arg [lindex $buttons 0]
    button $w.bot.0.button -text $arg \
	    -command "[list set result $arg] ; destroy $w"
    pack $w.bot.0.button -expand yes -padx 2 -pady 2 -ipadx 2
    bind $w <Return> "$w.bot.0.button invoke"
    focus $w
    
    set i 1
    foreach arg [lrange $buttons 1 end] {
	button $w.bot.$i -text $arg \
		-command "[list set result $arg] ; destroy $w"
	pack $w.bot.$i -side left -expand yes -padx 10 -ipadx 2
	incr i
    }
    pack $w.bot -fill x
}


# Dialog_Confirm --
#
# Confirm a message by pressing one button of the button list.
#
# Arguments:
# w -		The dialog window.
# bitmap -	The bitmap to show on the left side of the dialog.
# text -	The text to display right beside the bitmap.
# buttons -	A list of buttons to show in the bottom of the dialog.

proc Dialog_Confirm {w bitmap text buttons} {
    global result
    Dialog_Toplevel $w
    Dialog_Text $w $bitmap $text
    Dialog_Buttons $w $buttons
    Dialog_Center $w
    Dialog_Wait $w
    return $result
}


# Dialog_Browse
#
#
#
# Arguments:
# w -		The dialog window.
# title -	The title to display above the browser.
# text -	The text to browse.
# buttons -	A list of buttons to show in the bottom of the dialog.

proc Dialog_Browse {w title text buttons} {
    global result
    Dialog_Toplevel $w
    Dialog_Text $w "" $title
    frame $w.box -relief raised -bd 1
    scrollbar $w.box.scroll -command "$w.box.text yview" -relief sunken
    text $w.box.text -yscroll "$w.box.scroll set" -relief sunken
    $w.box.text insert 0.0 $text 
    $w.box.text configure -state disabled
    pack $w.box.scroll -side right -fill y
    pack $w.box.text -fill both -expand true
    pack $w.box -expand true -fill both
    Dialog_Buttons $w $buttons
    Dialog_Center $w
    Dialog_Wait $w
    return $result
}


# Dialog_Request --
#
# Request a simple line of input from the user.
#
# Arguments:
# w -		The dialog window.
# bitmap -	The bitmap to show on the left side of the dialog.
# text -	The text to display right beside the bitmap.
# value -	The default value to be edited by the user.
# buttons -	A list of buttons to show in the bottom of the dialog.

proc Dialog_Request {w bitmap text value buttons} {
    global result result$w
    Dialog_Toplevel $w
    Dialog_Text $w $bitmap $text
    entry $w.top.e -textvariable result$w
    set result$w $value
    bind $w.top.e <Return> "$w.bot.0.button invoke; break"
    pack $w.top.e -fill both -padx 3m -pady 3m
    Dialog_Buttons $w $buttons
    Dialog_Center $w
    Dialog_Wait $w
    return [list $result [set result$w]]
}


# Dialog_Select --
#
# Select an element out of a list of elements using a listbox.
#
# Arguments:
# w -		The dialog window.
# bitmap -	The bitmap to show on the left side of the dialog.
# text -	The text to display right beside the bitmap.
# list -	The list of elements presented to the user.
# buttons -	A list of buttons to show in the bottom of the dialog.

proc Dialog_Select {w bitmap text list buttons} {
    global result result$w
    Dialog_Toplevel $w
    Dialog_Text $w $bitmap $text
    frame $w.box -relief raised -bd 1
    scrollbar $w.box.scroll -command "$w.box.list yview" -relief sunken
    listbox $w.box.list -yscroll "$w.box.scroll set" -relief sunken
    foreach elem $list {
        $w.box.list insert end $elem
    }
    set result$w ""
    bind $w.box.list <ButtonRelease-1> \
	    "+set result$w \[%W get \[%W curselection\]\]"
    bind $w.box.list <Double-Button-1> "$w.bot.0.button invoke; break"
    pack $w.box.scroll -side right -fill y
    pack $w.box.list -side left -expand true -fill both
    pack $w.box -expand true -fill both
    Dialog_Buttons $w $buttons
    Dialog_Center $w
    Dialog_Wait $w
    return [list $result [set result$w]]
}


# Dialog_FileSelect --
#
# A very ugly file selector box. I really should pick up a nice one from
# the net.
#
# Arguments:
# w -		The dialog window.
# text -	The text to display above the file selector box.
# dir -		The directory name where we start.
# file -	The file name we used to start.

proc Dialog_FileSelect {w {text {Select a file:}} {dir {}} {filename {}}} {
    global result result$w
    if {[file isfile $dir] || ![file exists $dir]} {
	set dir [pwd]
    }
    if {($dir == "") || ($dir == ".")} { 
	set dir [pwd] 
    }
    Dialog_Toplevel $w
    frame $w.box -relief raised -bd 1
    label $w.box.label -text $text
    entry $w.box.entry -relief sunken
    scrollbar $w.box.scroll -relief sunken \
	-command "$w.box.filelist yview"
    listbox $w.box.filelist -yscroll "$w.box.scroll set" \
	-relief sunken -width 26 -height 14
    pack $w.box.label -side top -expand true
    pack $w.box.entry -side top -padx 10 -pady 10 -expand true
    pack $w.box.scroll -side right -fill y
    pack $w.box.filelist -side left -fill both -expand true
    pack $w.box -side top -expand true -fill both
    Dialog_Buttons $w "select cancel"

    bind $w <Return> "$w.bot.0.button invoke; break"
    bind $w.box.filelist <Button-1> \
	    "%W selection set \[%W nearest %y\];
         $w.box.entry delete 0 end;
         $w.box.entry insert 0 \[%W get \[%W nearest %y\]\]"
    bind $w.box.filelist <Double-Button-1> \
	    "%W selection set \[%W nearest %y\]; $w.bot.0.button invoke; break"
    bind $w.box.filelist <ButtonRelease-1> \
	"%W selection set \[%W nearest %y\];
         $w.box.entry delete 0 end;
         $w.box.entry insert 0 \[%W get \[%W nearest %y\]\]"

    Dialog_FileSelectBrowse $w $dir $filename
    Dialog_Center $w
    Dialog_Wait $w
    if {$result == "select"} {
	return [set result$w]
    }
    return ""
}


# Dialog_FileSelectBrowse --
#
# This one is a part of the file selector. It gets called whenever 
# something is selected in the filebrowser. This can happen by 
# pressing the select button, by double clicking in the listbox 
# or by typing <Return> in the entry widget.
#
# Arguments:
# w -		The dialog window.
# dir -		The current directory name.
# file -	The current file name.

proc Dialog_FileSelectBrowse {w dir file} {

    set file [string trimright $file "/"]
    switch -glob $file {
	""      {}
	".."    { set dir [file dirname $dir] }
	"~*"    { set dir $file }
	"/*"    { set dir $file }
	default { append dir "/$file" }
    }

    # accept URL definitions

    if {[string match "ftp://*" $file] || [string match "file://*" $file]} {
	global tkined_file_select_result
        set tkined_file_select_result $file
        destroy [winfo toplevel $w.box.filelist]
        return
    }
    
    regsub "//" $dir "/" dir

    $w.box.entry delete 0 end
    if {[file isfile $dir] || ![file exists $dir]} {
	global result result$w
	set result select
	set result$w $dir
	destroy [winfo toplevel $w.box.filelist]
	return
    }

    $w.box.filelist delete 0 end
    if {$dir != "/"} { $w.box.filelist insert end ".." }
    if {[catch {glob $dir/*} files]} { set files "" }
    foreach f [lsort $files] {
	if {[file isdirectory $f]} {
	    $w.box.filelist insert end "[file tail $f]/"
	} else {
	    $w.box.filelist insert end [file tail $f]
	}
    }

    $w.bot.0.button configure \
	-command "Dialog_FileSelectBrowse $w $dir \[$w.box.entry get\]"
}

