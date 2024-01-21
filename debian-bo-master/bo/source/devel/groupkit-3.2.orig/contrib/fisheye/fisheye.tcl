# Fisheye Fiewer: A relaxed WYSIWIS FileViewer that provides
# awareness of others locations in the file through fisheye lens
# ================================================================
# Created by Saul Greenberg
# Last modified June 4 1995 

# Help Description to be added to the help menu
#----------------------------------------------
set application(title) "Fisheye Viewer"

set application(description) {
{normal} {This is a prototype Fisheye Lens shared file viewer, constructed\
 by Saul Greenberg. It is being used as a proof of concept system\
 to test and demonstrate how fisheye views can be used to support\
 awareness of what others are doing in the shared view.

You and others can view a file through the shared view.\
 Fisheye lens are used to magnify the text around your area of interest,\
 as well as where other people are looking.

Open a file by the Open option in the file menu.

Change your area of interest by the slider on the bottom,\
 or by clicking on some text with the right mouse button.

Select the } 
{normalitalic} {Your Fisheye Lens} 
{normal} { option in the view menu to change the\
 text size and its extent around your fisheye, as well as the font size\
 of the non-magnified text.

Select the } 
{normalitalic} {Others Fisheye Lens} 
{normal} { option in the view menu to change the \
 text size and its extent around other participants areas of interest.

Try the following:
-open a large file (say several hundred lines) 
-play with your own fisheye view
-change the shape of your fisheye envelope
-start another viewer or two
-change their fisheye envelope

Bugs.
The widget for setting your fisheye lens should show the initial lens\
 properties being applied, but it doesnt.
The code is alot uglier than need be, and should be redesigned for\
 greater flexibility. But it works!
}}


# Globals
# -------

# Font and Default Fisheye configuration
set fish(mag_factor)  \
 { {60 0} {50 0} {40 0} {30 0} {24 0} {20 0} {18 0} {16 0} {14 0}  {12 1} {10 0} {8 4} {6 0}}


set fish(default_fontsize) 2 
set fish(others_size) 10
set fish(others_range) 1

set fish(global_focus) 1
set fish(others_being_displayed) {}

set fish(intro_message) \
"This is a prototype fisheye file viewer. 
Open a large file to see it via a fisheye view.
Other people's view into the document will be displayed as a fisheye view as well. 
Try changing the fisheye magnification and extent through the View menu."


# Widget names
set fish(text_widget) ""
set fish(scale) ""
set fish(others,id) {}
set fish(others,line) {}
set fish(others,color) {}


# Fisheye Display Routines
# -----------------------

# Given a mouse cursor position, find the index and center the view around it.
proc Redisplay {w x y} {
    CenterView $w [GetLineNumber [$w index @$x,$y]] false
}

# Given a line number, center the view around it
proc CenterFromScale {line} {
    CenterView .f.t $line true
}

# Given a line index, center the view around it. 
# (This routine could be cleaned up)
proc CenterView {w line_number force_update} {
    global fish

    # If the fisheye is already centered on this line,do nothing
    if {$line_number == $fish(global_focus) && $force_update == "false"} {
	return
    }

    # Remove all the font tags
    RemoveLocalFontTags $w

    UpdateOthers

    set offset 0
    foreach pair $fish(mag_factor) {
	set range [lindex $pair 1]
	# Zero range can be ignored
	if {$range == 0} {continue}
	set size [lindex $pair 0]
        set thistag localfont$size

        # Do the center lines
        if {$offset == 0} {
	    set from [expr $line_number - $range + 1]
	    set to [expr $line_number + $range - 1]
            SetLineTags $w $thistag false $from $to $size 
	    incr offset $range
        } else {
            #Do the lines before
	    set from [expr $line_number - $offset - $range + 1]
	    set to [expr $line_number - $offset] 
            SetLineTags $w $thistag false $from $to $size 

	    #Do the lines after 
	    set from [expr $line_number + $offset ]
	    set to [expr $line_number + $offset + $range - 1 ]
            SetLineTags $w $thistag false $from $to $size 

	    incr offset $range
        }
    }
    # Now remember what line the focus is on, and tell the other participants
    set fish(global_focus) $line_number
    if {[$fish(scale) get] != $line_number} {$fish(scale) set $line_number}
    tellOthers  $w $fish(global_focus)
}

# Tell the other participants about where we are, etc.
proc tellOthers {w focus} {
    set id [users local.usernum]
    set color [userprefs color]
    gk_toOthers setOthersFocus $w $id $color $focus
}

# Utility routines for Tags
#--------------------------
#Create font descriptions for Tags
proc createFontTags {w} {
    global fish
    foreach pair $fish(mag_factor) {
       set size [lindex $pair 0]
       $w tag configure localfont$size \
	   -font [FontName $size]
    }
    return [llength $fish(mag_factor)]
}

# Remove all the local font tags from the text
proc RemoveLocalFontTags {w} {
    foreach thistag [$w tag names] {
        if { [string first localfont $thistag] != -1} {
           $w tag remove $thistag 1.0 end
        }
    }
}
# Attach the tag to the line numbers starting at "from" and going to "to",
proc SetLineTags {w tag colortag from to size} {
   # Make sure that the line numbers are contained in the text widget
   set last_line [GetLineNumber [$w index end]]
   if {$from <= 0 && $to <= 0} {return}

   if {$from <= 0} {set from 1}
   if {$to > $last_line} {set to $last_line}

   # Always add the colortag, if it exists
   if {$colortag != "false"} {
       $w tag add $colortag "$from.0 linestart" "$to.0 lineend + 1 chars"
   }

   # Add the tag to the range
   for {set line $from} {$line <= $to} {incr line} {
       set current_size [LineFontSize $w $line]
       if  {$current_size < $size} {
	   set oldTag [LineFontTag $w $line]
           if {$oldTag  != ""} { 
		$w tag remove $oldTag "$line.0" "$line.0 lineend + 1 chars"
	   }
           $w tag add $tag "$line.0 linestart" "$line.0 lineend + 1 chars"
       }
   }
}

# Find the font size of the current line
proc LineFontSize {w line} {
    global fish	
    set fontsize $fish(default_fontsize) 
    foreach tag [$w tag names $line.0] {
	set idx [string first font $tag]
        if { $idx != -1} {
		set fontsize [string range $tag [expr $idx+4] end]
		if {$fontsize == "others"} {set fontsize $fish(others_size)}
        }
    }
    return $fontsize
}

# Find the font size of the current line
proc LineFontTag {w line} {
    set tagname "" 
    foreach tag [$w tag names $line.0] {
	set idx [string first font $tag]
        if { $idx != -1} {
		set tagname $tag
        }
    }
    return $tagname
}

# Utility routines for text widget
#---------------------------------

# Given an index, return the line number
proc GetLineNumber {idx} {
    return [string range $idx 0 [expr [string first "." $idx] - 1 ]]
}

# Given a font size, construct a font name from it
proc FontName {size} {
    return [concat -*-lucida-medium-r-*-*-$size-*-*-*-*-*-*-*]
}

# GroupKit Callbacks
#-------------------

proc setOthersFocus {w id color line} {
   global fish

    #A hack structure viewer for tcl programs
    ##The view onto others text will display the line prior to this one containing the string (if there is one)
#    if {$fish(use_structure)} {
#	puts $fish(structure_string)
#	set idx [$fish(text_widget) search -backwards -regexp $fish(structure_string) "$line.0 lineend"]
#	if {[GetLineNumber $idx] < $line} {set line [GetLineNumber $idx]}
#    }
   
   set usertag user$id
   set size  $fish(others_size)
   set range $fish(others_range)
   if {[lsearch [$w tag names] ${usertag}color] == -1 } {
       $w tag configure ${usertag}color -background $color 
       $w tag configure ${usertag}fontothers  -font [FontName $size]	
   }
   $w tag remove ${usertag}color 1.0 end
   $w tag remove ${usertag}fontothers 1.0 end

   set where [lsearch $fish(others,id) $id]
   if {$where == -1} {
	lappend fish(others,id) $id
	lappend fish(others,line) $line
	lappend fish(others,color) $color
   } else {
        set fish(others,line) [lreplace $fish(others,line) $where $where $line]
        set fish(others,color) [lreplace $fish(others,line) $where $where $color]
   }
   SetLineTags $w ${usertag}fontothers ${usertag}color \
	   [expr $line-$range]  [expr $line+$range] $size
}

proc UpdateOthers {args} {
    global fish
    set idx 0
    foreach person  $fish(others,id) {
	setOthersFocus $fish(text_widget) $person [lindex $fish(others,color) $idx] [lindex $fish(others,line) $idx]
	incr idx
    }
}

proc DeleteOther {id} {
    global fish

    # First remove the information about the other person from the others data structure
    set idx [lsearch $fish(others,id) $id]
    set fish(others,id)    [lreplace $fish(others,id) $idx $idx]
    set fish(others,line)  [lreplace $fish(others,line) $idx $idx]
    set fish(others,color) [lreplace $fish(others,color) $idx $idx]

    # Now remove the tags representing the other person
    set usertag user$id
    $fish(text_widget) tag remove ${usertag}color 1.0 end
    $fish(text_widget) tag remove ${usertag}fontothers 1.0 end
}

proc ReConfigureFont {} {
    global fish	
    foreach person  $fish(others,id) {
	$fish(text_widget) tag configure user${person}fontothers -font [FontName $fish(others_size)]
    }
}

proc DisplayValues {args} {
    global fish
    foreach pair $fish(mag_factor) {
        set size  [lindex $pair 0]
	lappend list [list $size $fish(scale,$size)]
    }
    set fish(mag_factor) $list
    CenterView $fish(text_widget) $fish(global_focus) true
}

# Groupkit Enter and Leaves
#---------------------------

# Add a callback so new conference entrants can be update, and departing ones destroyed
gk_on {[gk_event type]=="updateEntrant"} {updateEntrant [gk_event usernum]}
gk_on {[gk_event type]=="userDeleted"} {DeleteOther [gk_event usernum]}

proc updateEntrant {usernum} {
    global fish
    gk_toUserNum $usernum InsertFile [$fish(text_widget) get 1.0 end]
    gk_toUserNum $usernum setOthersFocus \
	    $fish(text_widget) [users local.usernum] [userprefs color] $fish(global_focus)
    set idx 0
    foreach person  $fish(others,id) {
	gk_toUserNum $usernum setOthersFocus \
		$fish(text_widget) $person \
		[lindex $fish(others,color) $idx] \
		[lindex $fish(others,line)  $idx] 
	incr idx
    }
}


# Interface components
# ---------------------

# This scale lets people control the focal point of the fisheye
proc MakeScale {w} {
    scale $w.s -orient horizontal \
	-from 1 -to [GetLineNumber [.f.t index end]] \
	-showvalue yes -width 8 
    $w.s set 1
    $w.s configure -command "CenterFromScale"
    pack $w.s -side bottom -fill x 
    return $w.s
}
# Procs for File Manipulation
#=====================
# Get contents of a file and display them, line by line, in the
# listboxes of ALL the conference processes.
proc OpenFile {} {
    set fileName [FSBox]
    if {$fileName == ""} {return}
    set fd [open $fileName r]
    set result [read $fd]
    gk_toAll InsertFile $result
    close $fd
}    

# Tell all application instances to insert the line into the shared list.
proc InsertFile {text} {
    global fish

    # Clear the widget and insert the new text
    $fish(text_widget) delete 1.0 end
    $fish(text_widget) insert 1.0 $text

    # Reset all the focuses to the first line
    set fish(global_focus) 1
    set tmplist {}
    foreach pair $fish(others,line) {
	lappend tmplist 1
    }
    set fish(others,line) $tmplist

    # Set the scale the new file size
    $fish(scale) configure -to \
	    [GetLineNumber [$fish(text_widget) index end]]

    # Update the display
    CenterView $fish(text_widget) $fish(global_focus) true
}

# To put in debug statements; not used.
proc xx {function args} {
    puts [concat [users local.usernum]  "-" $function ": " $args] 
}

#================
# Main
#================
gk_initConf $argv

source [userprefs scriptpath]/poly.tcl
source [userprefs scriptpath]/others_controls.tcl

gk_defaultMenu .menubar
wm title . "Fisheye File Viewer"
wm iconname . "Fisheye Viewer"
wm minsize .  200 400
wm geometry . 230x400
pack .menubar -side top -fill x

.menubar itemcommand help add command \
	-label "$application(title)" \
	-command "gk_topicWindow .helpWindow \
	             -title \"$application(title)\" \
                     -text \"$application(description)\""

.menubar itemcommand 0 add separator
.menubar itemcommand 0 add command -label "Open" -command "OpenFile"

.menubar add fisheye_controls 1 -text View
.menubar itemcommand 1 add command -label "Your Fisheye Lens" -command "MakeTopLevel"
.menubar itemcommand 1 add command -label "Others Fisheye Lens" -command "MakeOthersControls"

#Create a frame
frame .f
pack .f -fill both -expand yes

#Create the text widget
set fish(text_widget) .f.t
scrollbar .f.scroll -command ".f.t yview"
text .f.t -wrap none \
     -yscrollcommand ".f.scroll set" \
     -font [FontName $fish(default_fontsize)] 
pack .f.scroll -side right -fill y 
pack .f.t -fill both -expand yes

set fish(numb_fonts) [createFontTags .f.t ]

# Add some lines to the view 
$fish(text_widget) insert 1.0 $fish(intro_message) 

frame .fscale
set fish(scale) [MakeScale .fscale]
pack .fscale -side bottom -fill x

#Create the fisheye around the first line
CenterView .f.t 1 true

#The fisheye will readjust to the button 1 press and motion
bind .f.t <3> "Redisplay .f.t %x %y "
bind .f.t <B3-Motion> "Redisplay .f.t %x %y"

after {2000} {global fish; tellOthers $fish(text_widget) $fish(global_focus)}

