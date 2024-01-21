# Text Editor with MiniView

# ================================================================
# Help description
set application(title) {Text Editor with Miniview}

set application(description) {
{normalbold} {Shared Worked Space:
}
{normal} {The shared work space is on the left most side of the \
window.  This is a primitive text editor which is shared by a \
group of people. It can \
be scrolled with either the multi-user scroll bar, or by pressing the }
{normalitalic} {middle mouse button.

}
{normalbold} {Multi-user Scroll Bar:
}
{normal} {The multi-user scroll bar is located on the right most side of \
the window.  It consists of two parts; the user's actual scroll bar and \
the scroll bar indicators.  The user's actual scroll bar scrolls the shared \
work space. The scroll bar indicators control nothing, their only purpose \
is to  indicate what each user is currently viewing.  Press the }
{normalitalic} {left mouse button}
{normal} { on a scroll bar indicator to see who it is.

}
{normalbold} {Miniature View:
}   
{normal} {The miniature view is positioned between the shared work space \
and the multi-user scroll bar.  The miniature view displays a \
miniature of the shared work space.  Within the miniature view there are \
highlighted sections, one for each user, that indicate what that user's is \
currently viewing in the shared work space.  
Press the }
{normalitalic} {left mouse button}
{normal} { on a highlighted section to see who it is.  
The }
{normalitalic} {middle mouse button}
{normal} {will scroll the entire minature view - this is useful when you \
need to find where all the users are.  
The}
{normalitalic} {right mouse button}
{normal} {will move your highlighted section;  the shared \
work space will adjust its view as you move.

}
{normalbold} {File Menu:
}
{normal} {The }
{normalitalic} {File }
{normal} {menu will let you save ideas to a file, open an existing file,\
 clear the ideas from the display, or quit the application.

}
{normalbold} {Edit Menu:
}
{normal} {The }
{normalitalic} {Edit }
{normal} {menu will let you cut, copy and paste text in the shared \
work space.  currently there are no short-cut key bindings for these \
operations.

}
{normalbold} {Limitations:
}
{normal} {Control characters are handled poorly.}}



# =============================================================
# Do some standard groupkit setups...

# Initialize conference
gk_initConf $argv

# Tell all instances of this application to insert the idea (if it 
# isn't an empty string) into the editor.
proc insert_idea_into_editor {} { global myIdea
    if {$myIdea != ""} {
        gk_toAll actually_insert_idea_into_editor $myIdea
        set myIdea ""
    }
}

# Actually insert the idea into the editor and the miniature view
proc actually_insert_idea_into_editor {idea} { 

    .middle.editor insert end "$idea
"
    .middle.mini_text insert end "$idea
"
}

# ================================================================
# Update the entering participant with the state of the conference;
# send all the existing ideas into the new user's editor.

gk_bind updateEntrant "updateEntrant %U"
proc updateEntrant entrantUsernum { 
    set last [lindex [split [.middle.editor index end] .] 0]
    for {set count 1.0} {$count <= $last} {set count [expr $count + 1]} {
	set idea [.middle.editor get $count "$count lineend"]
        gk_toUserNum $entrantUsernum \
	    actually_insert_idea_into_editor $idea 
    }
}

# ================================================================
# These routines are invoked by the commands in the "File" menu.
# They save the ideas in a file, or open a new set of 
# previously saved ideas, or clears the ideas in all participant's editor.

# Saves contents of editor in file.  The FSBox routine asks the user which
# file to save into, the file is opened and the editor lines are put in.
# If the file already exists, the data is appendend onto it.
proc Save {} { 
    if {[.middle.editor size] == 0} {return}
    set fileName [FSBox]
    if {$fileName == ""} {return}
    set fd [open $fileName a]
    set last [lindex [split [.middle.editor index end] .] 0]
    for {set count 1.0} {$count < $last } {set count [expr $count + 1]} {
	puts $fd [.middle.editor get $count "$count lineend"]
    }
    close $fd
}

# Gets contents of a file and displays them, line by line, in the
# editors of ALL the conference processes.
proc Open {} {
    set fileName [FSBox]
    if {$fileName == ""} {return}
    Clear
    set fd [open $fileName r]
    set result [gets $fd temp]
    while {$result != -1} {
	gk_toAll actually_insert_idea_into_editor $temp
	set result [gets $fd temp]
    }
    close $fd
}    

# Clears ALL the editor and miniature views..
proc Clear {} { 
    gk_toAll doClear 
}

# Clear a editor and the miniature view.
proc doClear {} { 
    .middle.editor delete 0.0 end
    .middle.mini_text delete 0.0 end
}

# ================================================================
# These routines are invoked by the commands in the "Edit" menu.
# They Cut or Copy a highlighted portion of the text, or they Paste
# what is currently in the buffer at the current position.

proc Cut {} {
    global buffer

    set range [.middle.editor tag nextrange sel 1.0 end]
    if { $range != "" } {
	set buffer [eval .middle.editor get $range]
	gk_toAll eval .middle.editor delete $range
	gk_toAll eval .middle.mini_text delete $range
    } 
}

proc Copy {} {
    global buffer

    set range [.middle.editor tag nextrange sel 1.0 end]
    if { $range != "" } {
	gk_toAll set buffer [eval .middle.editor get $range]
    } else
}

proc Paste {} {
    global buffer
    set idx [.middle.editor index insert]
    if [info exists buffer] {
	gk_toAll .middle.editor insert $idx "$buffer"
	gk_toAll .middle.mini_text insert $idx "$buffer"
    }
}


# Tell the window manager the title and the default window size
wm title . $application(title)
wm minsize . 20 7

# Create the default groupkit pulldown menu bar 
# Then add Clear, Open, and Save items to the "File" menu, and help. 
gk_defaultMenu .menubar
.menubar itemcommand help add command -label "$application(title)" \
                          -command "gk_topicWindow .helpWindow \
                                    -height 12 \
                                    -title {$application(title)} \
                                    -text {$application(description)}"
.menubar add editMenu 1 -text Edit
.menubar itemcommand 1 add command -label "Cut" -command "Cut"
.menubar itemcommand 1 add command -label "Copy" -command "Copy"
.menubar itemcommand 1 add command -label "Paste" -command "Paste"

.menubar itemcommand 0 insert 1  command -label "Open" -command "Open"
.menubar itemcommand 0 insert 2 command -label "Save" -command "Save"
.menubar itemcommand 0 insert 3 command -label "Clear" -command "Clear"

pack .menubar -side top -fill x

# ================================================================
# Build the Application

# Create a frame in the window's middle which has a scrollable text editor
# containing the shared ideas. 
# Also there is a miniature view between the editor and the scrollbar.
# The portion being viewed in the editor is highlighted in the miniature
# view.

frame .middle -bd 2 -relief raised
gk_scrollbar .middle.vscroll -command ".middle.editor yview"  \
	-repeatdelay 1000 
gktext .middle.editor -height 12 -width 20 -wrap none \
    -setgrid true -exportselection true \
    -xscrollcommand ".middle.mini_text minixview" \
    -yscrollcommand ".middle.mini_text miniyview"

gk_miniText .middle.mini_text -width 100 \
	-textid [.middle.vscroll cget -scrollid]  \
        -xviewscrollcommand ".middle.editor xview" \
        -yviewscrollcommand ".middle.editor yview"

gk_duplicateText .middle.editor .middle.mini_text

pack .middle -side top -expand yes -fill both
pack .middle.vscroll -side right -fill y
pack .middle.editor -side left -fill both -expand yes
pack .middle.mini_text -side left -fill both -expand no



# its fairly easy to be typing quickly enough so that the screen
# never gets a chance to refresh; this code ensures we redraw at
# least once a second

proc idle_update {} {
	update idletasks
	after 1000 idle_update
}

after 1000 idle_update


