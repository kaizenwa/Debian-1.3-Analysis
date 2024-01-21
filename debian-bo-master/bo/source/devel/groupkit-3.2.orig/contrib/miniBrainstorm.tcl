# An Anonymous Brainstorming Tool

# ================================================================
# Help description
set application(title) {Brainstormer with Miniview}

set application(description) {
{normalbold} {Idea Area:
}
{normal} {This portion of the brainstormer is located at the \
very bottom of the window, where the cursor is.  Type an idea into \
the ideas area and hit return. 

}
{normalbold} {Shared Ideas List:
}
{normal} {The shared ideas list is on the left most side of the \
window.  The idea will display in the shared ideas list.  It can \
be scrolled with either the multi-user scroll bar, or by pressing the }
{normalitalic} {middle mouse button.

}
{normalbold} {Multi-user Scroll Bar:
}
{normal} {The multi-user scroll bar is located on the right most side of \
the window.  It consists of two parts; the user's actual scroll bar and \
the scroll bar indicators.  The user's actual scroll bar scrolls the shared \
idea list. The scroll bar indicators control nothing, their only purpose \
is to  indicate what each user is currently viewing.  
Press the }
{normalitalic} {left mouse button}
{normal} { on a scroll bar indicator to see who it is. 
The }
{normalitalic} { middle mouse button}
{normal} { will scroll the entire minature view - this is useful when you \
need to find where all the users are.  
The }
{normalitalic} {right mouse button}
{normal} { will move your highlighted section;  the shared \
ideas liste will adjust its view as you move.

}
{normalbold} {Miniature List:
}   
{normal} {The miniature list is positioned between the shared ideas \
list and the multi-user scroll bar.  The miniature list displays a \
miniature of the shared idea list.  Within the miniature list there are \
highlighted sections, one for each user, that indicate that a user is \
currently viewing in the shared ideasX list.  Press the }
{normalitalic} {left mouse button}
{normal} { on a highlighted section to see who it is.

}
{normalbold} {File Menu:
}
{normal} {The }
{normalitalic} {File }
{normal} {menu will let you save ideas to a file, open an existing file,\
 clear the ideas from the display, or quit the application.

}
{normalbold} {Limitations:
}
{normal} {Control characters are handled poorly.}}



# =============================================================
# Do some standard groupkit setups...

# Initialize conference
gk_initConf $argv

# Tell all instances of this application to insert the idea (if it 
# isn't an empty string) into the shared list.
proc insert_idea_into_shared_list {} { global myIdea
    if {$myIdea != ""} {
        gk_toAll actually_insert_idea_into_shared_list $myIdea
        set myIdea ""
    }
}

# Actually insert the idea into the shared list and the miniature view
proc actually_insert_idea_into_shared_list {idea} { 

    .middle.shared_list insert end $idea
    .middle.mini_text insert end "$idea
"
}

# ================================================================
# Update the entering participant with the state of the conference;
# send all the existing ideas into the new user's listbox.

gk_bind updateEntrant "updateEntrant %U"
proc updateEntrant entrantUsernum { 
    for {set count 0} {$count < [.middle.shared_list size]} {incr count 1} {
	set idea [.middle.shared_list get $count]
        gk_toUserNum $entrantUsernum \
	    actually_insert_idea_into_shared_list $idea 
    }
}

# ================================================================
# These routines are invoked by the commands in the "File" menu.
# They save the ideas in a file, or open a new set of 
# previously saved ideas, or clears the ideas in all participant's idea lists.

# Saves contents of listbox in file.  The FSBox routine asks the user which
# file to save into, the file is opened and the listbox elements are put in.
# If the file already exists, the data is appendend onto it.
proc Save {} { 
    if {[.middle.shared_list size] == 0} {return}
    set fileName [FSBox]
    if {$fileName == ""} {return}
    set fd [open $fileName a]
    for {set count 0} {$count < [.middle.shared_list size]} {incr count 1} {
	puts $fd [.middle.shared_list get $count]
    }
    close $fd
}

# Gets contents of a file and displays them, line by line, in the
# listboxes of ALL the conference processes.
proc Open {} {
    set fileName [FSBox]
    if {$fileName == ""} {return}
    Clear
    set fd [open $fileName r]
    set result [gets $fd temp]
    while {$result != -1} {
	gk_toAll actually_insert_idea_into_shared_list $temp
	set result [gets $fd temp]
    }
    close $fd
}    

# Clears ALL the listboxes and miniature views..
proc Clear {} { 
    gk_toAll doClear 
}

# Clear a listbox and the miniature view.
proc doClear {} { 
    .middle.shared_list delete 0 end
#    .middle.mini_text delete 0.0 end
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

.menubar itemcommand 0 insert 1 command -label "Open" -command "Open"
.menubar itemcommand 0 insert 2 command -label "Save" -command "Save"
.menubar itemcommand 0 insert 3 separator
.menubar itemcommand 0 insert 4 command -label "Clear Ideas" -command "Clear"
.menubar itemcommand 0 insert 5 separator
pack .menubar -side top -fill x 

# ================================================================
# Build the Application

# Create a frame in the window's middle which has a scrollable listbox
# containing the shared ideas. 
# The twist is that the scrollbar is actually the Groupkit multiuser scrollbar!
# Also their is a miniature view between the listbox and the scrollbar.
# The portion being viewed in the listbox is highlighted in the miniature
# view.

frame .middle -bd 2 -relief raised
gk_scrollbar .middle.vscroll -command ".middle.shared_list yview"  \
	-repeatdelay 100
	
listbox .middle.shared_list -height 12 \
    -setgrid true  \
    -xscrollcommand ".middle.mini_text minixview" \
    -yscrollcommand ".middle.vscroll set"

gk_miniText .middle.mini_text -width 100 \
	-yviewscrollcommand ".middle.shared_list yview" \
	-xviewscrollcommand ".middle.shared_list xview" \
	-textid [.middle.vscroll cget -scrollid]

pack .middle -side top -expand yes -fill both
pack .middle.vscroll -side right -fill x
pack .middle.shared_list -side left -fill both -expand yes
pack .middle.mini_text -side left -fill both -expand no

# Create a frame at the bottom that contains a labelled entry where
# the user will type their idea.
frame .bottom
label .bottom.label \
    -text " Enter Idea:" 
entry .bottom.idea_entry \
    -textvariable myIdea \
    -relief sunken 
focus .bottom.idea_entry

pack .bottom -side bottom -fill x
pack .bottom.label -side left 
pack .bottom.idea_entry -side left -expand yes -fill x

# After a carriage return, the idea is entered into the shared list
bind .bottom.idea_entry <Return> "insert_idea_into_shared_list"



