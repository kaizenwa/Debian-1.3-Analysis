# An Anonymous Brainstorming Tool
# ================================================================
# Last modified June 2 1995, S.G.

# Initialize conference
gk_initConf $argv

# Help Description to be added to the help menu
#----------------------------------------------
set application(title) "Brainstormer"

set application(description) {
{normal} { Type an idea into the ideas area and hit return. 

The idea will display in the shared ideas list, which is scrolled\
 by the multi-user scroll bar, or by pressing the }
{normalitalic} {middle mouse button.

}
{normal} {The }
{normalitalic} {File }
{normal} {menu will let you save ideas to a file, open an existing file, or\
 clear the ideas from the display}
}

# Callbacks
#----------

# Tell all instances of this application to insert the idea (if it 
# isn't an empty string) into the shared list.
proc addIdea {} { global myIdea
    if {$myIdea != ""} {
        gk_toAll doAddIdea $myIdea
        set myIdea ""
    }
}

# Actually insert the idea into the shared list
proc doAddIdea {idea} {
.shared_list insert end $idea
.shared_list see end	
}


# Events and Event Callbacks
#---------------------------

# Update the entering participant with the state of the conference 
# The event "updateEntrant" is generated on only ONE of the 
# existing users when a new user joins the conference.
# sendAllIdeas puts all the existing ideas into the new user's listbox.

gk_bind updateEntrant "sendAllIdeas %U"

proc sendAllIdeas entrantUsernum { 
    for {set count 0} {$count < [.shared_list size]} {incr count 1} {
	set idea [.shared_list get $count]
        gk_toUserNum $entrantUsernum \
	    doAddIdea $idea 
    }
}

# Procedures for saving and loading ideas to/from files, and clearing them
#-------------------------------------------------------------------------
# These routines are invoked by the commands in the "File" menu.

# Saves contents of listbox in file.  The FSBox routine asks the user which
# file to save into, the file is opened and the listbox elements are put in.
# If the file already exists, the data is appendend onto it.
proc Save {} { 
    if {[.shared_list size] == 0} {return}
    set fileName [FSBox]
    if {$fileName == ""} {return}
    set fd [open $fileName a]
    for {set count 0} {$count < [.shared_list size]} {incr count 1} {
	puts $fd [.shared_list get $count]
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
	gk_toAll doAddIdea $temp
	set result [gets $fd temp]
    }
    close $fd
}    

# Clears ALL the listboxes.
proc Clear {} { 
    gk_toAll doClear 
}

# Clear a listbox.
proc doClear {} { 
     .shared_list delete 0 end
}

# The Main Program
#=================

# Set the title and the default window size
wm title . $application(title)
wm minsize . 20 7

# Create the default groupkit pulldown menu bar 
# Then add Clear, Open, and Save items to the "File" menu, and help. 
gk_defaultMenu .menubar
.menubar itemcommand help add command \
	-label "$application(title)" \
	-command "gk_topicWindow .helpWindow \
	             -title $application(title) \
                     -text \"$application(description)\""

.menubar itemcommand 0 insert 1 command -label "Open" -command "Open"
.menubar itemcommand 0 insert 2 command -label "Save" -command "Save"
.menubar itemcommand 0 insert 3 separator
.menubar itemcommand 0 insert 4 command -label "Clear Ideas" -command "Clear"
.menubar itemcommand 0 insert 5 separator
pack .menubar -side top -fill x 

# Build the Interface
#----------------------
# Use the viewport widget to surrond a listbox with a vertical and
# a horizontal scrollbars.  The listbox will contain the shared
# ideas and the scrollbars are actually the Groupkit multiuser
# scrollbar.  To turn of the multi-user portion just add
# "-multiuser no" to the viewport options.

gk_viewport .middle [listbox .shared_list -height 13 -setgrid true] \
	-scroll {right bottom} 
pack .middle -side top  -expand yes -fill both

# Create a frame at the bottom that contains a labelled entry where
# the user will type their idea.
frame .bottom
label .bottom.label -text " Your Idea:" 
entry .bottom.idea_entry -textvariable myIdea -relief sunken 
focus .bottom.idea_entry

pack .bottom -side bottom -fill x
pack .bottom.label -side left 
pack .bottom.idea_entry -side left -expand yes -fill x

# After a carriage return, the idea is entered into the shared list
bind .bottom.idea_entry <Return> "addIdea"
