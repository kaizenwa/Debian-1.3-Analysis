# A Minimalist Anonymous Brainstorming Tool
# =========================================
# Last modified June 2 1995, S.G.

# Help Description to be added to the help menu
#----------------------------------------------
set help_title "Minimalist Brainstormer"

set help_text  {
 {normal} {This is a very simple brainstorm tool.

 Type an idea into the ideas area and hit return. 

 The idea will display in the shared ideas list, which is scrolled\
 by pressing the }
 {normalitalic} {middle mouse button.

}
 {normal} {It is minimalist as the participant cannot save the ideas\
 to a file, does not see old ideas if they join a session in progress,\
 and is missing a scroll bar.}
}


# Callbacks
#----------
# Tell all application instances to insert the idea into the shared list.
proc addIdea {} { global myIdea
    if {$myIdea != ""} {
        gk_toAll doAddIdea $myIdea
        set myIdea ""
    }
}

proc doAddIdea {idea} {
    .shared_list insert end $idea
    .shared_list see end	
}

# The Main Program
#-----------------
# Initialize the conference
gk_initConf $argv

# Create the default groupkit pulldown menu bar, and add help to it.
gk_defaultMenu .menubar
.menubar itemcommand 2 add command \
       -label "$help_title" \
       -command "gk_topicWindow .helpWindow \
                    -title \"$help_title\"  \
		    -text  \"$help_text\""

# Make a listbox to contain the shared ideas
listbox .shared_list -width 40 -height 8 -bd 2 -relief ridge

# Make a labelled entry where the user can type in ideas.
frame .bottom 
label .bottom.label -text "Your Idea:"
entry .bottom.idea_entry -textvariable myIdea -relief sunken 
focus .bottom.idea_entry

# Enter the idea into the shared list after every <return>
bind .bottom.idea_entry <Return> "addIdea"

# Put it all together
pack .bottom.label -side left 
pack .bottom.idea_entry -side left -expand yes -fill x
pack .bottom -side bottom -fill x
pack .menubar -side top -fill x 
pack .shared_list -side left -fill both -expand yes
