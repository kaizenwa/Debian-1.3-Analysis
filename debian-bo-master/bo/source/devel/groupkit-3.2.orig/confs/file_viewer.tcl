# A Simple File Viewing Tool
# ==========================
# Last modified June 3 1995, S.G.

# Help Description to be added to the help menu
#----------------------------------------------
set application(title) "File Viewer"

set application(description) {
 {normal} {When a file is opened from the File menu,\
 it will be displayed in a shared view.

Participants can independently scroll around the view with\
 the multi-user scroll bar. 

They can also independently turn the line wrap on\
 or off through the file menu. 
}}

# Procedures
# ----------

# Pass the contents of the text widget to the user that just arrived
proc updateEntrant entrantUserNum {
    gk_toUserNum $entrantUserNum \
	insertText [.middle.text get 1.0 end]
}

# Get contents of a file and display it in ALL the conference processes.
proc Open {} {
    set fileName [FSBox]
    if {$fileName == ""} {return}
    set fd [open $fileName r]
    gk_toAll insertText [read $fd]
    close $fd
}    

# Replace the text in the text widget with the text provided
proc insertText {text} {
    .middle.text configure -state normal
    .middle.text delete 1.0 end
    .middle.text insert 1.0 $text
    .middle.text configure -state disabled
}

# Turn line wrap on and off
proc lineWrap {} {  global wrap_state
    if {! [info exists wrap_state]} {set wrap_state word}
    if {$wrap_state == "word"} {set wrap_state "none"} else {set wrap_state "word"}
    .middle.text configure -wrap $wrap_state
}

# Main
# ====

gk_initConf $argv
wm title . $application(title)
wm minsize . 50 4

# Create the default groupkit pulldown menu bar.
# Add Open and Wrap Line to the File menu, and additional help to the Help menu
gk_defaultMenu .menubar
.menubar itemcommand 0 insert 1 command -label "Open" -command "Open"
.menubar itemcommand 0 insert 2 separator
.menubar itemcommand 0 insert 3 radiobutton -label "Wrap Line" \
	-command {lineWrap}
.menubar itemcommand 0 insert 4 separator

.menubar itemcommand help add command \
	-label "$application(title)" \
        -command "gk_topicWindow .helpWindow \
	            -title \"$application(title)\" \
                    -text \"$application(description)\""
pack .menubar -side top -fill x 

# Create a scrollable text widget that contains the shared file
# The scrollbar is actually the Groupkit multiuser scrollbar!
frame .middle -bd 2 -relief raised
text .middle.text \
    -setgrid true \
    -font -*-*courier*-medium-r-normal--*-120* \
    -yscrollcommand ".middle.vscroll set" 
insertText "To view a file, open it from the file menu"

gk_scrollbar .middle.vscroll -command ".middle.text yview"
pack .middle -side top  -expand yes -fill both
pack .middle.vscroll -side right -fill y
pack .middle.text -side left -expand yes -fill both

# When a new participant enters, update them
gk_bind updateEntrant "updateEntrant %U"

# add telepointers
gk_initializeTelepointers
gk_specializeWidgetTreeTelepointer .middle.text
