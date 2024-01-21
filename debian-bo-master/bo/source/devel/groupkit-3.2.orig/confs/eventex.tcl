# An Example Program Showing How Events Work
# =========================================
# Last modified June 2 1995, S.G.

# Help Description to be added to the help menu
#----------------------------------------------
set help_title "Event Example"

set help_text  {
 {normal} {This tutorial application shows how groupkit events such as the }
 {normalitalic} {newUserArrived, userDeleted, and updateEntrant }
 {normal} {events can be noticed and displayed by the application.
 
It also shows how a custom event can be created by the application\
 developer. In this case, if you  press the button on the bottom you\
 will post a custom event which says hello to all participants.

Events are presented as an information stack, actually just a listbox\
 with information inserted at the top. You can scroll it with the middle\
 mouse button.}}

# Callbacks
#===========

# Get the user's name from their number (this shows off Environments) and
# display the name, number, and the event message in the message stack
proc showMessage {number note} {
    if {$number == [users local.usernum]} {
	set name [users local.username]
    } else {
	set name [users remote.$number.username]
    }
    .message_stack insert 0 "($number) $name  $note"
}

# Main
#=====
gk_initConf $argv

# Construct the application window
wm minsize . 60 10
gk_defaultMenu .menubar
.menubar itemcommand 2 add command \
       -label "$help_title" \
       -command "gk_topicWindow .helpWindow \
              -title \"$help_title\" \
	      -text  \"$help_text\""

# Construct a "Message Stack" that displays the events
listbox .message_stack -width 40 -height 15 -bd 2 -relief ridge
.message_stack insert 0 "Bottom of Message Stack"

# Construct a button that lets us post an application-specific event
button .postbutton \
	-text "Post a Hello event" \
	-command {gk_toAll generateHelloEvent [users local.usernum]}
pack .menubar .message_stack .postbutton -side top -fill x

#Create our own application-specific event containing our number and a message
proc generateHelloEvent usernum {
    gk_notify hi_there [list [list U $usernum] [list M "says hello"]]
}

# Handle All Events
gk_bind newUserArrived {showMessage %U "arrived"}
gk_bind userDeleted {showMessage %U "left"}
gk_bind updateEntrant {showMessage %U "wants updating"}
gk_bind hi_there {showMessage %U %M}

