# A Postit note application that lets you place postit notes on the
# screen of selected participants
# ================================================================

# Help Description to be added to the help menu
#----------------------------------------------
set postit(help_title) \
    "About Postit"

set postit(description) {
{normal} { Create a "Postit" note and place it on the screens of\
 selected participants.

The steps are:
-clear the Postit note with the Clear Note button
-type your message into the note
-select the participants you want to send the note to
-send the note by pressing the PostIt button.

Postit notes are labelled with the name of the person who wrote it. \
 If you send a Postit note to yourself, it will be labelled as a \
 "Reminder".}
}


# Globals
#--------------------

# Widget paths for the items in the postit note editor
set postit(editor_frame) .editf
set postit(editor) .editf.editor
set postit(control_frame) .control_frame
set postit(postit_button) .control_frame.postit_button
set postit(clear_button)  .control_frame.clear_button
set postit(participant_frame) .participant_frame

# Fonts and colors
set postit(participant_font) -Adobe-times-medium-r-normal--*-140*
set postit(helvetica_normal) *helvetica*-r-*14*
set postit(helvetica_italic) *helvetica*-o-*14* 
set postit(helvetica_small) *helvetica*-r-*10* 
set postit(color) LightGoldenrod1
set postit(color2) LightGoldenrod2
set postit(background) ""

# Text messages
set postit(editor_message) "Type your message here.\
 Then select the people you want to send the message to,\
 and press the PostIt button."

# Window titles
set postit(sender_title) "PostIt Editor"
set postit(postit_title) "PostIt Note"


# PROCEDURES
#-------------------------

# This window creates the Postit note creator. It contains:
#  -an editor for the postit note text
#  -a selectable list of people that you can send Postit notes to.
#  -some controls
proc createPostitSender {} {global postit
    # Decorate the window
    wm title . $postit(sender_title)
    wm iconname . $postit(sender_title)

    # Create the postit editor in a frame
    frame $postit(editor_frame) -relief ridge -border 3
    pack $postit(editor_frame) -side top
    text $postit(editor) \
	-wrap word \
	-width 30 -height 10 \
	-font *helvetica*-r-*12*
    $postit(editor) insert end $postit(editor_message)
    pack $postit(editor) -side top 

    #Create a frame containing the selectable list of participants
    frame $postit(participant_frame) -relief ridge -border 3
    pack $postit(participant_frame) -side top -fill both

    # Create a set of controls (buttons) in a frame
    frame $postit(control_frame)

    pack $postit(control_frame) -side top -fill x
    button $postit(postit_button) -text "PostIt!" \
	-state disabled \
        -command "global postit; \
         postit \"[users local.username]\" \[$postit(editor) get 0.0 end\] "
    pack $postit(postit_button) -side right

    button $postit(clear_button) \
	-text "Clear Note" -command "$postit(editor) delete 1.0 end; normalButton"
    pack $postit(clear_button)  -side left
}

# Create the initial selectable list of participants that will be 
# displayed in the Postit editor
proc createParticipantList {} {
    # Show yourself first
    set unum [users local.usernum]
    showParticipant Yourself $unum 

    # Now show the other participants
    foreach person [users keys remote] {
	puts $person
	set unum [users remote.$person.usernum]
	set uname [users remote.$person.username]
	showParticipant $uname $unum 
    }
}

# Create a label that contains the participant's name and add it to the window
proc showParticipant {user_name user_number} {
    global postit
    
    checkbutton $postit(participant_frame).$user_number \
	-text $user_name \
	-font $postit(participant_font) \
	-border 2 -relief ridge \
	-anchor w \
	-variable participant($user_number) \
        -command {postitButtonState}
    
    pack $postit(participant_frame).$user_number -side top -fill x
}

# Only enable the Postit button if at least one participant is selected
proc postitButtonState {} {global participant postit
    set state disabled
    foreach usernum [array names participant] {
	if {$participant($usernum)} {
	    set state normal
	    break
	}
    }
    $postit(postit_button) configure -state $state

}

# Tell the selected participants to post the postit note
# Also select all the text in the postit editor
proc postit {from msg} {
    global participant postit

    foreach usernum [array names participant] {
	if {$participant($usernum)} {
	    yellowButton $postit(participant_frame).$usernum
	    if {$usernum == [users local.usernum]} {
		newPostitNote "Reminder" $msg 
	    } else {
		gk_toUserNum $usernum newPostitNote [concat "From: " $from] $msg
	    }
	}
    }
    $postit(editor) tag add sel 1.0 end
}

# Turn the buttons yellow, to indicate the message has been sent to a person
proc yellowButton {button} {global postit
    $button configure -background $postit(color)
    $button configure -activebackground $postit(color2)
}

# Turn the buttons to their default color 
proc normalButton {} {global participant postit
    foreach usernum [array names participant] {
	$postit(participant_frame).$usernum configure -background \#ffe4c4
	$postit(participant_frame).$usernum configure -activebackground \#e6ceb1
    }
}

# Generates unique id numbers
proc getUniqueNumber {} { 
    global blat; 
    if {![info exists blat]} {set blat 0}
    incr blat; 
    return $blat 
}

# Remote Procedure Calls
# ------------------------

# Create a new Postit note
proc newPostitNote {from msg} {
    global postit

    # Create a new top-level window
    set window .w[getUniqueNumber]
    toplevel $window
    wm title $window $postit(postit_title)
    wm iconname $window $postit(postit_title)
    wm protocol $window WM_DELETE_WINDOW "destroy $window"
    frame $window.f -bg $postit(color)
    pack $window.f -side top 
    message $window.f.msg -text $msg \
        -font $postit(helvetica_normal) \
        -aspect 300 \
	-bg $postit(color)

    label $window.f.lbl -text $from \
        -font $postit(helvetica_italic) \
	-bg $postit(color)

    label $window.f.date -text "([gk_info date])" \
        -font $postit(helvetica_small) \
	-bg $postit(color)

    pack $window.f.lbl -side top 
    pack $window.f.date -side top 
    pack $window.f.msg -side top 

    button $window.f.button \
	-text "Dismiss" \
	-command "destroy $window" \
	-bg $postit(color2)
    pack $window.f.button -fill x
}



# Callbacks - triggered when participants enter/leave the conference
#------------------------------------------------------------------

# A participant has left. Delete them from the list of participants.
proc removeUserFromWindow unum { 
    global postit
    catch {pack unpack $postit(participant_frame).$unum}
    return ""
}

# A participant has entered. Add them to the list of participants.
proc addUserToWindow unum { 
    set uname [users remote.$unum.username]
    showParticipant $uname $unum 	
    return ""
}


# Main
# =========

# Initialize conference, add a menu bar and help.
gk_initConf $argv
gk_defaultMenu .menubar
.menubar itemcommand help add command -label $postit(help_title) \
                               -command "gk_topicWindow .helpWindow \
                                    -title {$postit(help_title)} \
                                    -text {$postit(description)}"
pack .menubar -side top -fill x


# Create the Postit Sender editor
createPostitSender
createParticipantList

# Track when participants enter and leave the conference
gk_bind newUserArrived "addUserToWindow %U"
gk_bind userDeleted "removeUserFromWindow %U"

# Bindings 
# Whenever the text message is altered, revert participant checkbuttons to normal
bind $postit(editor) <KeyPress> "normalButton"

