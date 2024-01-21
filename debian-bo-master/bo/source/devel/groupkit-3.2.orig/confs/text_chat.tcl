# A Multi-user "Chat" system
# ================================================================
# Last modified June 2 1995, S.G.

# Help Description to be added to the help menu
#----------------------------------------------
set application(title) \
    "Text Chat"

set application(description) {
{normal} {You can have real-time text conversations with other\
 conference participants. 

Every person has their own window. You will see everything they type\
 in their window, and vice-versa.

}
{normalbold} {Phrases:
}
{normal} {You can select one of the } 
{normalitalic} {phrase buttons}
{normal} { on the bottom,\
 which will enter its text in your window as if you had typed it.\
 You can change these or create new ones by selecting }
{normalitalic} {Edit Phrases }
{normal} {from the }
{normalitalic} {Phrase }
{normal} {menu and changing the entries.

}   
{normalbold} {Flash:
}
{normal} {The }
{normalitalic} {flash button }
{normal} {on the bottom right will flash your chat window. This\
 is useful for drawing attention of others, particularly if the\
 conversation has been idle for a while.

}
{normalbold} {Emacs-like key bindings:
}
{normalitalic} { Control-f:  } {normal} {Forward character
}
{normalitalic} { Control-b:  } {normal} {Backward character
}
{normalitalic} { Control-a:  } {normal} {Beginning of line
}
{normalitalic} { Control-e:  } {normal} {End of line
}
{normalitalic} { Control-p:  } {normal} {Previous line
}
{normalitalic} { Control-n:  } {normal} {Next line
}
{normalitalic} { Control-d:  } {normal} {Delete forward character
}
{normalitalic} { Control-h:  } {normal} {Backspace
}}


# Phrase Globals 
#---------------
# Define what file to save phrases in, the max number of phrases, and  few default phrases.
set phrase(file)  [glob ~]/.textchat.gk
set phrase(number)   7
set phrase(label) "Make your own phrase collection"
set phrase(1,string) "Ok."
set phrase(2,string) "Uh huh."
set phrase(3,string) "Hang on a minute..."
set phrase(4,string) "I'm back!"
set phrase(5,string) ""
set phrase(6,string) ""
set phrase(7,string) ""

# Procedures for creating, destroying and updating  the chat windows
#-------------------------------------------------------------------

# Make a chat window for the specified user
proc makeChatWindow usernum { 
    createChatWindow \
        $usernum [users remote.$usernum.username] \
	disabled
    return ""
}

# Destroy a chat window for the specified user
proc removeChatWindow usernum { 
    catch {destroy .$usernum}
    return ""
}

# Update a new arrival with the contents of all the chat windows
proc updateEntrant usernum { 
    set frames [winfo children .]
    foreach i $frames {
	# Look for the other windows...
	if {$i != ".menubar" && $i != ".button_f" && [string compare $i .$usernum] } {
	    set text [$i.f.text get 0.0 end]
	    gk_toUserNum $usernum \
		updateTextWindow $i $text
	}
    }
}
# Given a window and some text, insert the text in the window
proc updateTextWindow {frame text} {
    if {[winfo exists $frame.f.text]} {
	$frame.f.text configure -state normal
	$frame.f.text insert 0.0 $text
	$frame.f.text configure -state disabled
    } else {
	# We have to delay a bit, because the application is
	# still trying to make the window!
	after 500 "updateTextWindow $frame \"$text\""
    }
}

# Actually create the chat window. The state says whether it
# it should be editable or not by the user.
proc createChatWindow {number name state} {
    set chatframe .$number 
    set label .$number.label
    set textframe .$number.f
    set text  .$number.f.text 
    set scroll  .$number.f.scroll

    frame $chatframe -bd 2
    pack $chatframe -side top -expand yes -fill both

    label $label \
	-text $name \
	-font -Adobe-times-bold-r-normal--*-120*
    pack  $label -side top -anchor w

    frame $textframe 

    text  $text \
	-state $state \
	-relief ridge \
	-bd 2 \
	-width 50 -height 6 \
	-font "-*-lucida-medium-r-*-*-12-*-*-*-*-*-*-*" \
	-wrap word \
	-setgrid true \
	-yscrollcommand "$scroll set"

    scrollbar $scroll \
	-orient vertical \
	-relief ridge \
	-command "$text yview"

    pack  $textframe -side top -expand yes -fill both
    pack  $text -side left -fill both -expand yes
    pack  $scroll -side right -fill y
    bindtags $text "$text Text"
    if {$state != "disabled" } {
	focus $text
	set return "\n"
	bind  $text  <Any-KeyPress> "insertCharacter $number %A; break"
	bind  $text  <Return> "insertCharacter $number \"\n\"; break"
    }
}


# Procedures that take care of inserting the characters
# the user had typed in the proper place and the proper text widget
#-------------------------------------------------------------------

proc insertCharacter {number char} {
    set position [.$number.f.text index insert]
    gk_toAll reallyInsertCharacter $number $char $position
}

# We over-ride all key bindings. Thus we handle all 
# character interpretation ourselves. 
proc reallyInsertCharacter {number char position} {
    set w .$number.f.text

    # If its disabled, make it normal so we can maniplate it
    if {$number != [users local.usernum]} {
	$w configure -state normal
    }
    case $char in {
	{\012 \015} {	    
	    # Returns, Line feeds
	    $w insert $position "\n"
	    $w yview -pickplace insert
	} 
	{\010 \177} {
	    # Backspace
            $w mark set insert $position
	    if [$w compare insert != 1.0] {
		$w delete insert-1c
		$w yview -pickplace insert
	    }
	}
	{\004} {
	    # Forward delete character
	    set old [$w index insert]
            $w mark set insert $position+1c
	    if {$old !=  [$w index insert]} {
		if [$w compare insert != 1.0] {
		    $w delete insert-1c
		    $w yview -pickplace insert
		}
	    }
	}
	{\006} {
	    # Forward Character (Control-f)
	    $w mark set insert $position+1c
	    $w yview -pickplace insert
	}
	{\002} {       
	    # Backward Character (Control-b)
	    $w mark set insert $position-1c
	    $w yview -pickplace insert
	}
	{\001} {
	    # Beginning of line (Control-e)            
	    $w mark set insert "insert linestart"
	    $w yview -pickplace insert
	}
	{\005} {
	    # End of line (Control-e)            
	    $w mark set insert "insert lineend"
	    $w yview -pickplace insert
	}
	{\016} {            
	    # Previous line (Control-p)
	    $w mark set insert "insert +1l"
	    $w yview -pickplace insert
	}
	{\020} {            
	    # Next line (Control-n)
	    $w mark set insert "insert-1l"
	    $w yview -pickplace insert
	}
	default {
	    # Insert printing characters only
	    if {$char >= " " && $char <= "~"} {
		$w insert $position $char
		$w yview -pickplace $position
	    }
	}
    }
    # All windows but the user's should be disabled
    if {$number != [users local.usernum]} {
	$w configure -state disabled
    }
}


# Procedures for handling phrases and their buttons, 
# and for creating the phrase editor
#--------------------------------------------------------

# Load the default Phrases from a file, if any
proc initializePhrases {} {
    global phrase 
    if {[file exists $phrase(file)]} {
	source $phrase(file)
    }
}

# Save the Phrases in the phrase file
proc writePhrases {} {
    global phrase 
    set fd [open $phrase(file) w]
    for {set i 1} {$i <= $phrase(number)} {incr i} {
        set string $phrase($i,string)
	puts $fd \
		"set phrase($i,string) \"$string\""
    }   
    close $fd
} 

# Create the phrase editor window
proc createPhraseEditor {} {
    global phrase

    # Create the top level window for the phrase editor
    set w .phrases
    if {[winfo exists $w]} return
    toplevel $w
    wm title $w "Phrase Editor"

    # The phrases
    set f .phrases.f
    frame $f -relief ridge -bd 2
    pack $f
    label $f.l -text $phrase(label)
    pack $f.l 
    for {set i 1} {$i <= $phrase(number)} {incr i} {
	frame $f.f$i
	pack  $f.f$i -side top
	entry $f.f$i.e$i -relief sunken
	$f.f$i.e$i insert end $phrase($i,string)
	pack $f.f$i.e$i -side left
    }

    # Buttons
    set fb .phrases.fb
    frame $fb 
    pack $fb -side bottom -expand t -fill x 
    button $fb.ok -text Ok -command "savePhrases $f; installPhrases; destroy $w"
    button $fb.apply -text Apply -command "savePhrases $f; installPhrases "
    button $fb.cancel -text Cancel -command "destroy $w"
    pack $fb.apply $fb.ok -side left
    pack $fb.cancel -side right


}

# Get the phrases from the phrase editor and save them in the phrase data structure
proc savePhrases {f} {
    global phrase
    for {set i 1} {$i <= $phrase(number)} {incr i} {
	set phrase($i,string)  [$f.f$i.e$i get]
    }   
}

# Add the phrases to the buttons
proc installPhrases {} {
    global phrase
    createButtons
}

# Create the buttons on the bottom of the window that allow us to insert some 
# standard phrases into the chat winow.
proc createButtons {} {
    global phrase
    if { [winfo exists .button_f]}  {
	destroy .button_f
    }
    frame .button_f
    pack .button_f -side bottom
    for {set i 1} {$i <= $phrase(number)} {incr i} {
	if {$phrase($i,string) != ""} {
	    createTextButton .button_f $i $phrase($i,string)
	}
    }
    button .button_f.flash -text Flash -command "flashWindow"
    pack .button_f.flash -side right
}

# Actually create a phrase button
proc createTextButton {w name string} {
    button $w.$name -text $string -command "insertPhraseIntoWindow [list $string]"
    pack $w.$name -side left
}

# Given a phrase, insert it at the end of the local chat window 
proc insertPhraseIntoWindow {string} {
    set number [users local.usernum] 
    gk_toAll reallyInsertCharacter $number $string\n end
}

# Procedures that flash the user's window several times to attract attention.
# ----------------------------------------------------------
proc flashWindow {} {
    set w .[users local.usernum].f.text 
    gk_toAll reallyFlashWindow $w
}

proc reallyFlashWindow {w} {
    set bg [lindex [$w configure -bg] 3]
    set fg [lindex [$w configure -fg] 3]
    doFlash $w $fg $bg
    after 100 doFlash $w $bg $fg
    after 200 doFlash $w $fg $bg
    after 300 doFlash $w $bg $fg 
    after 400 doFlash $w $fg $bg
    after 500 doFlash $w $bg $fg 
    after 600 doFlash $w $fg $bg
    after 700 doFlash $w $bg $fg 
}

proc doFlash {w bg fg} {
    $w configure -bg $bg
    $w configure -fg $fg
}

# The Main Program
#=================

# Initialize conference
gk_initConf $argv

# Set the title and the default window size
wm title . $application(title)
wm minsize . 50 6

# Create the default groupkit pulldown menu bar, adding both 
# help, and file menu options for editing and saving text phrases
gk_defaultMenu .menubar
.menubar itemcommand 2 add command -label "$application(title)" \
	           -command "gk_topicWindow .helpWindow \
                   -title \"$application(title)\" \
                   -text \"$application(description)\""

.menubar itemcommand 0 insert 1 command \
	-label "Edit Phrases" \
	-command createPhraseEditor
.menubar itemcommand 0 insert 1 command \
	-label "Save Phrases" \
	-command "writePhrases" 
pack .menubar -side top -fill x 


# And lets not forget to make my own chat window and phrases!
set me [users local.username]
createChatWindow [users local.usernum] "I'm $me" normal

initializePhrases 
createButtons

# Event handlers
#---------------
# A new person has arrived. Create a chat window for them
gk_bind newUserArrived "makeChatWindow %U"

# A person has left. Remove their chat window 
gk_bind userDeleted "removeChatWindow %U"

# A person has joined. It is our responsibility to update them
gk_bind updateEntrant "updateEntrant %U"


