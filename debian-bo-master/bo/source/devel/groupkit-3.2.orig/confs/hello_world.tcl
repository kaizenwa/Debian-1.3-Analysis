# Groupkit's version of Hello World!
# =========================================

# Make the window resizable, initialize the conference, and add the Groupkit 
# menu bar
wm minsize . 250 50
gk_initConf $argv
gk_defaultMenu .menubar
pack .menubar -side top -fill x

# Define the help and add it to the menu bar
set help_title "About Hello World"
set help_text  { 
    {normal}  {Press the button to say hello to all conference participants.}
}

.menubar itemcommand 2 add command \
        -label "$help_title" \
        -command "gk_topicWindow .helpWindow \
                -height 8 \
                -width 20 \
                -title \"$help_title\" \
                -text \"$help_text\""

# Text displayed by the buttons; greeting message says who it is from
set standard_message "Hello World"
set greetings_message [concat [users local.username] "says hello!"]

# Groupware callback: Briefly post the greeting, 
# then revert back to the standard message
proc say_hi {new_message} {global standard_message
    .hello configure -text $new_message
    after 1500 {.hello configure -text $standard_message}
}

# Create our "hello world" button and put it all together
button .hello \
    -text $standard_message \
    -command "gk_toAll say_hi [list $greetings_message]"
pack .hello -side top 


