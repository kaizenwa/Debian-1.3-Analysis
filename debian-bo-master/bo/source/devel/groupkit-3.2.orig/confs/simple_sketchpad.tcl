# A Simple Groupware Sketchpad
# ============================
# Last modified June 2 1995, S.G.

# This is a simple generic whiteboard like application, allowing the user 
# to do freehand drawing (Button 1) and erasing (Button 3). 
# The drawings are actually lines on a canvas widget. Telepointers are displayed

# Help Description to be added to the help menu
#----------------------------------------------
set help_title "About Simple Sketchpad"

set help_text  {
  {normal} {This is a very simple groupware sketchpad. 

} {normalbold} {Press } {normal} {and } 
  {normalbold} {drag } {normal} {the }
  {normalitalic} {left mouse button }
  {normal} {to draw on the displays of all participants.

} {normalbold} {Select }  {normal} {the }
  {normalitalic} {Clear }
  {normal} {option on the file menu to clear the displays of all participants.} 
}

# Procedures
#-----------
# Draw a line in the canvases of all participants
proc draw {thisX thisY} {
    global lastX lastY 
    set x1 $lastX
    set y1 $lastY
    set lastX $thisX
    set lastY $thisY
    gk_toAll doDraw $x1 $y1 $thisX $thisY
}

proc doDraw {lastX lastY thisX thisY} {
  .c create line $lastX $lastY $thisX $thisY -width 2 -tag line -fill Blue
   update idletasks
}

# Clear the contents of the canvas
proc clearCanvas {} {
   .c delete line
}

# Send the drawing to the entering user by sending them each line in the canvas
proc sendDrawing {canvas usernum} { 
    foreach line [$canvas find all] {
	set x1 [lindex [$canvas coords $line] 0]
	set y1 [lindex [$canvas coords $line] 1]
	set x2 [lindex [$canvas coords $line] 2]
	set y2 [lindex [$canvas coords $line] 3]
	gk_toUserNum $usernum \
	    doDraw $x1 $y1 $x2 $y2 
    }
}    

# Main
#=====

# Initialize the conference
#--------------------------
gk_initConf $argv

# Create the default groupkit menu bar, and add the help to it
gk_defaultMenu .menu
.menu itemcommand 2 add command \
     -label "$help_title" \
     -command "gk_topicWindow .helpWindow \
          -title \"$help_title\" \
          -text \"$help_text\""

.menu itemcommand 0 insert 1 command -label "Clear Canvas" \
	-command "gk_toAll clearCanvas"
.menu itemcommand 0 insert 2 separator

pack .menu -side top -fill x

# Create a canvas containing the drawing
canvas .c -relief ridge -bd 2
pack .c -side top -expand y -fill both

# Create a label with some instructions to the user
label .l -relief ridge \
    -text "Draw with the left mouse button, clear the display from the File Menu." 
pack .l -side top

# Add a callback so new conference entrants can be updated
gk_bind updateEntrant "sendDrawing .c %U"

# Set up bindings. The B1 callbacks will draw a line when the 
# mouse is moved with the left button down;B3 callback clears the canvas
bind .c <1> "global lastX lastY; set lastX %x; set lastY %y"
bind .c <B1-Motion> {draw  %x %y}

# Attach the GroupKit telepointers into the canvas
gk_initializeTelepointers
gk_specializeWidgetTreeTelepointer .c 

