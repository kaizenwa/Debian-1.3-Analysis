# Tetrominoes: a mulit-user puzzle. Fit the pieces perfectly into one of
# the shapes shown
# ================================================================
# Last modified June 3 1995, S.G.; Written by Carl Gutwin

# Help Description to be added to the help menu
#----------------------------------------------
set application(title) \
    "About Tetrominoes"

set application(description) {
{normal} {Play with the Tetris shapes! Fit the pieces perfectly into\
 one of the shapes shown, or make your own forms. They do fit, if you\
 are clever enough!

}
{normalbold} {Mouse controls:
}
{normalitalic} { Left button:  } {normal} {Move shape
}
{normalitalic} { Middle button:  } {normal} {Rotate shape
}
{normalitalic} { Right button:  } {normal} {Flip shape
}}

#Globals
#--------
set pent(frame)  	.frame2
set pent(canvas) 	$pent(frame).c
set pent(legend) 	$pent(frame).legend
set pent_color(one)	SeaGreen3
set pent_color(two)	DeepSkyBlue3
set pent_color(three)	bisque3
set pent_color(four)	Red
set pent_color(five)   	black
set pent_color(black) 	black

#Procedures
#----------

proc pieceChanged {piece coords} {
  global pent
  eval [concat $pent(canvas) coords $piece $coords]
  update idletasks
}

proc createPiece {piece coords} {
  global pent_color pent
  eval $pent(canvas) create polygon $coords \
         -fill $pent_color($piece) -tags $piece
}

# Initialize a dragging operation
proc itemStartDrag {c x y} {
    global lastX lastY itemTag

    set lastX [$c canvasx $x]
    set lastY [$c canvasy $y]
    set itemTag [lindex [$c gettags current] 0]
}

# Move a piece on every display
proc itemDrag {c x y} {
    global lastX lastY itemTag
    if {[member form [$c gettags $itemTag]]} {
	return
    }

    set x [$c canvasx $x]
    set y [$c canvasy $y]

    $c move $itemTag [expr $x-$lastX] [expr $y-$lastY]
    pieces $itemTag [$c coords $itemTag]

    set lastX $x
    set lastY $y
}


# Rotate a piece on every display
proc rotateItem {c x y} {

    set itemTag [lindex [$c gettags current] 0]

    if {$itemTag == ""} {
	return
    }

    if {[member form [$c gettags $itemTag]]} {
	return
    }

    set box [$c bbox current]
    set points [$c coords current]
    set xOffset $x
    set yOffset $y

    set newList ""
    for {set i 0} {$i < [llength $points]} {set i [expr $i + 2]} {
	set tempX [expr [lindex $points $i] - $xOffset]
	set tempY [expr [lindex $points [expr $i + 1]] - $yOffset]
	set tempX [expr $tempX * -1]
	set tempX [expr $tempX + $yOffset]
	set tempY [expr $tempY + $xOffset]
	
	set newList "$newList $tempY $tempX"
    }
    set box [$c bbox current]

    pieces $itemTag $newList
}

# Flip a piece on every display
proc flipItem {c x y} {

    set itemTag [lindex [$c gettags current] 0]

    if {$itemTag == ""} {
	return
    }
    if {[member form [$c gettags $itemTag]]} {
	return
    }

    set box [$c bbox current]
    set points [$c coords current]
    set xOffset [expr (([lindex $box 2] - [lindex $box 0]) / 2) \
		     + [lindex $box 0]]
    set yOffset [expr (([lindex $box 3] - [lindex $box 1]) / 2) \
		     + [lindex $box 1]]

    set newList ""
    for {set i 0} {$i < [llength $points]} {set i [expr $i + 2]} {
	set tempX [expr [lindex $points $i] - $xOffset]
	set tempY [expr [lindex $points [expr $i + 1]] - $yOffset]
	set tempX [expr $tempX * -1]
	set tempX [expr $tempX + $xOffset]
	set tempY [expr $tempY + $yOffset]
	
	set newList "$newList $tempX [lindex $points [expr $i + 1]]"
    }
    pieces $itemTag $newList
}

# Main
#-------
# Initialize conference
gk_initConf $argv

# Configure the window
wm geometry . 550x500
wm minsize . 50 50 
wm title . "Tetrominoes"
wm iconname . "Tetrominoes"

# Add the menu bar
gk_defaultMenu .menubar
.menubar itemcommand 2 add command -label "$application(title)" \
                          -command "gk_topicWindow .helpWindow \
                                    -title {$application(title)} \
                                    -text {$application(description)}"
pack .menubar -side top -fill x

# Construct the display and put it together
frame $pent(frame)  -relief raised -bd 2
label $pent(legend) -relief groove -text "Left: move shape  Middle: rotate  Right: flip"
canvas $pent(canvas) -scrollregion {0c 0c 30c 24c} -width 15c -height 10c 

pack $pent(frame)  -side top   -fill both -expand yes
pack $pent(canvas) -expand yes -fill both
pack $pent(legend) -side bottom -expand no -fill x

# If its a monochrame display, make all the pieces black
if {[winfo depth .] < 4} {
    foreach color [array names pent_color] {
	set pent_color($color) black
    }
}

# Create a shared environment that contains our pieces, plus their callbacks
gk_newenv -bind -share pieces
pieces bind changeEnvInfo {pieceChanged %K [pieces %K]}
pieces bind addEnvInfo {createPiece %K [pieces %K]}
pieces bind envReceived {
  foreach i [pieces keys] {
      createPiece $i [pieces $i]
  }   
}

# Now make the pieces 
if {[gk_amOriginator]} {
  pieces one "40 0 160 0 160 40 80 40 80 80 40 80 40 0"
  pieces two "40 80 120 80 120 160 40 160 40 80"
  pieces three "40 200 80 200 80 160 120 160 120 200 160 200 160 240 40 240 40 200"
  pieces four "160 40 200 40 200 200 160 200 160 40"
  pieces five "200 120 240 120 240 80 320 80 320 120 280 120 280 160 200 160 200 120"
}


# Make the outlines that the pieces go into
$pent(canvas) create line 400 40 400 320 520 320 520 40 480 40 480 80 \
    440 80 440 40 400 40 \
    -fill $pent_color(black) -tags form

$pent(canvas) create line 40 280 40 400 320 400 320 280 200 280 200 320 \
    160 320 160 280 40 280 \
    -fill $pent_color(black) -tags form

# Set up event bindings for canvas:
bind $pent(canvas) <1> "itemStartDrag $pent(canvas) %x %y"
bind $pent(canvas) <B1-Motion> "itemDrag $pent(canvas) %x %y"
bind $pent(canvas) <2> "rotateItem $pent(canvas) %x %y"
bind $pent(canvas) <3> "flipItem $pent(canvas) %x %y"

# Now put the GroupKit telepointers, which will only appear on the canvas
gk_initializeTelepointers
gk_specializeWidgetTreeTelepointer $pent(canvas) 

