gk_initConf $argv
pack [gk_defaultMenu .menubar] -fill x

.menubar itemcommand 2 add command -label "About Ladder" -command dohelp

proc dohelp {} {
	gk_topicWindow .help -height 15 -width 53 -title "About Ladder" \
			-text \
{{normal} {This program was created by a group of six
high school students attending the Shad Valley program
at the U of Calgary over the summer of 1995.  They were
shown a number of groupware systems and were asked to
explore the question of how this technology could be 
applied to education.  This program was part of their
exploration.

The program simulates the situation in a calculus problem
they had been assigned earlier, where if a ladder is being
pulled along the ground at a certain velocity, they had to 
calculate the velocity the other end is moving down the wall.
The students constructed this simulator as a conversational
prop for discussing the problem.

The program took them only about five hours to design and
implement, using only other GroupKit programs as a basis,
and with only a couple of the students having any programming
background at all. 

No guarantees they got the math right however. :-)
}   }
}

proc createAxesAndLabels {} {
	.c create line 30 30 30 230
	.c create line 30 230 230 230
	.c create text 30 20 -text "Y"
	.c create text 20 40 -text "20"
	.c create text 240 230 -text "X"
	.c create text 220 240 -text "20"
	.c create text 20 240 -text "0"
	.c create text 200 50 -text "Horizontal Position (m)" -anchor w
	.c create text 200 100 -text "Vertical Position (m)" -anchor w
	.c create text 200 150 -text "Velocity of Top (m/s)" -anchor w
}

proc createScreen {} {
	pack [label .l -text "Ladder Problem"] -fill x
	pack [canvas .c -width 400 -height 300] -fill both -expand yes
	createAxesAndLabels
	
	# start out with some default values
	set x 12; set y 16
	
	# create the line that gets moved around
	.c create line 30 [expr 230-10*$y] [expr 30+10*$x] 230 \
			-tags theline
	
	# create the two handles we use to drag the line around
	.c create rectangle 26 [expr 230-10*$y-4] 34 [expr 230-10*$y+4] \
			-fill black -tags handle1
	.c create rectangle [expr 10*$x+30-4] 234 [expr 10*$x+30+4] 226 \
		-fill black -tags handle2
	
	# make it so when we drag the handles, the procedures below get called    
	.c bind handle1 <B1-Motion> "gk_toAll handle1dragged %y"
	.c bind handle2 <B1-Motion> "gk_toAll handle2dragged %x"
	
	# create fields to display the x and y values
	.c create text 200 70 -text "12" -anchor w -tags xval
	.c create text 200 120 -text "16" -anchor w -tags yval
	.c create text 200 170 -text "[expr 3.0/2]" -anchor w -tags vval

	# add telepointers
	gk_initializeTelepointers
	gk_specializeWidgetTreeTelepointer .c
}

# we've moved the handle along the left axis (i.e. we're changing
# the y value)

proc handle1dragged {y} {
	set y $y.0
	if {$y < 30} {set y 30}
	if {$y > 230} {set y 230} 
	set x [expr 30+10*sqrt(400-(230-$y)*(230-$y)/100)]
	.c coords theline 30 $y $x 230
	.c coords handle1 26 [expr $y-4] 34 [expr $y+4]
	.c coords handle2 [expr $x-4] 234 [expr $x+4] 226
	.c itemconfig xval -text "[expr ($x-30)/10]"
	.c itemconfig yval -text "[expr (230-$y)/10]"
	.c itemconfig vval -text "[expr (2*$x/$y)]"
}

# we've moved the handle along the bottom axis (i.e. we're changing
# the x value)
#
proc handle2dragged {x} {
	set x $x.0
	if {$x < 30} {set x 30}
	if {$x > 230} {set x 230} 
	set y [expr 230-10*sqrt(400-($x-30)*($x-30)/100)]
	.c coords theline 30 $y $x 230
	.c coords handle1 26 [expr $y-4] 34 [expr $y+4]
	.c coords handle2 [expr $x-4] 234 [expr $x+4] 226
	.c itemconfig xval -text "[expr ($x-30)/10]"
	.c itemconfig yval -text "[expr (230-$y)/10]"
	.c itemconfig vval -text "[expr (2*$x/$y)]"
}

# start it all off
#
createScreen




